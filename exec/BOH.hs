{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}

module Main ( main ) where

import           Brick
import qualified Brick.AttrMap as A
import           Brick.Focus
import           Brick.Forms
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import           Chainweb.HostAddress (HostAddress, hostAddressToBaseUrl)
import           Chainweb.Utils (textOption, toText)
import           Chainweb.Version
import           Control.Error.Util (hoistMaybe, hush, (!?))
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.Generics.Product.Fields (field)
import           Data.Generics.Product.Positions (position)
import           Data.Generics.Sum.Constructors (_Ctor)
import           Data.Generics.Wrapped (_Unwrapped)
import qualified Graphics.Vty as V
import           Holding
import           Lens.Micro
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Options.Applicative hiding (footer, header, str)
import           RIO hiding (local, on)
import           RIO.Partial (fromJust)
import qualified RIO.Seq as Seq
import qualified RIO.Text as T
import           Servant.Client
import           Text.Printf (printf)

--------------------------------------------------------------------------------
-- CLI Handling

-- | CLI arguments.
data Args = Args ChainwebVersion FilePath Account BaseUrl

pArgs :: Parser Args
pArgs = Args
  <$> pVersion
  <*> strOption (long "keyfile" <> help "Path to key file")
  <*> (Account <$> strOption (long "account" <> help "Account name"))
  <*> pUrl

pVersion :: Parser ChainwebVersion
pVersion = textOption
    (long "version" <> metavar "VERSION" <> value defv
     <> help ("Chainweb Network Version (default: " <> T.unpack (toText defv) <> ")"))
  where
    defv :: ChainwebVersion
    defv = Testnet02

pUrl :: Parser BaseUrl
pUrl = hostAddressToBaseUrl Https <$> host
  where
    host :: Parser HostAddress
    host = textOption (long "node" <> metavar "HOSTNAME:PORT" <> help "Node to send TXs")

-- | The immutable runtime environment.
data Env = Env
  { verOf   :: !ChainwebVersion
  , clenvOf :: !ClientEnv
  , keysOf  :: !Keys
  , accOf   :: !Account }
  deriving stock (Generic)

-- | From some CLI `Args`, form the immutable runtime environment.
-- env :: Args -> IO (Either Text (LogFunc -> Env))
env :: Args -> IO (Either Text Env)
env (Args v fp acc url) = runExceptT $ do
  ks <- keysFromFile fp !? ("Could not decode key file: " <> T.pack fp)
  mn <- lift $ newManager tlsManagerSettings
  pure $ Env v (ClientEnv mn url Nothing) ks acc

call :: Env -> ChainId -> PactCode -> IO (Either ClientError TXResult)
call e cid c = do
  m <- meta (accOf e) cid
  tx <- transaction c (keysOf e) m
  runClientM (local (verOf e) cid tx) (clenvOf e)

--------------------------------------------------------------------------------
-- Brick

type Listo = L.GenericList Name Seq TX

data TX = TX ChainId PactCode (Either ClientError From) deriving stock (Generic)

data Wallet = Wallet
  { txsOf  :: Listo
  , logOf  :: Text
  , focOf  :: FocusRing Name
  , replOf :: Form REPL () Name }
  deriving stock (Generic)

data From = R Receipt | T TXResult deriving (Generic)

data REPL = REPL { rcid :: ChainId, rpc :: PactCode } deriving stock (Generic)

-- | Resource names.
data Name = TXList | ChainField | ReplField
  deriving stock (Eq, Ord, Show, Enum, Bounded)

header :: Widget a
header = vLimit 1 . C.center $ txt " The Bag of Holding "

footer :: Text -> Widget a
footer t = vLimit 1 $ txt (T.take 10 t) <+> C.hCenter legend
  where
    legend = txt "[T]ransaction - Pact [R]EPL - [H]elp - [Q]uit"

-- TODO Make it validate the `ChainId` against known values from the current
-- `ChainwebVersion`.
replForm :: REPL -> Form REPL e Name
replForm = newForm
  [ label "Chain" @@= editField (field @"rcid") ChainField Nothing
    toText (chainIdFromText . T.unlines) (txt . T.unlines) id
  , label "Pact Code" @@= editField (field @"rpc") ReplField Nothing
    (^. _Unwrapped) (code . T.unlines) (txt . T.unlines) id
  ]
  where
    label :: Text -> Widget Name -> Widget Name
    label t w = padBottom (Pad 1) $ (vLimit 1 $ hLimit 15 $ txt t <+> fill ' ') <+> w

main :: IO ()
main = do
  execParser opts >>= env >>= \case
    Left _ -> pure () -- TODO Say something.
    Right e -> void $ defaultMain (app e) initial
  where
    initial :: Wallet
    initial = Wallet
      (L.list TXList mempty 1)
      ""
      (focusRing [minBound ..])
      (replForm . REPL (unsafeChainId 0) . fromJust $ code "(+ 1 1)")

    opts :: ParserInfo Args
    opts = info (pArgs <**> helper)
        (fullDesc <> progDesc "The Bag of Holding: A Chainweb Wallet")

    app :: Env -> App Wallet () Name
    app e = App { appDraw = draw
                , appChooseCursor = focusRingCursor focOf
                , appHandleEvent = event e
                , appStartEvent = pure
                , appAttrMap = const $ A.attrMap V.defAttr attrs }

    attrs :: [(AttrName, V.Attr)]
    attrs = [ -- (L.listAttr,         V.white `on` V.blue)
              (L.listSelectedAttr,   V.blue  `on` V.white)
            , (E.editAttr,           V.white `on` V.black)
            , (E.editFocusedAttr,    V.black `on` V.yellow)
            , (invalidFormInputAttr, V.white `on` V.red)
            , (focusedFormInputAttr, V.black `on` V.yellow) ]

draw :: Wallet -> [Widget Name]
draw w = dispatch <> [ui]
  where
    l :: Listo
    l = txsOf w

    -- repl | dialOf w  = [D.renderDialog dia $ C.center $ txt "Super REPL here"]
    dispatch :: [Widget Name]
    dispatch = case focusGetCurrent $ focOf w of
      Just ReplField -> [repl]
      _              -> []

    repl :: Widget Name
    repl = C.centerLayer
           . vLimit 7
           . hLimitPercent 50
           . B.borderWithLabel (txt "Pact REPL")
           $ renderForm (replOf w) <=> C.hCenter (txt "[Esc] [Enter]")

    ui :: Widget Name
    ui = header <=> (left <+> right) <=> footer (logOf w)

    -- TODO Consider `round` border style.
    left :: Widget Name
    left = B.borderWithLabel (txt " Transaction History ") txs

    txs :: Widget Name
    txs | Seq.null (l ^. L.listElementsL) = C.center $ txt "No transactions yet!"
        | otherwise = L.renderList (const txListItem) True l

    txListItem :: TX -> Widget Name
    txListItem (TX cid pc eef) = hBox
      [ hLimit 1 (txt icon)
      , padLeft (Pad 2) . str $ printf "%02d" (chainIdInt cid :: Int)
      , padLeft (Pad 2) . txt $ prettyCode pc ]
      where
        icon = case eef of
          Left _      -> "☹"
          Right (R _) -> "⌛"
          Right (T t) -> maybe "☹" (const "✓") (t ^? pactValue)

    right :: Widget Name
    right = B.borderWithLabel (txt " Transaction Result ") . C.center $ txt contents
      where
        contents :: Text
        contents = case w ^? from of
          Nothing           -> "Select a Transaction"
          Just (TX _ _ eef) -> case eef of
            Left _      -> "This Transaction had an HTTP failure."
            Right (R r) -> r ^. _Unwrapped . to tencode
            Right (T t) -> tencode $ txr t

event :: Env -> Wallet -> BrickEvent Name () -> EventM Name (Next Wallet)
event e w be = case focusGetCurrent $ focOf w of
  Nothing         -> continue w
  Just TXList     -> mainEvent e w be
  Just ReplField  -> replEvent e w be
  Just ChainField -> replEvent e w be -- TODO avoid repitition

replEvent :: Env -> Wallet -> BrickEvent Name () -> EventM Name (Next Wallet)
replEvent e w ev@(VtyEvent ve) = case ve of
  -- Close Popups --
  V.EvKey V.KEsc [] -> continue (w & field @"focOf" %~ focusSetCurrent TXList)

  -- Submission --
  V.EvKey V.KEnter []
    | not (allFieldsValid $ replOf w) -> continue w
    | otherwise -> do
        let fs = formState $ replOf w
        t <- liftIO . call e (rcid fs) $ rpc fs
        let i = TX (rcid fs) (rpc fs) (T <$> t)
        continue $ w & field @"focOf" %~ focusSetCurrent TXList
                     & field @"txsOf" %~ L.listInsert 0 i

  -- Code Input --
  _ -> handleFormEventL (field @"replOf") w ev >>= continue
replEvent _ w _ = continue w

mainEvent :: Env -> Wallet -> BrickEvent Name e -> EventM Name (Next Wallet)
mainEvent e w (VtyEvent ve) = case ve of
  -- Quit --
  V.EvKey (V.KChar 'q') [] -> halt w

  -- REPL Form --
  V.EvKey (V.KChar 'r') [] -> continue (w & field @"focOf"  %~ focusSetCurrent ReplField)

  -- History Selection --
  V.EvKey V.KEnter [] -> liftIO f >>= continue . fromMaybe w
    where
      f :: IO (Maybe Wallet)
      f = runMaybeT $ do
        TX cid _ eef <- hoistMaybe (w ^? from)
        r <- hoistMaybe (eef ^? _Right . _Ctor @"R")
        t <- MaybeT . fmap (join . hush) $ runClientM (listen (verOf e) cid r) (clenvOf e)
        pure (w & field @"txsOf" %~ (L.listModify (set (position @3) (Right $ T t))))

  -- Keyboard Navigation --
  ev -> do
    l' <- L.handleListEventVi L.handleListEvent ev (txsOf w)
    continue (w & field @"txsOf" .~ l')
mainEvent _ w _ = continue w

from :: SimpleFold Wallet TX
from = field @"txsOf" . to L.listSelectedElement . _Just . _2

-- | A missing primitive from `brick`.
handleFormEventL :: Eq n => Lens' s (Form x e n) -> s -> BrickEvent n e -> EventM n s
handleFormEventL l s ev = do
  f' <- handleFormEvent ev (s ^. l)
  pure (s & l .~ f')
