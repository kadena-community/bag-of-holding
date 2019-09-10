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

--------------------------------------------------------------------------------
-- Endpoint Calling

call :: Env -> Endpoint -> ChainId -> PactCode -> IO TX
call e ep cid c = do
  m  <- meta (accOf e) cid
  tx <- transaction c (keysOf e) m
  TX cid ep c <$> runClientM (f tx) (clenvOf e)
  where
    f :: Transaction -> ClientM From
    f = case ep of
          Local -> fmap T . local (verOf e) cid
          Send  -> fmap R . send (verOf e) cid

--------------------------------------------------------------------------------
-- Terminal UI

type Listo = L.GenericList Name Seq TX

data TX = TX ChainId Endpoint PactCode (Either ClientError From)
  deriving stock (Generic)

data Wallet = Wallet
  { txsOf  :: Listo
  , logOf  :: Text
  , focOf  :: FocusRing Name
  , replOf :: Form REPL () Name }
  deriving stock (Generic)

data From = R Receipt | T TXResult deriving (Generic)

data Endpoint = Local | Send deriving (Eq)

data REPL = REPL { rcid :: ChainId, re :: Endpoint, rpc :: PactCode }
  deriving stock (Generic)

-- | Resource names.
data Name = TXList | ReplChain | ReplLocal | ReplSend | ReplCode
  deriving stock (Eq, Ord, Show, Enum, Bounded)

header :: Widget a
header = vLimit 1 . C.center $ txt " The Bag of Holding "

footer :: Text -> Widget a
footer t = vLimit 1 $ txt (T.take 10 t) <+> C.hCenter legend
  where
    legend = txt "[T]ransaction - [H]elp - [Q]uit"

-- TODO Make it validate the `ChainId` against known values from the current
-- `ChainwebVersion`.
replForm :: REPL -> Form REPL e Name
replForm = newForm
  [ label "Chain" @@= editField (field @"rcid") ReplChain Nothing
    toText (chainIdFromText . T.unlines) (txt . T.unlines) id
  , label "Endpoint" @@= radioField (field @"re")
    [(Local, ReplLocal, "Local"), (Send, ReplSend, "Send")]
  , label "Pact Code" @@= editField (field @"rpc") ReplCode Nothing
    prettyCode (code . T.unlines) (txt . T.unlines) id
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
      (replForm . REPL (unsafeChainId 0) Local . fromJust $ code "(+ 1 1)")

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

    dispatch :: [Widget Name]
    dispatch = case focusGetCurrent $ focOf w of
      Just ReplCode -> [repl]
      _             -> []

    repl :: Widget Name
    repl = C.centerLayer
           . vLimit 10
           . hLimitPercent 50
           . B.borderWithLabel (txt " Pact Transaction ")
           $ renderForm (replOf w) <=> C.hCenter (txt "[Esc] [Enter]")

    ui :: Widget Name
    ui = header <=> (left <+> right) <=> footer (logOf w)

    -- TODO Consider `round` border style.
    left :: Widget Name
    left = hLimitPercent 50 $ B.borderWithLabel (txt " Transaction History ") txs

    txs :: Widget Name
    txs | Seq.null (l ^. L.listElementsL) = txt "No transactions yet!" <+> fill ' '
        | otherwise = L.renderList (const txListItem) True l

    txListItem :: TX -> Widget Name
    txListItem (TX cid ep pc eef) = vLimit 1 $ hBox
      [ hLimit 1 $ txt icon
      , padLeft (Pad 2) . str $ printf "%02d" (chainIdInt cid :: Int)
      , padLeft (Pad 2) $ txt end
      , padLeft (Pad 2) . txt $ prettyCode pc
      , fill ' ' ]
      where
        icon = case eef of
          Left _      -> "☹"
          Right (R _) -> "?"
          Right (T t) -> maybe "☹" (const "✓") (t ^? pactValue)
        end = case ep of
          Local -> "LOCL"
          Send  -> "SEND"

    right :: Widget Name
    right = B.borderWithLabel (txt " Transaction Result ") $ txt contents <=> fill ' '
      where
        contents :: Text
        contents = case w ^? from of
          Nothing             -> "Select a Transaction"
          Just (TX _ _ _ eef) -> case eef of
            Left _      -> "This Transaction had an HTTP failure."
            Right (R r) -> r ^. _Unwrapped . to tencode
            Right (T t) -> tencode $ txr t

event :: Env -> Wallet -> BrickEvent Name () -> EventM Name (Next Wallet)
event e w be = case focusGetCurrent $ focOf w of
  Nothing     -> continue w
  Just TXList -> mainEvent e w be
  Just _      -> replEvent e w be

replEvent :: Env -> Wallet -> BrickEvent Name () -> EventM Name (Next Wallet)
replEvent e w ev@(VtyEvent ve) = case ve of
  -- Close Popups --
  V.EvKey V.KEsc [] -> continue (w & field @"focOf" %~ focusSetCurrent TXList)

  -- Submission --
  V.EvKey V.KEnter []
    | not (allFieldsValid $ replOf w) -> continue w
    | otherwise -> do
        let fs = formState $ replOf w
        t <- liftIO . call e (re fs) (rcid fs) $ rpc fs
        continue $ w & field @"focOf" %~ focusSetCurrent TXList
                     & field @"txsOf" %~ L.listInsert 0 t

  -- Code Input --
  _ -> handleFormEventL (field @"replOf") w ev >>= continue
replEvent _ w _ = continue w

mainEvent :: Env -> Wallet -> BrickEvent Name e -> EventM Name (Next Wallet)
mainEvent e w (VtyEvent ve) = case ve of
  -- Quit --
  V.EvKey (V.KChar 'q') [] -> halt w

  -- Transaction Form --
  V.EvKey (V.KChar 't') [] -> continue (w & field @"focOf"  %~ focusSetCurrent ReplCode)

  -- History Selection --
  V.EvKey V.KEnter [] -> liftIO f >>= continue . fromMaybe w
    where
      f :: IO (Maybe Wallet)
      f = runMaybeT $ do
        TX cid _ _ eef <- hoistMaybe (w ^? from)
        r <- hoistMaybe (eef ^? _Right . _Ctor @"R")
        t <- MaybeT . fmap (join . hush) $ runClientM (listen (verOf e) cid r) (clenvOf e)
        pure (w & field @"txsOf" %~ (L.listModify (set (position @4) (Right $ T t))))

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
