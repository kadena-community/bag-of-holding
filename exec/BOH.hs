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
-- import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import           Chainweb.HostAddress (HostAddress, hostAddressToBaseUrl)
import           Chainweb.Utils (textOption, toText)
import           Chainweb.Version
import           Control.Error.Util (hush, (!?))
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Generics.Product.Fields (field)
import           Data.Generics.Product.Typed (typed)
import           Data.Generics.Wrapped (_Unwrapped)
import qualified Graphics.Vty as V
import           Holding
import           Lens.Micro
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Options.Applicative hiding (footer, header, str)
import qualified Pact.Types.Command as P
import           RIO hiding (local, on)
import qualified RIO.Seq as Seq
import qualified RIO.Text as T
import           Servant.Client

---

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

{- BRICK NOTES

Specific widget sizes:

  hLimit 20 $ vLimit 5 $ <SOME-WIDGET>

Borders:

  B.borderWithLabel
  B.vBorder
  B.hBorder
  B.hBorderWithLabel

Cursor:

  neverShowCursor
  showFirstCursor
  showCursorNamed

-}

type Listo = L.GenericList Name Seq (Either ClientError From)

data Wallet = Wallet
  { txsOf  :: Listo
  , currOf :: Maybe TXResult
  , logOf  :: Text
  , focOf  :: FocusRing Name
  , replOf :: Form REPL () Name }
  deriving (Generic)

data From = R Receipt | T TXResult

-- data REPL = REPL !ChainId !PactCode
data REPL = REPL Text deriving stock (Generic)

-- | Resource names.
data Name = TXList | MsgField deriving stock (Eq, Ord, Show, Enum, Bounded)

header :: Widget a
header = vLimit 1 . C.center $ txt " The Bag of Holding "

footer :: Text -> Widget a
footer t = vLimit 1 $ txt (T.take 10 t) <+> C.hCenter legend
  where
    legend = txt "Check [B]alance - [T]ransaction - Pact [R]EPL - [H]elp - [Q]uit"

replForm :: REPL -> Form REPL e Name
replForm = newForm
  [ label "Pact Code" @@= editTextField (typed @Text) MsgField Nothing
  ]
  where
    label :: Text -> Widget Name -> Widget Name
    label t w = padTopBottom 1 $ (vLimit 1 $ hLimit 15 $ txt t <+> fill ' ') <+> w

main :: IO ()
main = do
  execParser opts >>= env >>= \case
    Left _ -> pure () -- TODO Say something.
    Right e -> void $ defaultMain (app e) initial
  where
    initial :: Wallet
    initial = Wallet
      (L.list TXList mempty 1)
      Nothing
      ""
      (focusRing [minBound ..])
      (replForm $ REPL "")

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
draw w = repl <> [ui]
  where
    l :: Listo
    l = txsOf w

    -- repl | dialOf w  = [D.renderDialog dia $ C.center $ txt "Super REPL here"]
    repl = case focusGetCurrent $ focOf w of
      Just MsgField ->
        [ C.centerLayer
          . vLimit 6
          . hLimitPercent 50
          . B.borderWithLabel (txt "Pact REPL")
          $ renderForm (replOf w) <=> C.hCenter (txt "[Enter]")
        ]
      _ -> []

    -- dia :: D.Dialog Choice
    -- dia = D.dialog (Just "Pact REPL") (Just (0, [("No", No), ("Yes", Yes)])) 40

    ui :: Widget Name
    ui = header <=> (left <+> right) <=> footer (logOf w)

    -- TODO Consider `round` border style.
    left :: Widget Name
    left = B.borderWithLabel (txt " Transaction History ") txs

    txs :: Widget Name
    txs | Seq.null (l ^. L.listElementsL) = C.center $ txt "No transactions yet!"
        | otherwise = L.renderList (const f) True l

    f :: Either ClientError From -> Widget Name
    f (Left _)      = hLimit 1 (txt "☹") <+> C.hCenter (txt "HTTP Failure")
    f (Right (R r)) = hLimit 1 (txt "⌛") <+> C.hCenter (txt rkr)
      where
        rkr = r ^. _Unwrapped . to P.requestKeyToB16Text
    f (Right (T t)) = hLimit 1 (txt sym) <+> C.hCenter (txt rkt)
      where
        sym = maybe "☹" (const "✓") (t ^? pactValue)
        rkt = t ^. _Unwrapped . P.crReqKey . to P.requestKeyToB16Text

    right :: Widget Name
    right = B.borderWithLabel (txt " Transaction Result ") . C.center $ txt foo
      where
        foo = maybe "Select a Transaction" (tencode . txr) $ currOf w

event :: Env -> Wallet -> BrickEvent Name () -> EventM Name (Next Wallet)
event e w be = case focusGetCurrent $ focOf w of
  Nothing       -> continue w
  Just TXList   -> mainEvent e w be
  Just MsgField -> replEvent w be

replEvent :: Wallet -> BrickEvent Name () -> EventM Name (Next Wallet)
replEvent w ev@(VtyEvent ve) = case ve of
  -- Close Popups --
  V.EvKey V.KEsc [] -> continue (w & field @"focOf"  %~ focusSetCurrent TXList)
  -- Code Input --
  _ -> handleFormEventL (field @"replOf") w ev >>= continue
replEvent w _ = continue w

mainEvent :: Env -> Wallet -> BrickEvent Name e -> EventM Name (Next Wallet)
mainEvent e w (VtyEvent ve) = case ve of
  -- Quit --
  V.EvKey (V.KChar 'q') [] -> halt w

  -- Balance Check --
  V.EvKey (V.KChar 'b') [] -> case balance (accOf e) of
    Nothing -> continue w
    Just c  -> do
      t <- liftIO $ call e cid c
      continue (w & field @"txsOf" %~ L.listInsert 0 (T <$> t))

  -- REPL Form --
  V.EvKey (V.KChar 'r') [] -> continue (w & field @"focOf"  %~ focusSetCurrent MsgField)

  -- TODO This can probably get prettier.
  -- History Selection --
  V.EvKey V.KEnter [] -> maybe (pure w) (liftIO . f) (w ^? from) >>= continue
      where
        f :: From -> IO Wallet
        f (T t) = pure (w & field @"currOf" ?~ t)
        f (R r) = join . hush <$> runClientM (listen (verOf e) cid r) (clenvOf e) >>= \case
          Nothing -> pure w
          Just t  -> pure $ w & field @"currOf" ?~ t
                              & field @"txsOf"  %~ (L.listModify (const . Right $ T t))

  -- Keyboard Navigation --
  ev -> do
    l' <- L.handleListEventVi L.handleListEvent ev (txsOf w)
    continue (w & field @"txsOf" .~ l')
  where
    cid :: ChainId
    cid = unsafeChainId 0  -- TODO Needs to come from elsewhere!
mainEvent _ w _ = continue w


-- TODO Emily halp
-- from :: Traversal' Listo From
from = field @"txsOf" . to L.listSelectedElement . _Just . _2 . _Right

-- | A missing primitive from `brick`.
handleFormEventL :: Eq n => Lens' s (Form x e n) -> s -> BrickEvent n e -> EventM n s
handleFormEventL l s ev = do
  f' <- handleFormEvent ev (s ^. l)
  pure (s & l .~ f')

{-

  _ -> handleFormEvent ev (replOf w) >>= continue . (\f' -> set (field @"replOf") f' w)
    -- form' <- handleFormEvent ev (replOf w)
    -- continue (w & field @"replOf" .~ form')
-}
