{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Main ( main ) where

import           Brick
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import           Chainweb.HostAddress (HostAddress, hostAddressToBaseUrl)
import           Chainweb.Utils (textOption, toText)
import           Chainweb.Version
import           Control.Error.Util ((!?))
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Generics.Product.Fields (field)
import           Data.Generics.Wrapped (_Unwrapped)
import qualified Graphics.Vty as V
import           Holding
import           Lens.Micro ((%~), (.~), (^?), _2, _Just, _Right)
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Options.Applicative hiding (footer, header, str)
import qualified Pact.Types.Command as P
import           RIO hiding (local, on)
import qualified RIO.Seq as Seq
import qualified RIO.Text as T
import           Servant.Client

---

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
  { versionOf :: !ChainwebVersion
  , clenvOf   :: !ClientEnv
  , keysOf    :: !Keys
  , accountOf :: !Account }
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
  m <- meta (accountOf e) cid
  tx <- transaction c (keysOf e) m
  runClientM (local (versionOf e) cid tx) (clenvOf e)

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

data Wallet = Wallet
  { txsOf  :: L.GenericList () Seq (Either ClientError From)
  , currOf :: Maybe TXResult
  }
  deriving (Generic)

data From = R Receipt | T TXResult

header :: Widget a
header = vLimit 1 . C.center $ txt " The Bag of Holding "

footer :: Widget a
footer = vLimit 1 . C.center $
  txt "Check [B]alance - [T]ransaction - Pact [R]EPL - [H]elp - [Q]uit"

main :: IO ()
main = do
  execParser opts >>= env >>= \case
    Left _ -> pure ()
    Right e -> void $ defaultMain (app e) initial
  where
    initial :: Wallet
    initial = Wallet (L.list () mempty 1) Nothing

    opts :: ParserInfo Args
    opts = info (pArgs <**> helper)
        (fullDesc <> progDesc "The Bag of Holding: A Chainweb Wallet")

    app :: Env -> App Wallet e ()
    app e = App { appDraw = draw
                , appChooseCursor = showFirstCursor
                , appHandleEvent = event e
                , appStartEvent = pure
                , appAttrMap = const $ A.attrMap V.defAttr attrs
                }

    attrs :: [(AttrName, V.Attr)]
    attrs = [ -- (L.listAttr,         V.white `on` V.blue)
              (L.listSelectedAttr, V.blue `on` V.white)
            ]

draw :: Wallet -> [Widget ()]
draw w = [ui]
  where
    l :: L.GenericList () Seq (Either ClientError From)
    l = txsOf w

    ui :: Widget ()
    ui = header <=> (left <+> right) <=> footer

    left :: Widget ()
    left = B.borderWithLabel (txt " Transaction History ") txs

    txs :: Widget ()
    txs | Seq.null (l ^. L.listElementsL) = C.center $ txt "No transactions yet!"
        | otherwise = L.renderList f True l

    f :: Bool -> Either ClientError From -> Widget ()
    f _ (Left _)      = hLimit 1 (txt "X") <+> C.hCenter (txt "FAILED")
    f _ (Right (R r)) = hLimit 1 (txt "/") <+> C.hCenter (txt "Receipt")
    f _ (Right (T t)) = hLimit 1 (txt icon) <+> C.hCenter (txt rk)
      where
        icon = maybe "X" (const "O") (t ^? pactValue)
        rk = t ^. _Unwrapped . P.crReqKey . to P.requestKeyToB16Text

    right :: Widget ()
    right = B.borderWithLabel (txt " Transaction Result ") . C.center $ txt foo
      where
        foo = maybe "Select a Transaction" (tencode . txr) $ currOf w

event :: Env -> Wallet -> BrickEvent () e -> EventM () (Next Wallet)
event e w (VtyEvent ve) = case ve of
  -- Quit --
  V.EvKey (V.KChar 'q') [] -> halt w

  -- Balance Check --
  V.EvKey (V.KChar 'b') [] -> case balance (accountOf e) of
    Nothing -> continue w
    Just c  -> do
      t <- liftIO $ call e (unsafeChainId 0) c
      continue (w & field @"txsOf" %~ L.listInsert 0 (T <$> t))

  -- History Selection --
  V.EvKey V.KEnter [] -> case L.listSelectedElement (txsOf w) ^? _Just . _2 . _Right of
    Nothing    -> continue w
    Just (T t) -> continue (w & field @"currOf" .~ Just t)
    Just (R r) -> undefined  -- TODO Do a `listen/` call.

  -- Keyboard Navigation --
  ev -> do
    l' <- L.handleListEventVi L.handleListEvent ev (txsOf w)
    continue (w & field @"txsOf" .~ l')
event _ w _ = continue w
