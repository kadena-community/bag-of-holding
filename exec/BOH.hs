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
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import           Chainweb.HostAddress (HostAddress, hostAddressToBaseUrl)
import           Chainweb.Utils (textOption, toText)
import           Chainweb.Version
import           Control.Error.Util ((!?))
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Generics.Product.Fields (field)
import           Data.Generics.Product.Typed (typed)
import qualified Graphics.Vty as Vty
import           Holding
import           Lens.Micro ((%~), (.~), _Right)
import           Lens.Micro.Extras (preview)
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Options.Applicative hiding (str)
import           RIO hiding (local, on)
import qualified RIO.Seq as Seq
import qualified RIO.Text as T
import           RIO.Time (getCurrentTime)
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
  , accountOf :: !Account
  , loggerOf  :: !LogFunc }
  deriving stock (Generic)

instance HasLogFunc Env where
  logFuncL = typed @LogFunc

-- | From some CLI `Args`, form the immutable runtime environment.
env :: Args -> IO (Either Text (LogFunc -> Env))
env (Args v fp acc url) = runExceptT $ do
  ks <- keysFromFile fp !? ("Could not decode key file: " <> T.pack fp)
  mn <- lift $ newManager tlsManagerSettings
  pure $ Env v (ClientEnv mn url Nothing) ks acc

-- TODO Jail off the Env/Logger usage here. This will clean up `main`.
-- withEnv :: Args -> (Env -> IO a) -> IO a
-- withEnv (Args v fp acc url) = runExceptT $ do
--   ks <- keysFromFile fp !? ("Could not decode key file: " <> T.pack fp)
--   mn <- lift $ newManager tlsManagerSettings
--   pure $ Env v (ClientEnv mn url Nothing) ks acc

-- The rightmost `True` sets "maximum verbosity", and then we opt to turn off
-- individual things. The opposite process doesn't seem to work.
-- logging :: IO LogOptions
-- logging = setLogUseLoc False <$> logOptionsHandle stderr True

-- main :: IO ()
-- main = execParser opts >>= env >>= \case
--   Left _  -> pure ()  -- TODO Say anything.
--   Right e -> logging >>= \los -> withLogFunc los (\lf -> runRIO (e lf) work)
--   where
--     opts :: ParserInfo Args
--     opts = info (pArgs <**> helper)
--         (fullDesc <> progDesc "The Bag of Holding: A Chainweb Wallet")

-- TODO How to pretty-print a `PactValue`?
work :: RIO Env ()
work = do
  logInfo "Opening Wallet."
  fmap balance (asks accountOf) >>= \case
    Nothing -> logWarn "Failed to parse Pact code"
    Just c -> call cid c >>= traceShowIO . (preview (_Right . pactValue))
  logInfo "Wallet closed."
  where
    cid :: ChainId
    cid = unsafeChainId 0

-- | Make a `local` call to the configured Chainweb node.
call :: ChainId -> PactCode -> RIO Env (Either ClientError TXResult)
call cid c = do
  e <- ask
  liftIO $ do
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
  { txs :: !(L.GenericList () Seq Text) }
  deriving stock (Generic)

border :: Widget a -> Widget a
border = withBorderStyle BS.unicode . B.borderWithLabel (str " The Bag of Holding ")

main :: IO ()
main = void $ defaultMain app initial
  where
    initial :: Wallet
    initial = Wallet $ L.list () mempty 1

    app :: App Wallet e ()
    app = App { appDraw = draw
              , appChooseCursor = showFirstCursor
              , appHandleEvent = event
              , appStartEvent = pure
              , appAttrMap = const $ A.attrMap Vty.defAttr attrs
              }

    attrs :: [(AttrName, Vty.Attr)]
    attrs = [ -- (L.listAttr,         Vty.white `on` Vty.blue)
              (L.listSelectedAttr, Vty.blue `on` Vty.white)
            ]

draw :: Wallet -> [Widget ()]
draw (Wallet l) = [ui]
  where
    ui :: Widget ()
    ui = border $ left <+> right

    left :: Widget ()
    left = B.borderWithLabel (str " Transaction History ") txs

    txs :: Widget ()
    txs | Seq.null (l ^. L.listElementsL) = C.center $ str "No transactions yet!"
        | otherwise = L.renderList (\_ c -> C.hCenter . str $ show c) True l

    right :: Widget ()
    right = B.borderWithLabel (str " Transaction Result ")
      $ C.center . str . show $ L.listSelected l

event :: Wallet -> BrickEvent () e -> EventM () (Next Wallet)
event w (VtyEvent e) = case e of
  Vty.EvKey (Vty.KChar 'q') [] -> halt w
  Vty.EvKey Vty.KBS [] -> do
    t <- tshow <$> getCurrentTime
    continue (w & field @"txs" %~ L.listInsert 0 t)
  ev -> do
    l' <- L.handleListEventVi L.handleListEvent ev (w ^. field @"txs")
    continue (w & field @"txs" .~ l')
event w _ = continue w
