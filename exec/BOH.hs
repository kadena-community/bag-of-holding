{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Main ( main ) where

import           Chainweb.HostAddress (HostAddress, hostAddressToBaseUrl)
import           Chainweb.Utils (textOption, toText)
import           Chainweb.Version
import           Control.Error.Util ((!?))
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Generics.Product.Typed (typed)
import           Holding
import           Lens.Micro (_Right)
import           Lens.Micro.Extras (preview)
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Options.Applicative
import           RIO hiding (local)
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

-- The rightmost `True` sets "maximum verbosity", and then we opt to turn off
-- individual things. The opposite process doesn't seem to work.
logging :: IO LogOptions
logging = setLogUseLoc False <$> logOptionsHandle stderr True

main :: IO ()
main = execParser opts >>= env >>= \case
  Left _  -> pure ()  -- TODO Say anything.
  Right e -> logging >>= \los -> withLogFunc los (\lf -> runRIO (e lf) work)
  where
    opts :: ParserInfo Args
    opts = info (pArgs <**> helper)
        (fullDesc <> progDesc "The Bag of Holding: A Chainweb Wallet")

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
