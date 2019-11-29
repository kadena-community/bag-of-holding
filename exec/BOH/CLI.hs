{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module BOH.CLI
  ( Env(..), env
  , Args(..), pArgs
  ) where

import           BOH.Signing (SignReq, Signed)
import           Brick.BChan (BChan, newBChan)
import           Control.Error.Util (note, (!?))
import           Control.Monad.Trans.Except (runExceptT)
import           Holding
import           Holding.Chainweb
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Options.Applicative hiding (command, footer, header, str)
import           RIO
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
pVersion = option p
    (long "version" <> metavar "VERSION" <> value defv
     <> help ("Chainweb Network Version (default: " <> T.unpack (vText defv) <> ")"))
  where
    p :: ReadM ChainwebVersion
    p = eitherReader (\v -> note ("Invalid Chainweb Version given: " <> v) $ verP v)

    defv :: ChainwebVersion
    defv = Mainnet

pUrl :: Parser BaseUrl
pUrl = hostAddressToBaseUrl <$> host
  where
    host :: Parser HostAddress
    host = option (eitherReader (note "Invalid host" . hostAddressP))
      (long "node" <> metavar "HOSTNAME:PORT" <> help "Node to send TXs")

-- | The immutable runtime environment.
data Env = Env
  { verOf   :: !ChainwebVersion
  , clenvOf :: !ClientEnv
  , keysOf  :: !Keys
  , accOf   :: !Account
  , chanOf  :: !(BChan SignReq)
  , respOf  :: !(TMVar (Maybe Signed)) }
  deriving stock (Generic)

-- | From some CLI `Args`, form the immutable runtime environment.
env :: Args -> IO (Either Text Env)
env (Args v fp acc url) = runExceptT $ do
  ks <- keysFromFile fp !? ("Could not decode key file: " <> T.pack fp)
  mn <- lift $ newManager tlsManagerSettings
  bc <- lift $ newBChan 1
  ts <- newEmptyTMVarIO
  pure $ Env { verOf = v
             , clenvOf = ClientEnv mn url Nothing
             , keysOf = ks
             , accOf = acc
             , chanOf = bc
             , respOf = ts }
