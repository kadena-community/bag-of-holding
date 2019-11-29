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
import qualified RIO.List as L
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
pUrl = option (eitherReader pBaseUrl)
  (long "node" <> metavar "HOSTNAME:PORT" <> help "Node to send TXs")
  where
    pBaseUrl :: String -> Either String BaseUrl
    pBaseUrl s = case L.break (== ':') s of
      ([],_) -> Left "Empty input"
      (h, ':' : mp)  -> case readMaybe mp of
          Nothing -> Left "Malformed port"
          Just p  -> Right $ BaseUrl Https h p ""
      _ -> Left "Bad url/port"

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
