{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module BOH.CLI
  ( Command(..), pCommand
  , Env(..), env
  , UIArgs(..)
  ) where

import           Brick.BChan (BChan, newBChan)
import           Control.Error.Util ((!?))
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Aeson (decodeFileStrict')
import           Holding hiding (command)
import           Holding.Chainweb
import qualified Kadena.SigningApi as K
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Options.Applicative hiding (footer, header, str)
import           RIO
import qualified RIO.List as L
import qualified RIO.Text as T
import           Servant.Client

---

data Command = KeyGen | UI UIArgs

pCommand :: Parser Command
pCommand = hsubparser
  $  command "keys"   (info (pure KeyGen)  (progDesc "Generate public/private key pair"))
  <> command "wallet" (info (UI <$> pArgs) (progDesc "Open the Bag of Holding Wallet UI"))

-- | Wallet UI arguments.
data UIArgs = UIArgs ChainwebVersion FilePath Account BaseUrl

pArgs :: Parser UIArgs
pArgs = UIArgs
  <$> pVersion
  <*> strOption (long "keyfile" <> help "Path to key file")
  <*> (Account <$> strOption (long "account" <> help "Account name"))
  <*> pUrl

pVersion :: Parser ChainwebVersion
pVersion = fmap (ChainwebVersion . T.pack) $ strOption
    (long "version" <> metavar "VERSION" <> value defv
     <> help ("Chainweb Network Version (default: " <> defv <> ")"))
  where
    defv = "mainnet01"

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
  , chanOf  :: !(BChan K.SigningRequest)
  , respOf  :: !(TMVar (Maybe K.SigningResponse)) }
  deriving stock (Generic)

-- | From some CLI `UIArgs`, form the immutable runtime environment.
env :: UIArgs -> IO (Either Text Env)
env (UIArgs v fp acc url) = runExceptT $ do
  ks <- decodeFileStrict' fp !? ("Could not decode key file: " <> T.pack fp)
  mn <- lift $ newManager tlsManagerSettings
  bc <- lift $ newBChan 1
  ts <- newEmptyTMVarIO
  pure $ Env { verOf = v
             , clenvOf = ClientEnv mn url Nothing
             , keysOf = ks
             , accOf = acc
             , chanOf = bc
             , respOf = ts }
