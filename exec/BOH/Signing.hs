{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module BOH.Signing
  ( SignReq(..)
  , Signed(..)
  , signApp
  ) where

import           Brick.BChan (BChan, writeBChan)
import           Data.Aeson
import           Data.Aeson.Types (prependFailure, typeMismatch)
import           Network.Wai.Middleware.Cors
import qualified Pact.Types.Command as P
import qualified Pact.Types.Gas as P
import           RIO hiding (Handler)
import           Servant

---

type API = "v1" :> "sign" :> ReqBody '[JSON] SignReq :> Post '[JSON] Signed

data SignReq = SignReq
  { _signReq_code     :: Text
  , _signReq_data     :: Maybe Object
  , _signReq_nonce    :: Maybe Text
  , _signReq_chainId  :: Maybe Text
  , _signReq_gasLimit :: Maybe P.GasLimit
  , _signReq_sender   :: Maybe Text }

instance FromJSON SignReq where
  parseJSON (Object v) = SignReq
    <$> v .:  "code"
    <*> v .:? "data"
    <*> v .:? "nonce"
    <*> v .:? "chainId"
    <*> v .:? "gasLimit"
    <*> v .:? "sender"
  parseJSON invalid =
    prependFailure "parsing SignReq failed, " $ typeMismatch "Object" invalid

data Signed = Signed (P.Command Text) Text

instance ToJSON Signed where
  toJSON (Signed c t) = object [ "body" .= c, "chainId" .= t ]

server :: BChan SignReq -> TMVar (Maybe Signed) -> Server API
server bc ts = sign
  where
    sign :: SignReq -> Handler Signed
    sign sr = do
      liftIO $ writeBChan bc sr
      atomically (takeTMVar ts) >>= \case
        Nothing     -> throwM err401
        Just signed -> pure signed

signApp :: BChan SignReq -> TMVar (Maybe Signed) -> Application
signApp bc ts = cors laxCors . serve (Proxy @API) $ server bc ts
  where
    laxCors :: a -> Maybe CorsResourcePolicy
    laxCors _ = Just $ simpleCorsResourcePolicy { corsRequestHeaders = simpleHeaders }
