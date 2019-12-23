{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module BOH.Signing
  ( signApp
  ) where

import           Brick.BChan (BChan, writeBChan)
import           Data.Aeson
import           Data.Aeson.Types (prependFailure, typeMismatch)
import           Network.Wai.Middleware.Cors
import           Kadena.SigningApi
import qualified Pact.Types.Command as P
import qualified Pact.Types.Gas as P
import           RIO hiding (Handler)
import           Servant

---

server :: BChan SigningRequest -> TMVar (Maybe SigningResponse) -> Server SigningApi
server bc ts = sign
  where
    sign :: SigningRequest -> Handler SigningResponse
    sign sr = do
      liftIO $ writeBChan bc sr
      atomically (takeTMVar ts) >>= \case
        Nothing     -> throwM err401
        Just signed -> pure signed

signApp :: BChan SigningRequest -> TMVar (Maybe SigningResponse) -> Application
signApp bc ts = cors laxCors . serve (Proxy @SigningApi) $ server bc ts
  where
    laxCors :: a -> Maybe CorsResourcePolicy
    laxCors _ = Just $ simpleCorsResourcePolicy { corsRequestHeaders = simpleHeaders }
