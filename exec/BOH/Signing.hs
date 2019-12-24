{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module BOH.Signing ( signApp ) where

import           Brick.BChan (BChan, writeBChan)
import qualified Kadena.SigningApi as K
import           Network.Wai.Middleware.Cors
import           RIO hiding (Handler)
import           Servant

---

server :: BChan K.SigningRequest -> TMVar (Maybe K.SigningResponse) -> Server K.SigningApi
server bc ts = sign
  where
    sign :: K.SigningRequest -> Handler K.SigningResponse
    sign sr = do
      liftIO $ writeBChan bc sr
      atomically (takeTMVar ts) >>= \case
        Nothing     -> throwM err401
        Just signed -> pure signed

signApp :: BChan K.SigningRequest -> TMVar (Maybe K.SigningResponse) -> Application
signApp bc ts = cors laxCors . serve (Proxy @K.SigningApi) $ server bc ts
  where
    laxCors :: a -> Maybe CorsResourcePolicy
    laxCors _ = Just $ simpleCorsResourcePolicy { corsRequestHeaders = simpleHeaders }
