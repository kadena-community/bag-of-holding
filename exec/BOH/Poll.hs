{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module BOH.Poll where

import           BOH.CLI (PollArgs(..))
import           Data.Aeson (decode')
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Holding
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           RIO hiding (poll)
import qualified RIO.ByteString.Lazy as BL
import           Servant.Client

---

pollOnKey :: PollArgs -> IO ()
pollOnKey (PollArgs v u cid ks) = do
  m <- newManager tlsManagerSettings
  let !cenv = ClientEnv m u Nothing
  rs <- traverse (f cenv) ks
  traverse_ g rs
  case sequenceA rs of
    Left _  -> exitFailure
    Right _ -> pure ()
  where
    -- | Make a Pact @/poll@ call on the user's input.
    f :: ClientEnv -> BL.ByteString -> IO (Either BL.ByteString TXResult)
    f cenv k = case decode' ("\"" <> k <> "\"") of
      Nothing -> pure . Left $ "Illegal Transaction ID: " <> k
      Just k' -> h k <$> runClientM (poll v cid $ Receipt k') cenv

    -- | Process the results of the HTTP call.
    h :: BL.ByteString -> Either a (Maybe b) -> Either BL.ByteString b
    h k (Left _) = Left $ "Lookup failure: " <> k
    h _ (Right (Just res)) = Right res
    h k (Right Nothing) = Left $ k <> " doesn't exist, or hasn't completed yet."

    -- | Report the results of the post-processing.
    g :: Either BL.ByteString TXResult -> IO ()
    g (Left e)  = BL.putStrLn e
    g (Right r) = BL.putStrLn . encodePretty $ txr r
