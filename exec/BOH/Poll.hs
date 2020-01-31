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
pollOnKey (PollArgs v u cid k) = case decode' ("\"" <> k <> "\"") of
  Nothing -> BL.putStrLn "Illegal Transaction ID"
  Just k' -> do
    m <- newManager tlsManagerSettings
    let !cenv = ClientEnv m u Nothing
    runClientM (poll v cid $ Receipt k') cenv >>= \case
      Left _ -> BL.putStrLn "Lookup failure." >> exitFailure
      Right (Just res) -> BL.putStrLn . encodePretty $ txr res
      Right Nothing -> do
        BL.putStrLn "That transaction doesn't exist, or hasn't completed yet."
        exitFailure
