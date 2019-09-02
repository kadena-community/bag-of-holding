module Holding
  ( -- * Public / Private Keys
    Keys(..)
  , keys
  , keysToFile
  , keysFromFile
  ) where

import           Data.Aeson
import           Data.Aeson.Types (prependFailure, typeMismatch)
import qualified Pact.Types.Crypto as P
import           RIO

---

{- OBJECTIVES

- [x] Generate a key pair.
- [x] Export/Import a key pair.

-}

{- OTHER NOTES

The Pact Haskell codebase is quite undocumented.

mkCommand :: (ToJSON m, ToJSON c) => [SomeKeyPair] -> m -> Text -> PactRPC c -> IO (Command ByteString)

sign :: SomeKeyPair -> Hash -> IO ByteString
verify :: SomeScheme -> Hash -> PublicKeyBS -> SignatureBS -> Bool

-}

newtype Keys = Keys { keysOf :: P.SomeKeyPair }

instance ToJSON Keys where
  toJSON (Keys ks) = object [ "public" .= pub ks, "private" .= prv ks ]
    where
      pub, prv :: P.SomeKeyPair -> Value
      pub = toJSON . P.PubBS . P.getPublic
      prv = toJSON . P.PrivBS . P.getPrivate

instance FromJSON Keys where
  parseJSON (Object v) = do
    pub <- v .: "public"
    prv <- v .: "private"
    either (const mempty) (pure . Keys) $ P.importKeyPair P.defaultScheme (Just pub) prv
  parseJSON invalid = prependFailure "parsing Keys failed: " (typeMismatch "Object" invalid)

-- | Generate a Pact-compatible key pair: one public key, and one private key.
-- This uses the ED25519 scheme.
keys :: IO Keys
keys = Keys <$> P.genKeyPair P.defaultScheme

keysToFile :: FilePath -> Keys -> IO ()
keysToFile fp = encodeFile fp

keysFromFile :: FilePath -> IO (Maybe Keys)
keysFromFile = decodeFileStrict'
