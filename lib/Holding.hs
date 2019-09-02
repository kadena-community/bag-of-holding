module Holding
  ( -- * Public / Private Keys
    Keys(..)
  , keys
  ) where

import           Data.Aeson
import           Data.Aeson.Types (prependFailure, typeMismatch)
import qualified Pact.Types.Crypto as P
import           RIO

---

{- OBJECTIVES

- [x] Generate a key pair.
- [ ] Export a key pair.
- [ ] Import a key pair.

-}

{- OTHER NOTES

The Pact Haskell codebase is quite undocumented.

genKeyPair :: IO (PublicKey, PrivateKey)
getPublicKey :: PrivateKey -> PublicKey
genKeyPair :: SomeScheme -> IO SomeKeyPair
mkKeyPairs :: [ApiKeyPair] -> IO [SomeKeyPair]
mkCommand :: (ToJSON m, ToJSON c) => [SomeKeyPair] -> m -> Text -> PactRPC c -> IO (Command ByteString)

sign :: SomeKeyPair -> Hash -> IO ByteString
verify :: SomeScheme -> Hash -> PublicKeyBS -> SignatureBS -> Bool
getPublic :: SomeKeyPair -> ByteString
getPrivate :: SomeKeyPair -> ByteString
formatPublicKey :: SomeKeyPair -> ByteString

Who has nice JSON instances?
- PublicKeyBS
- PrivateKeyBS

But how to convert from those types...

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
    case P.importKeyPair P.defaultScheme (Just pub) prv of
      Left _    -> undefined  -- TODO
      Right skp -> pure $ Keys skp
  parseJSON invalid = prependFailure "parsing Keys failed, " (typeMismatch "Object" invalid)

-- | Generate a Pact-compatible key pair: one public key, and one private key.
-- This uses the ED25519 scheme.
keys :: IO Keys
keys = Keys <$> P.genKeyPair P.defaultScheme

-- work :: IO ()
-- work = do
--   kp <- keyPair
--   traceShowIO . toJSON . P.PubBS $ P.getPublic kp
