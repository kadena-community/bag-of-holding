{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Holding
  ( -- * Types
    -- ** Public / Private Keys
    Keys(..)
  , keys
  , keysToFile
  , keysFromFile
    -- * Calling Pact
    -- | It's assumed that the Pact instance in question lives on a running
    -- Chainweb network.
  , send
  ) where

import           Chainweb.Pact.RestAPI (pactApi)
import           Chainweb.Version
import           Data.Aeson
import           Data.Aeson.Types (prependFailure, typeMismatch)
import           Data.Singletons
import qualified Pact.Types.API as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Crypto as P
import qualified Pact.Types.Hash as P
import           RIO
import           Servant.API
import           Servant.Client (ClientM, client)

---

{- OBJECTIVES

- [x] Generate a key pair.
- [x] Export/Import a key pair.
- [x] What endpoint(s) do I send to?
- [x] Depend on Chainweb.
- [x] Pact client calls.
- [ ] How to form a `SubmitBatch`?
- [ ] Send arithemetic.
- [ ] Transaction signing?
- [ ] Create coin account.
- [ ] Data type for Account

-}

{- OTHER NOTES

The Pact Haskell codebase is quite undocumented.

mkCommand :: (ToJSON m, ToJSON c) => [SomeKeyPair] -> m -> Text -> PactRPC c -> IO (Command ByteString)

sign :: SomeKeyPair -> Hash -> IO ByteString
verify :: SomeScheme -> Hash -> PublicKeyBS -> SignatureBS -> Bool

-}

--------------------------------------------------------------------------------
-- Key Pairs

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

--------------------------------------------------------------------------------
-- Endpoint Calls

{- SENDING COMMANDS

type ApiV1API = "api" :> ("v1" :> (ApiSend :<|> (ApiPoll :<|> (ApiListen :<|> ApiLocal))))

type ApiSend = "send" :> (ReqBody '[JSON] SubmitBatch :> Post '[JSON] RequestKeys)
newtype SubmitBatch = SubmitBatch { _sbCmds :: NonEmpty (Command Text) }

type ApiPoll = "poll" :> (ReqBody '[JSON] Poll :> Post '[JSON] PollResponses)
newtype Poll = Poll { _pRequestKeys :: NonEmpty RequestKey }

type ApiListen = "listen" :> (ReqBody '[JSON] ListenerRequest :> Post '[JSON] ListenResponse)
newtype ListenerRequest = ListenerRequest { _lrListen :: RequestKey }

type ApiLocal = "local" :> (ReqBody '[JSON] (Command Text) :> Post '[JSON] (CommandResult Hash))
data Command a = Command
  { _cmdPayload :: !a
  , _cmdSigs :: ![UserSig]
  , _cmdHash :: !PactHash
  }

-}

send :: ChainwebVersion -> ChainId -> P.SubmitBatch -> ClientM P.RequestKeys
send v cid = case clients v cid of
  f :<|> _ :<|> _ :<|> _ -> f

clients
  :: ChainwebVersion
  -> ChainId
  ->   (P.SubmitBatch -> ClientM P.RequestKeys)
  :<|> (P.Poll -> ClientM P.PollResponses)
  :<|> (P.ListenerRequest -> ClientM P.ListenResponse)
  :<|> (P.Command Text -> ClientM (P.CommandResult P.Hash))
clients (FromSing (SChainwebVersion :: Sing v)) (FromSing (SChainId :: Sing cid)) =
  client (pactApi @v @cid)
