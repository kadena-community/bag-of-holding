{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Holding
  ( -- * Types
    -- ** Public / Private Keys
    Keys
  , keys, keysToFile, keysFromFile
    -- ** Pact Code
  , PactCode
  , code, prettyCode
  , codeFromFile
  , Transaction
  , transaction
    -- ** Pact Communication
  , Account(..)
  , meta
  , txTime
  , Receipt
  , prettyReceipt
  , TXResult(..)
  , pactValue
    -- * Calling Pact
    -- | It's assumed that the Pact instance in question lives on a running
    -- Chainweb network.

    -- ** Endpoints
    -- | To call a running Chainweb instance.
  , send
  , poll
  , listen
  , local
    -- ** Coin Contract Functions
    -- | Known functions from the Coin Contract that are built into the Genesis
    -- Blocks.
  , Sender(..), Receiver(..)
  , balance
  , transfer
    -- * Misc.
  , tencode
  , cute
  ) where

import           Chainweb.Pact.RestAPI (pactApi)
import           Chainweb.Time
import           Chainweb.Version
import           Control.Error.Util (hush)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson.Types (prependFailure, typeMismatch)
import           Data.Generics.Wrapped (_Unwrapped)
import           Data.Singletons
import           Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty)
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Lens.Micro (Traversal', _Right)
import qualified Pact.ApiReq as P
import qualified Pact.Compile as P
import qualified Pact.Parse as P
import qualified Pact.Types.API as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Crypto as P
import qualified Pact.Types.Hash as P
import qualified Pact.Types.PactValue as P
import qualified Pact.Types.Pretty as P
import qualified Pact.Types.Runtime as P
import           RIO hiding (local, poll)
import qualified RIO.HashMap as HM
import qualified RIO.NonEmpty as NEL
import qualified RIO.Text as T
import           Servant.API
import           Servant.Client
import           Text.Printf (printf)

---

{- SCOPE

DO:
- Manage keys.
- Allow single-chain transfers, cross-chain transfers, and balance checks (single-sig only).
- Manage active `RequestKeys` in a sane way.
- Allow short, custom TXs to be written.
- Track health of the bootstrap nodes.
- Pretty-print the results of TXs.
- Show a history of recent TXs.
- LISTEN ON PORT 9467 FOR SIGNING TXS IN DAPPS! HAVE MANUAL CONFIRMATION WITHIN WALLET!
- Bare-bones REPL mode that sends gasless TXs to `local/`.
- Show JSON'd config values in Help window.

DON'T:
- Be a module/transaction explorer.
- List existing accounts.

-}

{- OBJECTIVES

- [ ] Support for "pacts" (to enable X-chain transfers)
- [ ] (post launch) Estimate an optimal GasPrice

-}

--------------------------------------------------------------------------------
-- Key Pairs

-- | A public/private key pair to associate and sign Pact `Transaction`s.
newtype Keys = Keys P.SomeKeyPair

-- | To avoid exporting the JSON instances for `Keys`.
newtype Hidden = Hidden { hidden :: Keys }

instance ToJSON Hidden where
  toJSON (Hidden (Keys ks)) = object [ "public" .= pub ks, "private" .= prv ks ]
    where
      pub, prv :: P.SomeKeyPair -> Value
      pub = toJSON . P.PubBS . P.getPublic
      prv = toJSON . P.PrivBS . P.getPrivate

instance FromJSON Hidden where
  parseJSON (Object v) = do
    pub <- v .: "public"
    prv <- v .: "private"
    either mempty (pure . Hidden . Keys) $ P.importKeyPair P.defaultScheme (Just pub) prv
  parseJSON invalid = prependFailure "parsing Keys failed: " (typeMismatch "Object" invalid)

-- | Generate a Pact-compatible key pair: one public key, and one private key.
-- This uses the ED25519 scheme.
keys :: IO Keys
keys = Keys <$> P.genKeyPair P.defaultScheme

keysToFile :: FilePath -> Keys -> IO ()
keysToFile fp = encodeFile fp . Hidden

-- | Will fail with `Nothing` if the `Keys` failed to parse from the expected
-- format.
keysFromFile :: FilePath -> IO (Maybe Keys)
keysFromFile = fmap (fmap hidden) . decodeFileStrict'

--------------------------------------------------------------------------------
-- Pact Code

data PactCode = PactCode { rawOf :: !Text }

instance Show PactCode where
  show = T.unpack . prettyCode

-- | Prove that some raw `Text` is real Pact code.
code :: Text -> Maybe PactCode
code t = hush (P.parseExprs t) >>= hush . P.compileExps P.mkEmptyInfo >> pure (PactCode t)

-- | Pretty-print some `PactCode`.
prettyCode :: PactCode -> Text
-- prettyCode = renderStrict . layoutPretty defaultLayoutOptions . P.pretty . codeOf
-- prettyCode = T.pack . P.renderPrettyString P.RPlain . codeOf
prettyCode = rawOf

-- | Will fail with `Nothing` if the file couldn't be parsed as legal Pact.
codeFromFile :: FilePath -> IO (Maybe PactCode)
codeFromFile = fmap code . readFileUtf8

-- | A parsed and signed transaction, ready to be sent to a running Chainweb
-- instance.
newtype Transaction = Transaction (P.Command Text)

-- | Form some parsed `PactCode` into a `Transaction` that's sendable to a running
-- Chainweb instance.
transaction :: PactCode -> Keys -> P.PublicMeta -> IO Transaction
transaction (PactCode pc) (Keys ks) pm =
  Transaction <$> P.mkExec (T.unpack pc) Null pm [ks] Nothing

--------------------------------------------------------------------------------
-- Pact Communication

-- | A "Coin Contract" account.
newtype Account = Account Text

-- TODO Come up with a sane default `GasPrice`.
-- TODO What is the difference between the two gas values?
-- | To feed to the `transaction` function.
meta :: Account -> ChainId -> IO P.PublicMeta
meta (Account t) c = P.PublicMeta c' t gl gp (P.TTLSeconds 3600) <$> txTime
  where
    c' = P.ChainId $ chainIdToText c
    gl = P.GasLimit 100
    gp = P.GasPrice 0.00000001

txTime :: IO P.TxCreationTime
txTime = do
  Time (TimeSpan (Micros m)) <- getCurrentTimeIntegral
  pure . fromIntegral $ m `div` 1000000

-- | Confirmation that a `Transaction` has been accepted by the network. This
-- can be used again as input to other calls to inspect the final results of
-- that `Transaction`.
newtype Receipt = Receipt P.RequestKey deriving stock (Generic)

prettyReceipt :: Receipt -> Text
prettyReceipt (Receipt r) = P.requestKeyToB16Text r

-- | The final result/outcome of some sent `Transaction`.
newtype TXResult = TXResult { txr :: (P.CommandResult P.Hash) }
  deriving stock (Generic)

-- | Attempt to pull a real `P.PactValue` from some returned `TXResult`.
pactValue :: Traversal' TXResult P.PactValue
pactValue = _Unwrapped . P.crResult . _Unwrapped . _Right

--------------------------------------------------------------------------------
-- Endpoint Calls

-- | Submit a `Transaction` to Chainweb. This will cost gas, and the associated
-- `Account` will be charged.
send :: ChainwebVersion -> ChainId -> Transaction -> ClientM Receipt
send v cid (Transaction tx) = case clients v cid of
  f :<|> _ -> Receipt . NEL.head . P._rkRequestKeys <$> f (P.SubmitBatch $ pure tx)

-- | A quick peek into the status of a `Transaction`. Unlike `listen`, this is
-- non-blocking and so will always return right away, even when the
-- `Transaction` has not completed.
poll :: ChainwebVersion -> ChainId -> Receipt -> ClientM (Maybe TXResult)
poll v cid (Receipt rk) = case clients v cid of
  _ :<|> f :<|> _ -> g <$> f (P.Poll $ pure rk)
  where
    g :: P.PollResponses -> Maybe TXResult
    g (P.PollResponses hm) = TXResult <$> HM.lookup rk hm

-- | Do a blocking call to wait for the results corresponding to some `Receipt.`
-- Might time out, in which case `Nothing` is returned. Should return quickly
-- for `Transaction`s which have already completed.
listen :: ChainwebVersion -> ChainId -> Receipt -> ClientM (Maybe TXResult)
listen v cid (Receipt rk) = case clients v cid of
  _ :<|> _ :<|> f :<|> _ -> g <$> f (P.ListenerRequest rk)
  where
    g :: P.ListenResponse -> Maybe TXResult
    g (P.ListenTimeout _)   = Nothing
    g (P.ListenResponse cr) = Just $ TXResult cr

-- | A non-blocking `Transaction` that can't write changes and spends no gas.
local :: ChainwebVersion -> ChainId -> Transaction -> ClientM TXResult
local v cid (Transaction tx) = case clients v cid of
  _ :<|> _ :<|> _ :<|> f -> TXResult <$> f tx

clients
  :: ChainwebVersion
  -> ChainId
  ->   (P.SubmitBatch -> ClientM P.RequestKeys)
  :<|> (P.Poll -> ClientM P.PollResponses)
  :<|> (P.ListenerRequest -> ClientM P.ListenResponse)
  :<|> (P.Command Text -> ClientM (P.CommandResult P.Hash))
clients (FromSing (SChainwebVersion :: Sing v)) (FromSing (SChainId :: Sing cid)) =
  client (pactApi @v @cid)

--------------------------------------------------------------------------------
-- Coin Contract Functions

-- | The sender `Account` in a coin transfer.
newtype Sender = Sender Account

-- | The receiver `Account` in a coin transfer.
newtype Receiver = Receiver Account

-- | The @coin.account-balance@ function.
balance :: Account -> Maybe PactCode
balance (Account a) = code . T.pack $ printf "(coin.account-balance \"%s\")" a

-- | The @coin.transfer@ function.
transfer :: Sender -> Receiver -> Double -> Maybe PactCode
transfer (Sender (Account s)) (Receiver (Account r)) d =
  code . T.pack $ printf "(coin.transfer \"%s\" \"%s\" %f)" s r d

--------------------------------------------------------------------------------
-- Misc.

cute :: P.Doc -> Text
cute = renderStrict . layoutPretty defaultLayoutOptions

-- | Pretty-render a JSON value as strict `Text`.
tencode :: ToJSON a => a -> Text
tencode = T.decodeUtf8With lenientDecode . toStrictBytes . encodePretty . toJSON
