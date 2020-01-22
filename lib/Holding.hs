{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Holding
  ( -- * Types
    -- ** Public / Private Keys
    Keys
  , keys
    -- ** Pact Code
  , PactCode
  , code, prettyCode
  , codeFromFile
  , Transaction
  , transaction
  , command
  , TxData(..)
  , gasCap, transferCap
  , KDA
  , kda
    -- ** Pact Communication
  , Account(..)
  , meta
  , txTime
  , Receipt
  , prettyReceipt
  , TXResult(..)
  , pactValue
  , pactDouble
    -- * Calling Pact
    -- | It's assumed that the Pact instance in question lives on a running
    -- Chainweb network.

    -- ** Endpoints
    -- | To call a running Chainweb instance.
  , send, sends
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

import           Control.Error.Util (hush)
import           Data.Aeson
import           Data.Aeson.Types (prependFailure, typeMismatch)
import           Data.Decimal (Decimal, decimalPlaces)
import           Data.Generics.Sum.Constructors (_Ctor)
import           Data.Generics.Wrapped (_Unwrapped)
import           Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty)
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Yaml.Pretty (defConfig, encodePretty)
import           Holding.Chainweb
import           Lens.Micro (SimpleFold, Traversal', _Right)
import qualified Pact.ApiReq as P
import qualified Pact.Compile as P
import qualified Pact.Parse as P
import qualified Pact.Types.API as P
import qualified Pact.Types.Capability as P
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
- [ ] Manage keys.
- [ ] Allow simple transfers, cross-chain transfers, and balance checks (single-sig only).
- [x] Manage active `RequestKeys` in a sane way.
- [x] Allow short, custom TXs to be written.
- [x] Pretty-print the results of TXs.
- [x] Show a history of recent TXs.
- [x] LISTEN ON PORT 9467 FOR SIGNING TXS IN DAPPS! HAVE MANUAL CONFIRMATION WITHIN WALLET!
- [x] Bare-bones REPL mode that sends gasless TXs to `local/`.
- [x] Show JSON'd config values in Help window.
- [ ] Use `brick-skylighting` for syntax highlighting.
- [x] Print TX results as YAML, not JSON.

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
    either mempty (pure . Keys) $ P.importKeyPair P.defaultScheme (Just pub) prv
  parseJSON invalid = prependFailure "parsing Keys failed: " (typeMismatch "Object" invalid)

-- | Generate a Pact-compatible key pair: one public key, and one private key.
-- This uses the ED25519 scheme.
keys :: IO Keys
keys = Keys <$> P.genKeyPair P.defaultScheme

--------------------------------------------------------------------------------
-- Pact Code

newtype PactCode = PactCode { rawOf :: Text }

instance Show PactCode where
  show = T.unpack . prettyCode

-- | Prove that some raw `Text` is real Pact code.
code :: Text -> Maybe PactCode
code t = hush (P.parseExprs t) >>= hush . P.compileExps P.mkEmptyInfo >> pure (PactCode t)

-- | Pretty-print some `PactCode`.
prettyCode :: PactCode -> Text
prettyCode = rawOf

-- | Will fail with `Nothing` if the file couldn't be parsed as legal Pact.
codeFromFile :: FilePath -> IO (Maybe PactCode)
codeFromFile = fmap code . readFileUtf8

-- | A parsed and signed transaction, ready to be sent to a running Chainweb
-- instance.
newtype Transaction = Transaction { cmdt :: P.Command Text }

command :: SimpleGetter Transaction (P.Command Text)
command = to cmdt

-- | Form some parsed `PactCode` into a `Transaction` that's sendable to a running
-- Chainweb instance.
transaction
  :: ChainwebVersion
  -> TxData
  -> PactCode
  -> [P.SigCapability]
  -> Keys
  -> P.PublicMeta
  -> IO Transaction
transaction v (TxData td) (PactCode pc) caps (Keys ks) pm =
  Transaction <$> P.mkExec (T.unpack pc) td pm [(ks, caps)] nid Nothing
  where
    nid :: Maybe P.NetworkId
    nid = Just . P.NetworkId $ chainwebVersionToText v

newtype TxData = TxData Value deriving newtype (ToJSON, FromJSON)

gasCap :: P.SigCapability
gasCap = P.SigCapability (P.QualifiedName "coin" "GAS" (P.mkInfo "coin.GAS")) []

transferCap :: Sender -> Receiver -> KDA -> P.SigCapability
transferCap (Sender (Account s)) (Receiver (Account r)) (KDA m) =
  P.SigCapability (P.QualifiedName "coin" "TRANSFER" (P.mkInfo "coin.TRANSFER"))
  [ P.PLiteral $ P.LString s
  , P.PLiteral $ P.LString r
  , P.PLiteral $ P.LDecimal m ]

-- | Enforces Pact's rounding and truncation rules.
newtype KDA = KDA Decimal deriving newtype (Show, Read, Num)

-- | Smart constructor for the `KDA` type. This requires there to be no more
-- than 12 digits after the decimal point.
kda :: Decimal -> Maybe KDA
kda d = bool Nothing (Just $ KDA d) $ decimalPlaces d <= 12

--------------------------------------------------------------------------------
-- Pact Communication

-- | A "Coin Contract" account.
newtype Account = Account Text deriving (Generic)

-- TODO Make the `GasLimit` an argument for the signing API.
-- | To feed to the `transaction` function.
meta :: Account -> P.ChainId -> P.GasLimit -> IO P.PublicMeta
meta (Account t) c gl = P.PublicMeta c t gl gp (P.TTLSeconds 3600) <$> txTime
  where
    gp = P.GasPrice 0.00001

txTime :: IO P.TxCreationTime
txTime = fromInteger . round <$> getPOSIXTime

-- | Confirmation that a `Transaction` has been accepted by the network. This
-- can be used again as input to other calls to inspect the final results of
-- that `Transaction`.
newtype Receipt = Receipt P.RequestKey deriving stock (Generic)

newtype Receipts = Receipts (NonEmpty P.RequestKey) deriving stock (Generic)

prettyReceipt :: Receipt -> Text
prettyReceipt (Receipt r) = P.requestKeyToB16Text r

-- | The final result/outcome of some sent `Transaction`.
newtype TXResult = TXResult { txr :: P.CommandResult P.Hash }
  deriving stock (Generic)

-- | Attempt to pull a real `P.PactValue` from some returned `TXResult`.
pactValue :: Traversal' TXResult P.PactValue
pactValue = _Unwrapped . P.crResult . _Unwrapped . _Right

pactDouble :: SimpleFold TXResult Decimal
pactDouble = pactValue . _Ctor @"PLiteral" . _Ctor @"LDecimal"

--------------------------------------------------------------------------------
-- Endpoint Calls

type PactAPI = SendAPI :<|> PollAPI :<|> ListenAPI :<|> LocalAPI

type SendAPI = "chainweb"
  :> "0.0"
  :> Capture "version" ChainwebVersion
  :> "chain"
  :> Capture "chainId" P.ChainId
  :> "pact"
  :> "api"
  :> "v1"
  :> "send"
  :> ReqBody '[JSON] P.SubmitBatch
  :> Post '[JSON] P.RequestKeys

type PollAPI = "chainweb"
  :> "0.0"
  :> Capture "version" ChainwebVersion
  :> "chain"
  :> Capture "chainId" P.ChainId
  :> "pact"
  :> "api"
  :> "v1"
  :> "poll"
  :> ReqBody '[JSON] P.Poll
  :> Post '[JSON] P.PollResponses

type ListenAPI = "chainweb"
  :> "0.0"
  :> Capture "version" ChainwebVersion
  :> "chain"
  :> Capture "chainId" P.ChainId
  :> "pact"
  :> "api"
  :> "v1"
  :> "listen"
  :> ReqBody '[JSON] P.ListenerRequest
  :> Post '[JSON] P.ListenResponse

type LocalAPI = "chainweb"
  :> "0.0"
  :> Capture "version" ChainwebVersion
  :> "chain"
  :> Capture "chainId" P.ChainId
  :> "pact"
  :> "api"
  :> "v1"
  :> "local"
  :> ReqBody '[JSON] (P.Command Text)
  :> Post '[JSON] (P.CommandResult P.Hash)

-- | Submit a `Transaction` to Chainweb. This will cost gas, and the associated
-- `Account` will be charged.
send :: ChainwebVersion -> P.ChainId -> Transaction -> ClientM Receipt
send v cid (Transaction tx) =
  Receipt . NEL.head . P._rkRequestKeys <$> send' v cid (P.SubmitBatch $ pure tx)

sends :: ChainwebVersion -> P.ChainId -> NonEmpty Transaction -> ClientM Receipts
sends v cid txs =
  Receipts . P._rkRequestKeys <$> send' v cid (P.SubmitBatch $ NEL.map cmdt txs)

-- | A quick peek into the status of a `Transaction`. Unlike `listen`, this is
-- non-blocking and so will always return right away, even when the
-- `Transaction` has not completed.
poll :: ChainwebVersion -> P.ChainId -> Receipt -> ClientM (Maybe TXResult)
poll v cid (Receipt rk) = g <$> poll' v cid (P.Poll $ pure rk)
  where
    g :: P.PollResponses -> Maybe TXResult
    g (P.PollResponses hm) = TXResult <$> HM.lookup rk hm

-- | Do a blocking call to wait for the results corresponding to some `Receipt.`
-- Might time out, in which case `Nothing` is returned. Should return quickly
-- for `Transaction`s which have already completed.
listen :: ChainwebVersion -> P.ChainId -> Receipt -> ClientM (Maybe TXResult)
listen v cid (Receipt rk) = g <$> listen' v cid (P.ListenerRequest rk)
  where
    g :: P.ListenResponse -> Maybe TXResult
    g (P.ListenTimeout _)   = Nothing
    g (P.ListenResponse cr) = Just $ TXResult cr

-- | A non-blocking `Transaction` that can't write changes and spends no gas.
local :: ChainwebVersion -> P.ChainId -> Transaction -> ClientM TXResult
local v cid (Transaction tx) = TXResult <$> local' v cid tx

send'   :: ChainwebVersion -> P.ChainId -> P.SubmitBatch -> ClientM P.RequestKeys
poll'   :: ChainwebVersion -> P.ChainId -> P.Poll -> ClientM P.PollResponses
listen' :: ChainwebVersion -> P.ChainId -> P.ListenerRequest -> ClientM P.ListenResponse
local'  :: ChainwebVersion -> P.ChainId -> P.Command Text -> ClientM (P.CommandResult P.Hash)
send' :<|> poll' :<|> listen' :<|> local' = client (Proxy @PactAPI)

--------------------------------------------------------------------------------
-- Coin Contract Functions

-- | The sender `Account` in a coin transfer.
newtype Sender = Sender Account

-- | The receiver `Account` in a coin transfer.
newtype Receiver = Receiver Account deriving stock (Generic)

-- | The @coin.get-balance@ function.
balance :: Account -> Maybe PactCode
balance (Account a) = code . T.pack $ printf "(coin.get-balance \"%s\")" a

-- | The @coin.transfer@ function.
transfer :: Sender -> Receiver -> KDA -> Maybe PactCode
transfer (Sender (Account s)) (Receiver (Account r)) (KDA d) =
  code . T.pack $ printf "(coin.transfer \"%s\" \"%s\" %s)" s r (show d)

--------------------------------------------------------------------------------
-- Misc.

cute :: P.Doc -> Text
cute = renderStrict . layoutPretty defaultLayoutOptions

-- | Pretty-render a JSON value as strict `Text`.
tencode :: ToJSON a => a -> Text
tencode = T.decodeUtf8With lenientDecode . encodePretty defConfig
