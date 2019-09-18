{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Main ( main ) where

import           Brick
import qualified Brick.AttrMap as A
import           Brick.Focus
import           Brick.Forms
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import           Chainweb.HostAddress (HostAddress, hostAddressToBaseUrl)
import           Chainweb.Utils (textOption, toText)
import           Chainweb.Version
import           Control.Error.Util (hoistMaybe, hush, (!?))
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.Aeson
import           Data.Bitraversable (bitraverse)
import           Data.Generics.Product.Fields (field)
import           Data.Generics.Product.Positions (position)
import           Data.Generics.Sum.Constructors (_Ctor)
import           Data.Generics.Wrapped (_Unwrapped)
import qualified Graphics.Vty as V
import           Holding
import           Lens.Micro
import           Lens.Micro.Extras (preview)
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.Wai.Handler.Warp as W
import           Options.Applicative hiding (footer, header, str)
import qualified Pact.Types.ChainMeta as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Runtime as P
import           RIO hiding (Handler, local, on)
import qualified RIO.HashSet as HS
import qualified RIO.List as L
import           RIO.Partial (fromJust)
import qualified RIO.Seq as Seq
import qualified RIO.Text as T
import           Servant
import           Servant.Client
import           Text.Printf (printf)

--------------------------------------------------------------------------------
-- CLI Handling

-- | CLI arguments.
data Args = Args ChainwebVersion FilePath Account BaseUrl

pArgs :: Parser Args
pArgs = Args
  <$> pVersion
  <*> strOption (long "keyfile" <> help "Path to key file")
  <*> (Account <$> strOption (long "account" <> help "Account name"))
  <*> pUrl

pVersion :: Parser ChainwebVersion
pVersion = textOption
    (long "version" <> metavar "VERSION" <> value defv
     <> help ("Chainweb Network Version (default: " <> T.unpack (toText defv) <> ")"))
  where
    defv :: ChainwebVersion
    defv = Testnet02

pUrl :: Parser BaseUrl
pUrl = hostAddressToBaseUrl Https <$> host
  where
    host :: Parser HostAddress
    host = textOption (long "node" <> metavar "HOSTNAME:PORT" <> help "Node to send TXs")

-- | The immutable runtime environment.
data Env = Env
  { verOf   :: !ChainwebVersion
  , clenvOf :: !ClientEnv
  , keysOf  :: !Keys
  , accOf   :: !Account }
  deriving stock (Generic)

-- | From some CLI `Args`, form the immutable runtime environment.
-- env :: Args -> IO (Either Text (LogFunc -> Env))
env :: Args -> IO (Either Text Env)
env (Args v fp acc url) = runExceptT $ do
  ks <- keysFromFile fp !? ("Could not decode key file: " <> T.pack fp)
  mn <- lift $ newManager tlsManagerSettings
  pure $ Env v (ClientEnv mn url Nothing) ks acc

--------------------------------------------------------------------------------
-- Endpoint Calling

call :: Env -> REPL -> IO TX
call e r@(REPL cid ep c) = do
  m  <- meta (accOf e) cid
  tx <- transaction c (keysOf e) m
  TX r <$> runClientM (f tx) (clenvOf e)
  where
    f :: Transaction -> ClientM From
    f = case ep of
          Local -> fmap T . local (verOf e) cid
          Send  -> fmap R . send (verOf e) cid

--------------------------------------------------------------------------------
-- Terminal UI

type Listo = L.GenericList Name Seq TX

data TX = TX REPL (Either ClientError From)
  deriving stock (Generic)

data Wallet = Wallet
  { txsOf  :: !Listo
  , logOf  :: !Text
  , focOf  :: !(FocusRing Name)
  , replOf :: !(Form REPL () Name)
  , balsOf :: [(ChainId, Maybe Double)] }
  deriving stock (Generic)

data From = R Receipt | T TXResult deriving (Generic)

data Endpoint = Local | Send deriving (Eq)

data REPL = REPL { rcid :: !ChainId, re :: !Endpoint, rpc :: !PactCode }
  deriving stock (Generic)

-- | Resource names.
data Name = TXList | ReplChain | ReplLocal | ReplSend | ReplCode | Help | Balances
  deriving stock (Eq, Ord, Show, Enum, Bounded)

header :: Widget a
header = vLimit 1 . C.center $ txt " The Bag of Holding "

footer :: Text -> Widget a
footer t = vLimit 1 $ txt (T.take 10 t) <+> C.hCenter legend
  where
    legend = txt "[T]ransaction - [B]alances - [H]elp - [Q]uit"

replForm :: Env -> REPL -> Form REPL e Name
replForm e = newForm
  [ label "Chain" @@= editField (field @"rcid") ReplChain Nothing
    toText goodChain (txt . T.unlines) id
  , label "Endpoint" @@= radioField (field @"re")
    [(Local, ReplLocal, "Local"), (Send, ReplSend, "Send")]
  , label "Pact Code" @@= editField (field @"rpc") ReplCode Nothing
    prettyCode (code . T.unlines) (txt . T.unlines) id
  ]
  where
    label :: Text -> Widget Name -> Widget Name
    label t w = padBottom (Pad 1) $ vLimit 1 (hLimit 15 $ txt t <+> fill ' ') <+> w

    -- | Requires that the specified `ChainId` be a valid member of the Chain
    -- Graph of the current `ChainwebVersion`.
    goodChain :: [Text] -> Maybe ChainId
    goodChain ts = do
      cid <- chainIdFromText $ T.unlines ts
      bool Nothing (Just cid) . HS.member cid . chainIds $ verOf e

main :: IO ()
main = execParser opts >>= env >>= \case
  Left _ -> pure () -- TODO Say something.
  Right e -> race_ (W.run 9467 signApp) (void $ defaultMain (app e) (initial e))
  where
    initial :: Env -> Wallet
    initial e = Wallet
      (L.list TXList mempty 1)
      ""
      (focusRing [minBound ..])
      (replForm e . REPL (unsafeChainId 0) Local . fromJust $ code "(+ 1 1)") []

    opts :: ParserInfo Args
    opts = info (pArgs <**> helper)
        (fullDesc <> progDesc "The Bag of Holding: A Chainweb Wallet")

    app :: Env -> App Wallet () Name
    app e = App { appDraw = draw e
                , appChooseCursor = focusRingCursor focOf
                , appHandleEvent = event e
                , appStartEvent = pure
                , appAttrMap = const $ A.attrMap V.defAttr attrs }

    attrs :: [(AttrName, V.Attr)]
    attrs = [ -- (L.listAttr,         V.white `on` V.blue)
              (L.listSelectedAttr,   V.blue  `on` V.white)
            , (E.editAttr,           V.white `on` V.black)
            , (E.editFocusedAttr,    V.black `on` V.yellow)
            , (invalidFormInputAttr, V.white `on` V.red)
            , (focusedFormInputAttr, V.black `on` V.yellow) ]

draw :: Env -> Wallet -> [Widget Name]
draw e w = dispatch <> [ui]
  where
    ui :: Widget Name
    ui = header <=> (left <+> right) <=> footer (logOf w)

    dispatch :: [Widget Name]
    dispatch = case focusGetCurrent $ focOf w of
      Just ReplCode -> [repl]
      Just Help     -> [he1p]
      Just Balances -> [balances]
      _             -> []

    repl :: Widget Name
    repl = C.centerLayer . vLimit 10 . hLimitPercent 50
           . B.borderWithLabel (txt " Pact Transaction ")
           $ renderForm (replOf w) <=> C.hCenter (txt "[Esc] [Enter]")

    -- TODO Look into text wrapping.
    he1p :: Widget Name
    he1p = C.centerLayer . vLimit 16 . hLimitPercent 50 . B.borderWithLabel (txt " Help ")
      $ vBox
      [ C.hCenter . padBottom (Pad 1) $ txt "The Bag of Holding - A Chainweb Wallet"
      , txt "Author:   Colin Woodbury"
      , txt "Issues:   " <+> hyperlink url (txt url)
      , txt $ "Chainweb: " <> toText (verOf e)
      , txt $ "Account:  " <> (accOf e ^. _Unwrapped)
      , padTop (Pad 1) $ txt "A note on endpoints:"
      , txt "LOCAL: Transaction is 'free', but results aren't"
      , txt "       saved to the blockchain. Returns instantly."
      , txt "SEND:  Transaction is mined into a block."
      , txt "       Costs gas and takes time for the results."
      , padTop (Pad 1) $ C.hCenter (txt "Press any key.") ]
      where
        url = "gitlab.com/fosskers/bag-of-holding"

    balances :: Widget Name
    balances =
      C.centerLayer . vLimit 14 . hLimitPercent 50 . B.borderWithLabel (txt " Balances ")
      $ vBox (map f $ balsOf w) <=> padTop (Pad 1) (C.hCenter $ txt "Press any key.")
      where
        f :: (ChainId, Maybe Double) -> Widget Name
        f (cid, md) = hBox
          [ txt "Chain ", txt (toText cid), txt " => "
          , str $ maybe "Balance check failed." show md ]

    -- TODO Consider `round` border style.
    left :: Widget Name
    left = hLimitPercent 50 $ B.borderWithLabel (txt " Transaction History ") txs

    txs :: Widget Name
    txs | Seq.null (txsOf w ^. L.listElementsL) = txt "No transactions yet!" <+> fill ' '
        | otherwise = L.renderList (const txListItem) True $ txsOf w

    txListItem :: TX -> Widget Name
    txListItem (TX (REPL cid ep pc) eef) = vLimit 1 $ hBox
      [ hLimit 1 $ txt icon
      , padLeft (Pad 2) . str $ printf "%02d" (chainIdInt cid :: Int)
      , padLeft (Pad 2) $ txt end
      , padLeft (Pad 2) . txt $ prettyCode pc
      , fill ' ' ]
      where
        icon = case eef of
          Left _      -> "☹"
          Right (R _) -> "?"
          Right (T t) -> maybe "☹" (const "✓") (t ^? pactValue)
        end = case ep of
          Local -> "LOCL"
          Send  -> "SEND"

    right :: Widget Name
    right = B.borderWithLabel (txt " Transaction Result ") $ contents <=> fill ' '
      where
        contents :: Widget Name
        contents = case w ^? from of
          Nothing             -> txt "Select a Transaction."
          Just (TX _ eef) -> case eef of
            Left _      -> txt "This Transaction had an HTTP failure."
            Right (T t) -> txt . tencode $ txr t
            Right (R r) -> vBox [ txt $ prettyReceipt r
                                , padTop (Pad 1) $ txt "Press [Enter] to query the result." ]

event :: Env -> Wallet -> BrickEvent Name () -> EventM Name (Next Wallet)
event e w be = case focusGetCurrent $ focOf w of
  Nothing       -> continue w
  Just TXList   -> mainEvent e w be
  Just Help     -> simplePage w be
  Just Balances -> simplePage w be
  Just _        -> replEvent e w be

replEvent :: Env -> Wallet -> BrickEvent Name () -> EventM Name (Next Wallet)
replEvent e w ev@(VtyEvent ve) = case ve of
  -- Close Popups --
  V.EvKey V.KEsc [] -> continue (w & field @"focOf" %~ focusSetCurrent TXList)

  -- Submission --
  V.EvKey V.KEnter []
    | not (allFieldsValid $ replOf w) -> continue w
    | otherwise -> do
        t <- liftIO . call e . formState $ replOf w
        continue $ w & field @"focOf" %~ focusSetCurrent TXList
                     & field @"txsOf" %~ (L.listMoveTo 0 . L.listInsert 0 t)

  -- Code Input --
  _ -> handleFormEventL (field @"replOf") w ev >>= continue
replEvent _ w _ = continue w

-- | Display some simple page until any key is pressed.
simplePage :: Wallet -> BrickEvent Name () -> EventM Name (Next Wallet)
simplePage w be = case be of
  VtyEvent (V.EvKey _ []) -> continue (w & field @"focOf" %~ focusSetCurrent TXList)
  _ -> continue w

mainEvent :: Env -> Wallet -> BrickEvent Name e -> EventM Name (Next Wallet)
mainEvent e w (VtyEvent ve) = case ve of
  -- Quit --
  V.EvKey (V.KChar 'q') [] -> halt w

  -- Transaction Form --
  V.EvKey (V.KChar 't') [] -> continue (w & field @"focOf" %~ focusSetCurrent ReplCode)

  -- Balance Check --
  V.EvKey (V.KChar 'b') [] -> do
    txs <- liftIO $ mapConcurrently (bitraverse pure (traverse (call e))) rs
    continue $ w & field @"focOf"  %~ focusSetCurrent Balances
                 & field @"balsOf" .~ map (second ds) txs
    where
      cs = L.sort . toList . chainIds $ verOf e
      cd = balance $ accOf e
      rs = map (\cid -> (cid, REPL cid Local <$> cd)) cs
      ds = preview (_Just . position @2 . _Right . _Ctor @"T" . pactDouble)

  -- Help Window --
  V.EvKey (V.KChar 'h') [] -> continue (w & field @"focOf" %~ focusSetCurrent Help)

  -- History Selection --
  V.EvKey V.KEnter [] -> liftIO f >>= continue . fromMaybe w
    where
      f :: IO (Maybe Wallet)
      f = runMaybeT $ do
        TX (REPL cid _ _) eef <- hoistMaybe (w ^? from)
        r <- hoistMaybe (eef ^? _Right . _Ctor @"R")
        t <- MaybeT . fmap (join . hush) $ runClientM (listen (verOf e) cid r) (clenvOf e)
        pure (w & field @"txsOf" %~ L.listModify (set (position @2) (Right $ T t)))

  -- Keyboard Navigation --
  ev -> do
    l' <- L.handleListEventVi L.handleListEvent ev (txsOf w)
    continue (w & field @"txsOf" .~ l')
mainEvent _ w _ = continue w

from :: SimpleFold Wallet TX
from = field @"txsOf" . to L.listSelectedElement . _Just . _2

-- | A missing primitive from `brick`.
handleFormEventL :: Eq n => Lens' s (Form x e n) -> s -> BrickEvent n e -> EventM n s
handleFormEventL l s ev = do
  f' <- handleFormEvent ev (s ^. l)
  pure (s & l .~ f')

--------------------------------------------------------------------------------
-- Signing Server

type API = "v1" :> "sign" :> ReqBody '[JSON] SigningRequest :> Post '[JSON] SigningResponse

data SigningRequest = SigningRequest
  { _signingRequest_code     :: Text
  , _signingRequest_data     :: Maybe Object
  , _signingRequest_nonce    :: Maybe Text
  , _signingRequest_chainId  :: Maybe Text
  , _signingRequest_gasLimit :: Maybe P.GasLimit
  , _signingRequest_ttl      :: Maybe P.TTLSeconds
  , _signingRequest_sender   :: Maybe Text }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data SigningResponse = SigningResponse
  { _signingResponse_body    :: P.Command Text
  , _signingResponse_chainId :: Text }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

server :: Server API
server = sign
  where
    sign :: SigningRequest -> Handler SigningResponse
    sign _ = throwM $ err503 { errBody = "NERD!" }

signApp :: Application
signApp = serve (Proxy @API) server
