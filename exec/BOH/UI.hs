{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}

module BOH.UI
  ( -- * Types
    Wallet(..)
  , Name(TXList)
  , REPL(..)
  , Endpoint(Local)
    -- * UI
  , app
  , replForm
  ) where

import           BOH.CLI (Env(..))
import           BOH.Signing (SignReq(..), Signed(..))
import           Brick
import qualified Brick.AttrMap as A
import           Brick.Focus
import           Brick.Forms
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import           Chainweb.Utils (fromText, toText)
import           Chainweb.Version
    (ChainId, chainIdFromText, chainIdInt, chainIds)
import           Control.Error.Util (hoistMaybe, hush)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.Aeson (Value(..), decodeStrict', encode)
import           Data.Bitraversable (bitraverse)
import           Data.Generics.Product.Fields (field)
import           Data.Generics.Product.Positions (position)
import           Data.Generics.Sum.Constructors (_Ctor)
import           Data.Generics.Wrapped (_Unwrapped)
import qualified Graphics.Vty as V
import           Holding
import           Lens.Micro
import           Lens.Micro.Extras (preview)
import           RIO hiding (local, on)
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.HashSet as HS
import qualified RIO.List as L
import qualified RIO.Seq as Seq
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL
import           Servant.Client
import           Text.Pretty.Simple (pShowNoColor)
import           Text.Printf (printf)

---

type Listo = L.GenericList Name Seq TX

data TX = TX REPL (Either ClientError From)
  deriving stock (Generic)

data Wallet = Wallet
  { txsOf  :: !Listo
  , logOf  :: !Text
  , focOf  :: !(FocusRing Name)
  , replOf :: !(Form REPL SignReq Name)
  , balsOf :: [(ChainId, Maybe Double)]
  , reqOf  :: Maybe SignReq }
  deriving stock (Generic)

data From = R Receipt | T TXResult deriving (Generic)

data Endpoint = Local | Send deriving (Eq)

data REPL = REPL { rcid :: !ChainId, re :: !Endpoint, dat :: !TxData, rpc :: !PactCode }
  deriving stock (Generic)

-- | Resource names.
data Name = TXList | ReplChain | ReplLocal | ReplSend | ReplData | ReplCode
  | Help | Balances | Sign
  deriving stock (Eq, Ord, Show, Enum, Bounded)

app :: Env -> App Wallet SignReq Name
app e = App { appDraw = draw e
            , appChooseCursor = focusRingCursor focOf
            , appHandleEvent = event e
            , appStartEvent = pure
            , appAttrMap = const $ A.attrMap V.defAttr attrs }
  where
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
      Just Sign     -> [signing]
      _             -> []

    repl :: Widget Name
    repl = C.centerLayer . vLimit 12 . hLimitPercent 50
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
        url = "github.com/kadena-community/bag-of-holding"

    -- | Display account balances on every chain.
    balances :: Widget Name
    balances =
      C.centerLayer . vLimit 15 . hLimitPercent 50 . B.borderWithLabel (txt " Balances ")
      $ vBox (map f $ balsOf w) <=> total <=> padTop (Pad 1) (C.hCenter $ txt "Press any key.")
      where
        total :: Widget w
        total = txt "Total   => " <+> str (show . sum . mapMaybe snd $ balsOf w)

        f :: (ChainId, Maybe Double) -> Widget Name
        f (cid, md) = hBox
          [ txt "Chain ", txt (toText cid), txt " => "
          , str $ maybe "Balance check failed." show md ]

    signing :: Widget Name
    signing = C.centerLayer . vLimit 12 . hLimitPercent 50
      . B.borderWithLabel (txt " Transaction Signing ") $ vBox $
      maybe [] reqContents (reqOf w) <>
      [ C.hCenter . padTop (Pad 1) $ txt "[ESC] [Enter]" ]
      where
        reqContents :: SignReq -> [Widget Name]
        reqContents sr =
          [ txt $ _signReq_code sr
          , padTop (Pad 1) . txt $ "Chain:  " <> fromMaybe "Unknown" (_signReq_chainId sr)
          , txt $ "Sender: " <> fromMaybe "Unknown" (_signReq_sender sr)
          , txt $ "Gas:    " <> maybe "Unknown" tshow (_signReq_gasLimit sr)
          , C.hCenter . padTop (Pad 1) $ txt "Sign this Transaction?" ]

    -- TODO Consider `round` border style.
    left :: Widget Name
    left = hLimitPercent 50 $ B.borderWithLabel (txt " Transaction History ") txs

    txs :: Widget Name
    txs | Seq.null (txsOf w ^. L.listElementsL) = txt "No transactions yet!" <+> fill ' '
        | otherwise = L.renderList (const txListItem) True $ txsOf w

    txListItem :: TX -> Widget Name
    txListItem (TX (REPL cid ep _ pc) eef) = vLimit 1 $ hBox
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
          Nothing         -> txt "Select a Transaction."
          Just (TX _ eef) -> case eef of
            Left err    -> txt . TL.toStrict $ pShowNoColor err
            Right (T t) -> txt . tencode $ txr t
            Right (R r) -> vBox [ txt $ prettyReceipt r
                                , padTop (Pad 1) $ txt "Press [Enter] to query the result." ]

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
  , label "TX Data" @@= editField (field @"dat") ReplData Nothing
    (decodeUtf8Lenient . BL.toStrict . encode) (decodeStrict' . encodeUtf8 . T.unlines) (txt . T.unlines) id
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

event :: Env -> Wallet -> BrickEvent Name SignReq -> EventM Name (Next Wallet)
event e w be = case focusGetCurrent $ focOf w of
  Nothing       -> continue w
  Just TXList   -> mainEvent e w be
  Just Help     -> simplePage w be
  Just Balances -> simplePage w be
  Just Sign     -> signEvent e w be
  Just _        -> replEvent e w be

replEvent :: Env -> Wallet -> BrickEvent Name SignReq -> EventM Name (Next Wallet)
replEvent e w ev@(VtyEvent ve) = case ve of
  -- Close Popup --
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

replEvent _ w (AppEvent sr) = continue $ w & field @"reqOf" ?~ sr
                                           & field @"focOf" %~ focusSetCurrent Sign
replEvent _ w _ = continue w

signEvent :: Env -> Wallet -> BrickEvent Name SignReq -> EventM Name (Next Wallet)
signEvent e w (VtyEvent ve) = case ve of
  -- Close Popup --
  V.EvKey V.KEsc [] -> do
    liftIO . atomically $ putTMVar (respOf e) Nothing
    continue (w & field @"focOf" %~ focusSetCurrent TXList)

  -- Sign the Transaction --
  V.EvKey V.KEnter [] -> do
    liftIO $ for_ codeAndChain $ \(c, cid) -> do
      m  <- meta (accOf e) cid
      -- TODO This should return the data that they gave, not `Null`!
      tx <- view command <$> transaction (verOf e) (TxData Null) c (keysOf e) m
      atomically $ putTMVar (respOf e) (Just . Signed tx $ toText cid)
    continue $ w & field @"focOf" %~ focusSetCurrent TXList
                 & field @"reqOf" .~ Nothing
    where
      codeAndChain :: Maybe (PactCode, ChainId)
      codeAndChain = do
        sr  <- reqOf w
        c   <- code $ _signReq_code sr
        cid <- _signReq_chainId sr >>= fromText
        pure (c, cid)

  _ -> continue w
signEvent _ w _ = continue w

-- | Display some simple page until any key is pressed.
simplePage :: Wallet -> BrickEvent Name SignReq -> EventM Name (Next Wallet)
simplePage w be = case be of
  VtyEvent (V.EvKey _ []) -> continue (w & field @"focOf" %~ focusSetCurrent TXList)
  AppEvent sr -> continue $ w & field @"reqOf" ?~ sr
                              & field @"focOf" %~ focusSetCurrent Sign
  _ -> continue w

mainEvent :: Env -> Wallet -> BrickEvent Name SignReq -> EventM Name (Next Wallet)
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
      rs = map (\cid -> (cid, REPL cid Local (TxData Null) <$> cd)) cs
      ds = preview (_Just . position @2 . _Right . _Ctor @"T" . pactDouble)

  -- Help Window --
  V.EvKey (V.KChar 'h') [] -> continue (w & field @"focOf" %~ focusSetCurrent Help)

  -- History Selection --
  V.EvKey V.KEnter [] -> liftIO f >>= continue . fromMaybe w
    where
      f :: IO (Maybe Wallet)
      f = runMaybeT $ do
        TX (REPL cid _ _ _) eef <- hoistMaybe (w ^? from)
        r <- hoistMaybe (eef ^? _Right . _Ctor @"R")
        t <- MaybeT . fmap (join . hush) $ runClientM (listen (verOf e) cid r) (clenvOf e)
        pure (w & field @"txsOf" %~ L.listModify (set (position @2) (Right $ T t)))

  -- Keyboard Navigation --
  ev -> do
    l' <- L.handleListEventVi L.handleListEvent ev (txsOf w)
    continue (w & field @"txsOf" .~ l')
mainEvent _ w (AppEvent sr) = continue $ w & field @"reqOf" ?~ sr
                                           & field @"focOf" %~ focusSetCurrent Sign
mainEvent _ w _ = continue w

from :: SimpleFold Wallet TX
from = field @"txsOf" . to L.listSelectedElement . _Just . _2

-- | A missing primitive from `brick`.
handleFormEventL :: Eq n => Lens' s (Form x e n) -> s -> BrickEvent n e -> EventM n s
handleFormEventL l s ev = do
  f' <- handleFormEvent ev (s ^. l)
  pure (s & l .~ f')

--------------------------------------------------------------------------------
-- Endpoint Calling

call :: Env -> REPL -> IO TX
call e r@(REPL cid ep td c) = do
  m  <- meta (accOf e) cid
  tx <- transaction (verOf e) td c (keysOf e) m
  TX r <$> runClientM (f tx) (clenvOf e)
  where
    f :: Transaction -> ClientM From
    f = case ep of
          Local -> fmap T . local (verOf e) cid
          Send  -> fmap R . send (verOf e) cid