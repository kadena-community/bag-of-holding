{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           BOH.CLI (Command(..), Env(..), env, pCommand)
import           BOH.Poll (listenOnKey)
import           BOH.Signing (signApp)
import           BOH.UI
import           Brick (customMain)
import           Brick.Focus (focusRing)
import qualified Brick.Widgets.List as L
import           Data.Aeson hiding (Options)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Graphics.Vty as V
import           Holding
import qualified Network.Wai.Handler.Warp as W
import           Options.Applicative hiding (command, footer, header, str)
import qualified Pact.Types.ChainId as P
import           RIO hiding (Handler, local, on)
import qualified RIO.ByteString.Lazy as BL
import           RIO.Partial (fromJust)
import           Text.Printf (printf)

---

main :: IO ()
main = execParser opts >>= \case
  KeyGen -> keys >>= BL.putStrLn . encodePretty
  Listen arg -> listenOnKey arg
  UI arg -> env arg >>= \case
    Left err -> printf "%s\n" err
    Right e -> do
      initialVty <- buildVty
      race_ (W.run 9467 $ signApp (chanOf e) (respOf e))
            (void $ customMain initialVty buildVty (Just $ chanOf e) (app e) (initial e))
  where
    buildVty :: IO V.Vty
    buildVty = V.mkVty V.defaultConfig

    initial :: Env -> Wallet
    initial e = Wallet
      (L.list TXList mempty 1)
      ""
      (focusRing [minBound ..])
      (replForm e . REPL (P.ChainId "0") Local (TxData Null) . fromJust $ code "(+ 1 1)")
      (transferForm e $ Trans (P.ChainId "0") (Receiver $ Account "") 0 False)
      []
      Nothing

    opts :: ParserInfo Command
    opts = info (pCommand <**> helper)
        (fullDesc <> progDesc "The Bag of Holding: A Chainweb Wallet")
