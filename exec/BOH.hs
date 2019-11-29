{-# LANGUAGE LambdaCase #-}

module Main ( main ) where

import           BOH.CLI (Args(..), Env(..), env, pArgs)
import           BOH.Signing (signApp)
import           BOH.UI
import           Brick (customMain)
import           Brick.Focus (focusRing)
import qualified Brick.Widgets.List as L
import           Data.Aeson hiding (Options)
import qualified Graphics.Vty as V
import           Holding
import           Holding.Chainweb
import qualified Network.Wai.Handler.Warp as W
import           Options.Applicative hiding (command, footer, header, str)
import           RIO hiding (Handler, local, on)
import           RIO.Partial (fromJust)
import           Text.Printf (printf)

---

main :: IO ()
main = execParser opts >>= env >>= \case
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
      (replForm e . REPL (ChainId 0) Local (TxData Null) . fromJust $ code "(+ 1 1)") []
      Nothing

    opts :: ParserInfo Args
    opts = info (pArgs <**> helper)
        (fullDesc <> progDesc "The Bag of Holding: A Chainweb Wallet")
