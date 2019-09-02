module Main ( main ) where

import Data.Aeson (decode, encode)
import Holding
import RIO
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

---

main :: IO ()
main = defaultMain suites

suites :: TestTree
suites = testGroup "Tests"
  [ testGroup "Unit Tests"
    [ testCase "keysJsonIso" keysJsonIso
    ]
  ]

keysJsonIso :: Assertion
keysJsonIso = do
  mks <- decode . encode <$> keys
  assertBool "keysJsonIso failed" $ isJust (mks :: Maybe Keys)
