{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}

-- |
-- Module: Holding.Chainweb
-- Copyright: Copyright © 2019 Colin Woodbury
-- License: BSD3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
-- Stability: experimental
--
-- This module is a reimplementation of certain Chainweb types / functionality,
-- to avoid depending on it in its entirety.
--

module Holding.Chainweb
  ( -- * ChainwebVersion
    ChainwebVersion(..)
  , chainIds
  ) where

import qualified Pact.Types.ChainId as P
import           RIO
import qualified RIO.Text as T
import           Servant.API (ToHttpApiData(..))

---

-- | A newtype over `Text` allows BOH to be resilient to bumps in Testnet
-- version numbers.
newtype ChainwebVersion = ChainwebVersion { chainwebVersionToText :: Text }

instance ToHttpApiData ChainwebVersion where
  toUrlPiece = chainwebVersionToText

chainIds :: ChainwebVersion -> [P.ChainId]
chainIds _ = map (P.ChainId . T.pack . show @Int) [0 .. 9]
