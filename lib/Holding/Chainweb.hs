{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

import           Pact.Types.ChainId
import           RIO
import qualified RIO.Text as T
import           Servant.API

---

newtype ChainwebVersion = ChainwebVersion { chainwebVersionToText :: Text }

instance ToHttpApiData ChainwebVersion where
  toUrlPiece = chainwebVersionToText

chainIds :: ChainwebVersion -> [ChainId]
chainIds _ = map (ChainId . T.pack . show) [0 :: Int .. 9]
