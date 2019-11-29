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
  , vText, verP
    -- * ChainId
  , ChainId(..)
  , chainIdToText, chainIdFromText
  , chainIds
  ) where

import           RIO
import qualified RIO.HashSet as HS
import qualified RIO.Text as T
import           Servant.API

---

data ChainwebVersion = Testnet | Mainnet

instance ToHttpApiData ChainwebVersion where
  toUrlPiece = vText

chainIds :: ChainwebVersion -> HashSet ChainId
chainIds Testnet = HS.fromList [0 .. 9]
chainIds Mainnet = HS.fromList [0 .. 9]

vText :: ChainwebVersion -> Text
vText Testnet = "testnet03"
vText Mainnet = "mainnet01"

verP :: String -> Maybe ChainwebVersion
verP "testnet03" = Just Testnet
verP "mainnet01" = Just Mainnet
verP _           = Nothing

newtype ChainId = ChainId { chainIdInt :: Word }
  deriving stock (Eq, Ord)
  deriving newtype (Num, Enum, Hashable)

instance ToHttpApiData ChainId where
  toUrlPiece = chainIdToText

chainIdToText :: ChainId -> Text
chainIdToText (ChainId n) = T.pack $ show n

chainIdFromText :: Text -> Maybe ChainId
chainIdFromText = fmap ChainId . readMaybe . T.unpack
