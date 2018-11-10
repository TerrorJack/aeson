{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Data.Aeson.Functions
-- Copyright:   (c) 2011-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable

module Data.Aeson.Internal.Functions
    (
      mapKeyVal
    ) where

import Prelude.Compat

import qualified Data.Map.Strict as M

-- | Transform the keys and values of a 'M.Map'.
mapKeyVal :: (Eq k2, Ord k2) => (k1 -> k2) -> (v1 -> v2)
          -> M.Map k1 v1 -> M.Map k2 v2
mapKeyVal fk kv = M.foldrWithKey (\k v -> M.insert (fk k) (kv v)) M.empty
{-# INLINE mapKeyVal #-}
