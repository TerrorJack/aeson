module Data.Aeson.Compat
  (
    fromStrict
  ) where

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

fromStrict :: S.ByteString -> L.ByteString
fromStrict = L.fromChunks . (:[])
