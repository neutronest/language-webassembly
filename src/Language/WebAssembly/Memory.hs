module Language.WebAssembly.Memory (
    Address
  , Size
  , Offset
  , MemSize (..)
  , Extension (..)
  , Segment' (..)
) where

import Data.Int
import qualified Data.Text as T

type Address = Int64

type Size = Address

type Offset = Address

data MemSize = Mem8 | Mem16 | Mem32

data Extension = SX | ZX

data Segment' = Segment' {
    addr :: !Address
  , segmentData :: !T.Text
}
