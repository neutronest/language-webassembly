module Language.WebAssembly.Memory (
    Address
  , Size
  , Offset
  , MemSize (..)
  , Extension (..)
) where

import Data.Int

type Address = Int64

type Size = Address

type Offset = Address

data MemSize = Mem8 | Mem16 | Mem32

data Extension = SX | ZX
