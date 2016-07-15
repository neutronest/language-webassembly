module Language.WebAssembly.Values (
    Op (..)
) where

import Language.WebAssembly.Types

data Op i f
    = Int32 !i | Int64 !i | Float32 !f | Float64 !f
