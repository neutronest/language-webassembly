module Language.WebAssembly.Values (
    Op (..)
  , Value
) where

data Op i f
    = Int32 !i | Int64 !i | Float32 !f | Float64 !f

data Value -- todo
