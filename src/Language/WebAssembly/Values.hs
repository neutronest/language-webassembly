module Language.WebAssembly.Values (
    Op (..)
  , Value
  , Func
) where

import Data.Int
import Data.Vector

data Op i32 i64 f32 f64
    = Int32 !i32 | Int64 !i64 | Float32 !f32 | Float64 !f64

type Value = Op Int32 Int64 Float Double

type Func = Vector Value -> Maybe Value
