module Language.WebAssembly.Values (
    Op (..)
) where

import Language.WebAssembly.Types

data Op i32 i64 f32 f64
    = Int32 !i32
    | Int64 !i64
    | Float32 !f32
    | Float64 !f64
