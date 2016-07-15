module Language.WebAssembly.Types (
    ValueType (..)
  , ExprType (..)
  , FuncType (..)
) where

import Data.Vector

data ValueType
    = Int32Type
    | Int64Type
    | Float32Type
    | Float64Type

newtype ExprType = ExprType (Maybe ValueType)

data FuncType
    = FuncType
    { ins :: !(Vector ValueType)
    , out :: !ExprType
    }
