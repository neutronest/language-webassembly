module Language.WebAssembly.Kernel (
    IntUnOp (..)
  , IntBinOp (..)
  , IntTestOp (..)
  , IntRelOp (..)
  , IntCvtOp (..)
  , FloatUnOp (..)
  , FloatBinOp (..)
  , FloatTestOp
  , FloatRelOp (..)
  , FloatCvtOp (..)
  , UnOp
  , BinOp
  , TestOp
  , RelOp
  , CvtOp
) where

import Language.WebAssembly.Values

data IntUnOp = Clz | Ctz | Popcnt

data IntBinOp
    = IntAdd | IntSub | IntMul | DivS | DivU | RemS | RemU
    | And | Or | Xor | Shl | ShrS | ShrU | Rotl | Rotr

data IntTestOp = Eqz

data IntRelOp
    = IntEq | IntNe | LtS | LtU | LeS | LeU | GtS | GtU | GeS | GeU

data IntCvtOp
    = ExtendSInt32 | ExtendUInt32 | WrapInt64
    | TruncSFloat32 | TruncUFloat32 | TruncSFloat64 | TruncUFloat64
    | ReinterpretFloat

data FloatUnOp
    = Neg | Abs | Ceil | Floor | Trunc | Nearest | Sqrt

data FloatBinOp
    = FloatAdd | FloatSub | FloatMul | Div
    | Min | Max | CopySign

data FloatTestOp

data FloatRelOp
    = FloatEq | FloatNe | Lt | Le | Gt | Ge

data FloatCvtOp
    = ConvertSInt32 | ConvertUInt32 | ConvertSInt64 | ConvertUInt64
    | PromoteFloat32 | DemoteFloat64
    | ReinterpretInt

type UnOp = Op IntUnOp FloatUnOp
type BinOp = Op IntBinOp FloatBinOp
type TestOp = Op IntTestOp FloatTestOp
type RelOp = Op IntRelOp FloatRelOp
type CvtOp = Op IntCvtOp FloatCvtOp
