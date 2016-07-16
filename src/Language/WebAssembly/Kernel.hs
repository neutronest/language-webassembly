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
  , MemOp (..)
  , ExtOp (..)
  , WrapOp (..)
  , HostOp (..)
  , Var
  , Literal
  , Expr
  , Expr' (..)
  , Func
  , Func' (..)
  , Memory
  , Memory' (..)
  , Segment
  , Export
  , Export' (..)
) where

import Data.Text
import Data.Vector
import Language.WebAssembly.Annotated
import Language.WebAssembly.Memory
import Language.WebAssembly.Types
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

data MemOp = MemOp {
    ty :: !ValueType
  , offset :: !Offset
  , align :: !Int
}

data ExtOp = ExtOp {
    extMemOp :: !MemOp
  , extSz :: !MemSize
  , ext :: !Extension
}

data WrapOp = WrapOp {
    wrapMemOp :: !MemOp
  , wrapSz :: !MemSize
}

data HostOp
    = CurrentMemory
    | GrowMemory

type Var a = Annotated Int a
type Literal a = Annotated Value a
type Expr a = Annotated (Expr' a) a
type Func a = Annotated (Func' a) a
type Memory a = Annotated (Memory' a) a
type Segment a = Annotated Segment' a
type Export a = Annotated (Export' a) a

data Expr' a
    = Nop
    | Unreachable
    | Block !(Vector (Expr a)) !(Expr a)
    | Loop !(Expr a)
    | Break !(Var a) !(Maybe (Expr a))
    | BreakIf !(Var a) !(Maybe (Expr a)) !(Expr a)
    | BreakTable !(Vector (Var a)) !(Var a) !(Maybe (Expr a)) !(Expr a)
    | If !(Expr a) !(Expr a) !(Expr a)
    | Select !(Expr a) !(Expr a) !(Expr a)
    | Call !(Var a) !(Vector (Expr a))
    | CallImport !(Var a) !(Vector (Expr a))
    | CallIndirect !(Var a) !(Expr a) !(Vector (Expr a))
    | GetLocal !(Var a)
    | SetLocal !(Var a) !(Expr a)
    | Load !MemOp !(Expr a)
    | Store !MemOp !(Expr a) !(Expr a)
    | LoadExtend !ExtOp !(Expr a)
    | StoreWrap !WrapOp !(Expr a) !(Expr a)
    | Const !(Literal a)
    | Unary !UnOp !(Expr a)
    | Binary !BinOp !(Expr a) !(Expr a)
    | Test !TestOp !(Expr a)
    | Compare !RelOp !(Expr a) !(Expr a)
    | Convert !CvtOp !(Expr a)
    | Host !HostOp !(Vector (Expr a))

data Func' a = Func' {
    ftype :: !(Var a)
  , locals :: !(Vector ValueType)
  , body :: !(Expr a)
}

data Memory' a = Memory' {
    min :: !Size
  , max :: !Size
  , segments :: !(Vector (Segment a))
}

data Export' a = Export' {
    name :: !Text
  , kind :: !() -- todo
}
