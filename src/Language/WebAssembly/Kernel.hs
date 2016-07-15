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

import qualified Data.Text as T
import Data.Vector
import Language.WebAssembly.Memory
import Language.WebAssembly.Source
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

type Var = Phrase Int
type Literal = Phrase Value
type Expr = Phrase Expr'
type Func = Phrase Func'
type Memory = Phrase Memory'
type Segment = Phrase Segment'
type Export = Phrase Export'

data Expr'
    = Nop
    | Unreachable
    | Block !(Vector Expr) !Expr
    | Loop !Expr
    | Break !Var !(Maybe Expr)
    | BreakIf !Var !(Maybe Expr) !Expr
    | BreakTable !(Vector Var) !Var !(Maybe Expr) !Expr
    | If !Expr !Expr !Expr
    | Select !Expr !Expr !Expr
    | Call !Var !(Vector Expr)
    | CallImport !Var !(Vector Expr)
    | CallIndirect !Var !Expr !(Vector Expr)
    | GetLocal !Var
    | SetLocal !Var !Expr
    | Load !MemOp !Expr
    | Store !MemOp !Expr !Expr
    | LoadExtend !ExtOp !Expr
    | StoreWrap !WrapOp !Expr !Expr
    | Const !Literal
    | Unary !UnOp !Expr
    | Binary !BinOp !Expr !Expr
    | Test !TestOp !Expr
    | Compare !RelOp !Expr !Expr
    | Convert !CvtOp !Expr
    | Host !HostOp !(Vector Expr)

data Func' = Func' {
    ftype :: !Var
  , locals :: !(Vector ValueType)
  , body :: !Expr
}

data Memory' = Memory' {
    min :: !Size
  , max :: !Size
  , segments :: !(Vector Segment)
}

data Export' = Export' {
    name :: !T.Text
  , kind :: !() -- todo
}
