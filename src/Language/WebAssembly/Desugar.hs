module Language.WebAssembly.Desugar (
    desugar
) where

import Data.Vector
import Language.WebAssembly.Annotated
import qualified Language.WebAssembly.AST as AST
import qualified Language.WebAssembly.Kernel as Kernel

desugarAnnotated :: (b -> b') -> Annotated b a -> Annotated b' a
desugarAnnotated f (Annotated b a) = Annotated (f b) a

desugarExprs :: Vector (AST.Expr a) -> Kernel.Expr a
desugarExprs = undefined -- todo

desugarFunc :: AST.Func a -> Kernel.Func a
desugarFunc = desugarAnnotated $ \f -> Kernel.Func' {
    Kernel.ftype = AST.ftype f
  , Kernel.locals = AST.locals f
  , Kernel.body = desugarExprs $ AST.body f
}

desugarModule :: AST.Module a -> Kernel.Module a
desugarModule = desugarAnnotated $ \m -> Kernel.Module' {
    Kernel.memory = AST.memory m
  , Kernel.types = AST.types m
  , Kernel.funcs = desugarFunc <$> AST.funcs m
  , Kernel.start = AST.start m
  , Kernel.imports = AST.imports m
  , Kernel.exports = AST.exports m
  , Kernel.table = AST.table m
}

desugar :: AST.Module a -> Kernel.Module a
desugar = desugarModule
