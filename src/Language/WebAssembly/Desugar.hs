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
    ftype = AST.ftype f
  , locals = AST.locals f
  , body = desugarExprs $ AST.body f
}

desugarModule :: AST.Module a -> Kernel.Module a
desugarModule = desugarAnnotated $ \m -> Kernel.Module' {
    memory = AST.memory m
  , types = AST.types m
  , funcs = desugarFunc <$> AST.funcs m
  , start = AST.start m
  , imports = AST.imports m
  , exports = AST.exports m
  , table = AST.table m
}

desugar :: AST.Module a -> Kernel.Module a
desugar = desugarModule
