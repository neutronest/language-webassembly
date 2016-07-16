module Language.WebAssembly.Annotated (
    Annotated (..)
) where

data Annotated b a = Annotated {
    base :: !b
  , annotation :: !a
}
