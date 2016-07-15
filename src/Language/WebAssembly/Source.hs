module Language.WebAssembly.Source (
    Pos (..)
  , Region (..)
  , Phrase (..)
) where

data Pos = Pos {
    file :: !FilePath
  , line :: !Int
  , column :: !Int
}

data Region = Region {
    left :: !Pos
  , right :: !Pos
}

data Phrase a = Phrase {
    at :: !Region
  , it :: !a
}
