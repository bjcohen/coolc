module Semant where

import Syntax
import qualified Data.Map as Map

type SymbolTable = Map.Map Symbol Type
type StringTable = Map.Map Syntax.Symbol String

ppSemantErrors :: [String] -> String
ppSemantErrors = (>>= id)

semant :: SyntaxTerm a => StringTable -> a -> Either [String] a
semant st term = Left ["Unimplemented"]

  -- First check inheritance graph
  -- Second check other semantic conditions