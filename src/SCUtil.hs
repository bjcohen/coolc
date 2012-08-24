{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}

module SCUtil where

import qualified Data.Map as Map
import Data.Monoid
import Control.Monad.Fix
import Control.Monad.Writer.Class

-- Combining Table Monad

data TableM k v a = TableM (Map.Map k v) a
                   
instance Ord k => Monad (TableM k v) where
  (TableM st a) >>= f =
    TableM (Map.union st st') b
      where TableM st' b = f a
  return k =
    TableM Map.empty k
    
tAdd :: k -> v -> a -> TableM k v a
tAdd k v = TableM (Map.singleton k v)

tGetT :: TableM k v a -> Map.Map k v
tGetT (TableM t a) = t

tGetV :: TableM k v a -> a
tGetV (TableM t a) = a

-- Symbols

newtype Symbol = Symbol String deriving (Eq,Ord)

instance Show Symbol where
  show (Symbol s) = s

class Symbolable a where
  newSymbol :: a -> Symbol

instance Show a => Symbolable a where
  newSymbol = Symbol . show
  
combineSymbol :: Symbol -> Symbol -> Symbol
combineSymbol (Symbol s1) (Symbol s2) = Symbol $ s1 ++ s2
