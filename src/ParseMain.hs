module Main where

import Lexer
import Parser
import Control.Monad (when)
import System (getArgs)
import Directory (doesFileExist)
import Control.Monad.State (evalStateT)

main :: IO ()
main = do
  argv <- getArgs
  let filename = head argv
  flag <- doesFileExist filename
  when (not flag) (error (filename ++ " does not exist"))
  t <- readFile filename
  toks <- (return $ case (scanner t) of Left er -> error er; Right toks -> toks)
  let (st, p) = stGet $ evalStateT (cool toks) 0
  print st
  print p
