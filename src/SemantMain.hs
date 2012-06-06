module Main where

import Lexer
import Parser
import Semant
import Control.Monad (when)
import System (getArgs)
import Directory (doesFileExist)
import Control.Monad.State (evalStateT)
import Parser.PrettyPrint (pp)

main :: IO ()
main = do
  argv <- getArgs
  let filename = head argv
  flag <- doesFileExist filename
  when (not flag) (error (filename ++ " does not exist"))
  t <- readFile filename
  toks <- (return $ case (scanner t) of Left er -> error er; Right toks -> toks)
  let (st, p) = stGet $ evalStateT cool (0, filename, toks, 0)
  let annotatedparse = case semant st p of Left er -> error $ ppSemantErrors er; Right ap -> ap
  mapM_ putStrLn (pp st annotatedparse)
