module Main where

import Lexer
import Control.Monad (when)
import System (getArgs)
import Directory (doesFileExist)

main :: IO ()
main = do
  argv <- getArgs
  let filename = head argv
  flag <- doesFileExist filename
  when (not flag) (error (filename ++ " does not exist"))
  t <- readFile filename
  putStrLn $ "#name " ++ show filename
  case (scanner t) of
    Left st -> error st
    Right ls -> mapM_ print ls
