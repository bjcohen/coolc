module Main where

import Control.Monad (when, liftM)
import Directory (doesFileExist)
import System (getArgs)

import Parser.ParsecParser
import Parser.PrettyPrint (pp)

import qualified Data.Map as Map

-- runParserInIO :: IO ()
runParserInIO p s f = return $ runCoolParser p s f

main :: IO ()
main = do argv <- getArgs
          let filename = head argv
          flag <- doesFileExist filename
          when (not flag) (error (filename ++ " does not exist"))
          text <- readFile filename
          (result, st) <- runParserInIO program text filename
          case result of
            Left err -> print err
            Right ast -> mapM_ putStrLn (pp st ast)

