module Main where
import Control.Monad
import Control.Monad.Error
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

import Hyparser

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"


main :: IO()
main =  do args <- getArgs
           putStrLn (readExpr (args !! 0 ))