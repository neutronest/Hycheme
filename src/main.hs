module Main where
import Control.Monad
import Control.Monad.Error
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

import Hyparser
import LispVal
import Eval


readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

-- change our main function to read an expression, 
-- evaluate it, convert it to a string, and print it out.
main :: IO()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)