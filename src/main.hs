module Main where
import Control.Monad
import Control.Monad.Error
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

import Hyparser
import LispVal
import Eval
import Hyerror


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

-- change our main function to read an expression, 
-- evaluate it, convert it to a string, and print it out.
main :: IO()
main = do
       args <- getArgs
       evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
       putStrLn $ extractValue $ trapError evaled