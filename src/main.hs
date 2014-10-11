module Main where
import Control.Monad
import Control.Monad.Error
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except
import System.IO hiding (try)

import Hyparser
import LispVal
import Eval
import Hyerror

flushStr :: String -> IO()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m()) -> m()
until_ pred prompt action = do
                            result <- prompt
                            if pred result
                                then return ()
                                else action result >> until_ pred prompt action


runRepl :: IO()
runRepl = until_ (== "quit") (readPrompt "Mycheme>>> ") evalAndPrint


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

-- change our main function to read an expression, 
-- evaluate it, convert it to a string, and print it out.
main :: IO()
main = do
       args <- getArgs
       case length args of
        0 -> runRepl
        1 -> evalAndPrint $ args !! 0
        otherwise -> putStrLn "Program takes only 0 OR 1 agrument"