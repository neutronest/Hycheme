module Eval where

import Control.Monad.Except
import LispVal
import Hyerror

-- val@(String ￼ ) matches against any LispVal 
-- that’s a string and then binds val to the whole LispVal
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
                                             result <- eval pred
                                             case result of
                                                Bool False -> eval alt
                                                otherwise -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecongnized special form " badForm


--  lookup : built-in function, looks up a key (its first argument) in a list of pairs
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecongnized primitives funcion args" func) 
    ($ args) 
    (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
                ("=", numBoolBinop (==)),
                ("<", numBoolBinop (<)),
                (">", numBoolBinop (>)),
                ("/=", numBoolBinop (/=)),
                ("<=", numBoolBinop (<=)),
                (">=", numBoolBinop (>=)),
                ("&&", boolBoolBinop (&&)),
                ("||", boolBoolBinop (||)),
                ("string=?", strBoolBinop(==)),
                ("string?", strBoolBinop(>)),
                ("stirng?<=", strBoolBinop(<=)),
                ("string?>=", strBoolBinop(>=)),
                ("+", numberBinop (+)),
                ("-", numberBinop (-)),
                ("*", numberBinop (*)),
                ("/", numberBinop (div)),
                ("mod", numberBinop div),
                ("quotient", numberBinop quot),
                ("remainder", numberBinop rem)]

numberBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numberBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numberBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
    then throwError $ NumArgs 2 args
    else do
         left <- unpacker $ args !! 0
         right <- unpacker $ args !! 1
         return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool


unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                            if null parsed
                                then throwError $ TypeMismatch " number " $ String n
                                else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool
