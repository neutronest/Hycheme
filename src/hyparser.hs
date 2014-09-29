module Hyparser where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import LispVal

symbol :: Parser Char
symbol = oneOf "!$%&|+-*/:<=?>@^_~#"

spaces :: Parser()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString =  do
               --  each line of a do-block must have the same type
               char '"'
               x <- many (noneOf "\"")
               char '"'
               -- apply the String constructor (from our LispVal data type) to turn it into a LispVal
               -- $ is a functions which like the LispValsp function apply
               return $ String x

parseAtom :: Parser LispVal
parseAtom = do
             -- <|> is a Parsec combinator
             -- This tries the first parser, then if it fails, tries the second.
             -- If either succeeds, then it returns the value returned by that parser.
             first <- letter <|> symbol
             rest <- many (letter <|> digit <|> symbol)
             let atom = [first] ++ rest
             return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                otherwise -> Atom atom

parseNumber :: Parser LispVal
-- The function composition operator . 
-- creates a function that applies its right argument 
-- and then passes the result to the left argument
-- liftM example: liftM2 (+) [0,1] [0,2] = [0,2,1,3], just span 
parseNumber = liftM (Number . read) $ many1 digit

-- parsing a series of expressions separated by whitespace 
-- (sepBy parseExpr spaces)
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
                  head <- endBy parseExpr spaces
                  tail <- char '.' >> spaces >> parseExpr
                  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
              char '\''
              x <- parseExpr
              return $ List [Atom "quote", x]


parseExpr :: Parser LispVal
parseExpr = parseAtom 
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
        char '('
        x <- (try parseList) <|> parseDottedList
        char ')'
        return x


