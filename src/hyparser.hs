module Hyparser where
import Text.ParserCombinators.Parsec hiding (spaces)

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

parserAtom :: Parser LispVal
parserAtom = do
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



