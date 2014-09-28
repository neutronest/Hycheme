module Hyparser where
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$%&|+-*/:<=?>@^_~#"
