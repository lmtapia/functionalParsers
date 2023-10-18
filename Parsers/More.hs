module Parsers.More where
import Parsers.Elementary

digit :: Parser Char Char
digit = satisfy isDigit


epsilon :: Parser s ()
epsilon xs = [((), xs)]

