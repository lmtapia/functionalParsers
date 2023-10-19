module Numbers where
import Parsers.Elementary
import Parsers.Combinators
import Prelude hiding ((<*>),(<|>),(<$>))
import Data.Char

digit :: Parser Char Char
digit = satisfy isDigit

digit2Int :: Parser Char Int
digit2Int = f <$> digit
  where f c = ord c - ord '0' 

decimal :: Parser Char Int 
decimal = foldl1 (\x y -> x*10 + y)  <$> many1 digit2Int