module Parentheses where
import Parsers.Elementary
import Parsers.Combinators 
import Prelude hiding ((<|>), (<*>), (<$>))

data Parentheses1 = Match1 Char Parentheses1 Char Parentheses1
                  | Empty1
data Parentheses = Match Parentheses Parentheses
                  | Empty
                  deriving Show

open  = symbol '('
close = symbol ')'

parens :: Parser Char Parentheses
parens =  f <$> open <*> parens <*> close <*> parens
      <|> succeed Empty
  where f o a c b = Match a b

nesting :: Parser Char Int
nesting =  f <$> open <*> nesting <*> close <*> nesting
       <|> succeed 0
  where f o a c b = max (a+1) b
