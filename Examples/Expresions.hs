
module Expresions where
import Parsers.Elementary
import Parsers.Combinators
import Prelude hiding ((<*>),(<|>),(<$>))
import Data.Char
import Numbers

data Expr = Con Int
          | Var String
          | Fun String [Expr]
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr 
          | Expr :/: Expr 
          deriving Show

identifier = many1 (satisfy isAlpha)

fact :: Parser Char Expr
fact =  Con <$> integer
      <|> Fun <$> identifier <*> parenthesised (commaList expr)
      <|> Var <$> identifier
      <|> parenthesised expr


term :: Parser Char Expr
term = chainr fact
               (    const (:*:) <$> symbol '*'
                <|> const (:/:) <$> symbol '/'
               )

expr :: Parser Char Expr
expr = chainr term
              (    const (:+:) <$> symbol '+'
               <|> const (:-:) <$> symbol '-'
              )


multis = [('*',(:*:)), ('/',(:*:))]  
addis  = [('+',(:+:)), ('-',(:-:))]

-- HINT:  expr = gen (gen fact multis) sums 
expr' :: Parser Char Expr
expr' = foldr gen fact [addis, multis]
