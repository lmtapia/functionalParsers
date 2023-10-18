module Parsers.Combinators where
import Parsers.Elementary
-- Parser combinators

infixr 4 <|>   -- choice combinator
infixl 6 <*>   -- sequential combinator
infixl 7 <$>   -- combinator to aply semantic functions


(<|>)  :: Parser s a -> Parser s a -> Parser s a
(p <|> q) xs  = p xs ++ q xs


(<*>)  :: Parser s (b -> a) -> Parser s b -> Parser s a
(p <*> q) input = [ (f x, rest)
                  | (f  , qinput)  <- p input
                  , (  x, rest)    <- q qinput 
                  ]

(<$>)  :: (a -> b ) ->  Parser s a -> Parser s b
(f <$> p) xs = [(f y, ys)
               |(  y, ys) <- p xs]
