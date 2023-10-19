module Parsers.Combinators where
import Parsers.Elementary
import Prelude hiding ((<*>), (<|>), (<$>))

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

--EBNF parser combinators 
-- combinator for ? sufix 
option     :: Parser s a -> a -> Parser s a
option p d = p <|> succeed d

-- combinator for * sufix
many :: Parser s a -> Parser s [a]
many p =  list <$> p <*> many p
      <|> succeed []

list x xs = x:xs

-- combinator for + sufix
many1    :: Parser s a -> Parser s [a]
many1 p = list <$> p <*> many p


pack :: Parser s o -> Parser s a -> Parser s c -> Parser s a
pack p r q = (\x y z -> y) <$> p <*> r <*> q

listOf     :: Parser s a -> Parser s b -> Parser s [a]
listOf p s = list <$> p <*> many ((\x y -> y) <$> s <*> p)