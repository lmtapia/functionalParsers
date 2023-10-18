module Parsers.Elementary where

type Parser symbol result = [symbol] -> [(result, [symbol])]

-- Elementary parsers
symbol :: Eq s => s -> Parser s s
symbol a (b:bs) = if b == a then [(b,bs)] else []
symbol a []     = []

satisfy :: (s -> Bool) -> Parser s s
satisfy p []                 = []
satisfy p (x:xs) | p x       = [(x,xs)]
                 | otherwise = []

token :: Eq s => [s] -> Parser s [s]
token k xs | k == take n xs = [(k, drop n xs)]
           | otherwise      = []
    where n = length k

failp :: Parser s a
failp xs = []

succeed :: a -> Parser s a
succeed r xs = [ (r, xs)]
