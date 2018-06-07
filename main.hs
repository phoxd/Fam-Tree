import Data.Array

data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show
data Person = Name String deriving Show

token :: [String] -> [Person]
token [] = []
token (x:xs) = Name x : token xs

parse :: [Person] -> Int -> Tree Person
parse [] _ = Nil
parse t@(x:xs) n = Node x leftN rightN
  where left = 2*n+1
        right = 2*(n+1)
        leftN = parse (drop (left-n) t) left
        rightN = parse (drop (right-n) t) right

-- Using Array        
                                                      

tokenArray :: [String] -> Array Int String
tokenArray t@(x:xs) = array (0, len) (zip [0..len] t)
  where len = length t - 1
