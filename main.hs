import Data.Array
import System.Environment (getArgs)
import System.IO (readFile)

import Data.IntMap 

data Tree a = Leaf | Node a (Tree a) (Tree a)
data Person = Name String

instance Show a => Show (Tree a) where
  show Leaf = ""
  show (Node a Leaf Leaf) = show a
  show (Node a b c) = show a ++ show b ++ show c

instance Show Person where 
  show (Name n) = n

token :: [String] -> [Person]
token [] = []
token (c:cs) = Name c : token cs

parse :: [Person] -> Int -> Tree Person
parse [] _ = Leaf 
parse t@(x:xs) n = Node x leftN rightN
  where left = 2*n+1
        right = 2*(n+1)
        leftN = parse (drop (left-n) t) left
        rightN = parse (drop (right-n) t) right

parseTokens s = parse (token s) 0
-- Using Array        
tokenArray :: [String] -> Array Int String
tokenArray t@(x:xs) = array (0, len) (zip [0..len] t)
  where len = length t - 1


data DAG a = Nil | Level a [DAG a] [DAG a] deriving Show

data Node' a = Node' { nodeValue :: a, links :: [Int] }
data Graph' a = Graph (IntMap (Node' a))

--instance Show a => Show (Graph' a) where
  
someDag = 
  fromList 
    [ (1, Node' { nodeValue = 1, links = [2] })
    , (2, Node' { nodeValue = 1, links = [3] })
    , (3, Node' { nodeValue = 1, links = [] })
    ]

data Child = Child { age :: Int, name :: String } deriving Show

{- look up Aeson
main = do
  [filepath] <- getArgs
  content <- readFile filepath
  putStrLn $ show (parseTokens content)
-}
