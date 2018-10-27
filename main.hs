import Data.Array
import System.Environment (getArgs)
import System.IO (readFile)
import qualified Data.IntMap as IntMap

data Node a = Node { nodeValue :: a, links :: [Int] } deriving Show
data Graph a = IntMap (Node a) 

--instance Show a => Show (Graph' a) where
  
someDag = 
  IntMap.fromList 
    [ (1, Node { nodeValue = 1, links = [2] })
    , (2, Node { nodeValue = 1, links = [3] })
    , (3, Node { nodeValue = 1, links = [] })
    ]

data Child = Child { age :: Int, name :: String } deriving Show

{- look up Aeson
main = do
  [filepath] <- getArgs
  content <- readFile filepath
  putStrLn $ show (parseTokens content)
-}



-- Using binary tree
-- implicitly father on left and mother on right side
data Ancestry a = Empty | Person a (Ancestry a) (Ancestry a)
  deriving Show

data Chunk a = Nil | Instance a deriving Show

tokenize [] = []
tokenize (' ':xs) = tokenize xs
tokenize ('\n':xs) = tokenize xs
tokenize ('0':xs) = Nil : tokenize xs
tokenize t = Instance name : tokenize rest
  where (name, rest) = span (/= ' ') t

isInstance Nil = False 
isInstance _ = True
   
construct s = go s 0
  where
    go (Nil:_) _ = Empty
    go [] _ = Empty
    go t@(Instance a:xs) depth =
          Person a (go paternal depth') (go maternal depth')
          where 
            paternal = drop (2^depth) t
            maternal = drop (2^depth + 1) t
            depth' = depth + 1
  


isLeaf (Person a Empty Empty) = True
isLeaf _ = False

serializeDepth :: Ancestry String -> String
serializeDepth (Person a f m) = a ++ " " ++ serializeDepth f ++ " " ++ serializeDepth m
serializeDepth Empty = "0"

--serializeBreadth :: Ancestry String -> String
serializeBreadth tree = go [tree]
  where go [] = []
        go xs = map (\(Person a _ _) -> a) xs ++ go (concat $ map leftAndRight xs)
        leftAndRight (Person _ Empty Empty) = []
        leftAndRight (Person _ Empty b) = b:[]
        leftAndRight (Person _ a Empty) = a:[]
        leftAndRight (Person _ a b) = [a,b]
