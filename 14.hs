import Zero
import Util
import Data.List.Extra (sortBy,splitOn,stripInfix)
import Data.Maybe
import Data.Tuple
import Data.Bifunctor
import Data.List
import Data.Ord
import Control.Monad

data Ch = Ch { name :: String } 
   deriving (Eq,Show)

data Product = P { ch :: Ch , qt :: Int }

instance Show Product where
   show (P c q) = show q ++ (name c)

data Reaction = R { produce :: Product , ingredients :: [Product] }

instance Show Reaction where
   show (R p i) = show p ++ show i ++ "\n"

data Tree a = Root [Tree a] | Tree a [Tree a] 

-- parse input into list of R { produce , ingredients }
parse :: String -> [Reaction]
parse = map f . lines
   where
   f :: String -> Reaction
   f s = R (prod p) (map prod $ splitOn ", " s')
      where
      Just (s',p) = stripInfix " => " s
      prod = uncurry P . bimap Ch read . swap . fromJust . stripInfix " "

main :: IO ()
main = do
   input <- parse <$> readFile "14.txt"
   print $ search (Ch "FUEL") input
   print . part 3 $ search (Ch "FUEL") input

-- from a list of recipes
-- list every possible combination of obtaining x Ch
part :: Int -> [Reaction] -> [[Reaction]]
part n rs =  echo tree 
--                    ^ ore nedded (find recursively)
   where
   rs' = sortBy (comparing (qt . produce)) rs
   tree :: Tree Reaction
   tree = Root $ go n rs'
      where
      go :: Int -> [Reaction] -> [Tree Reaction]
      go _ [] = []
      go i (x:xs)
         | i > 0 = Tree x (go (i - (qt $ produce x)) (x:xs)) : go i xs
         | otherwise = []
   -- get all root to leaf paths
   echo :: Tree Reaction -> [[Reaction]]
   echo t = go [] t
      where
      go acc (Root []) = [acc]
      go acc (Root ts) = join $ go acc <$> ts
      go acc (Tree x []) = [(x:acc)]
      go acc (Tree x ts) = join $ go (x:acc) <$> ts

search :: Ch -> [Reaction] -> [Reaction]
search _ [] = []
search c (x:xs)
   | P c' _ <- produce x, c' == c = x : search c xs
   | otherwise = search c xs

--derive :: Chemical -> Reaction -> Product
-- recursively find production in reaction until Chemical is found

--cost :: [Reaction] -> 
--cost [] _ = error "fuel recipe absent"
--cost rs c = map calc recipes
--   where
--   recipes = filter (== c . fst) rs
--   calc :: Reaction -> 
