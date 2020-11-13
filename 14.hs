import Zero.Zero
import Data.List.Extra (splitOn,stripInfix)
import Data.Maybe
import Data.Tuple
import Data.Bifunctor
import Data.List
import Data.Ord
import Data.Function
import Control.Monad

newtype Ch = Ch { name :: String } 
   deriving (Eq,Show)

data Product = P { ch :: Ch , qt :: Int }

instance Show Product where
   show (P c q) = " " ++ show q ++ name c ++ " "

data Reaction = R { produce :: Product , ingredients :: [Product] }

instance Show Reaction where
   show (R p i) = show p ++ show i ++ " "

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
   itest <- parse <$> readFile "14.test"
-- print $ search (Ch "RKBM") input
-- print $ search (Ch "ORE") input
-- print $ search (Ch "FUEL") input
   print $ calc (Ch "ORE") input (P (Ch "QMVN") 1)

calc :: Ch -> [Reaction] -> Product -> Int
-- r basic resource chemical
-- u universe of all reactions
-- p target product
calc r u p = minimum . map resource . part (qt p) $ search (ch p) u  # show (minimumBy (on compare resource) . part (qt p) $ search (ch p) u)
   where
   resource :: [Reaction] -> Int
   resource = sum . map go
      where
      go (R _ ps)
         | [P c q] <- ps , c == r = q  # "found " ++ show ps
         | otherwise = sum $ map (calc r u) ps  # "calqing " ++ show ps

-- from a list of recipes
-- list every possible combination of obtaining x Ch
part :: Int -> [Reaction] -> [[Reaction]]
part n rs = echo tree 
   where
   rs' = sortBy (comparing (qt . produce)) rs
   tree :: Tree Reaction
   tree = Root $ go n rs'
      where
      go :: Int -> [Reaction] -> [Tree Reaction]
      go _ [] = []
      go i (x:xs)
         | i > 0 = Tree x (go (i - qt  (produce x)) (x:xs)) : go i xs
         | otherwise = []
   -- get all root to leaf paths
   echo :: Tree Reaction -> [[Reaction]]
   echo = go [] 
      where
      go acc (Root []) = [acc]
      go acc (Root ts) = join $ go acc <$> ts
      go acc (Tree x []) = [x:acc]
      go acc (Tree x ts) = join $ go (x:acc) <$> ts

search :: Ch -> [Reaction] -> [Reaction]
search _ [] = []
search c (x:xs)
   | P c' _ <- produce x, c' == c = x : search c xs
   | otherwise = search c xs

