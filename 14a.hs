{-# LANGUAGE BangPatterns #-}

{- This is abandoned
 - but not forgotten.
 - Look at calq + part
 - for interesting modeling
 -}

import Zero.Zero
import Zero.Shut
import Data.List.Extra (splitOn,minimumOn,stripInfix)
import Data.Maybe
import Data.Tuple
import Data.Bifunctor
import Data.List
import Data.Ord
import Data.Function
import Control.Monad
import Control.Arrow
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype Ch = Ch { name :: String } 
   deriving (Ord,Eq,Show)

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
   tests <- map parse . splitOn "\n\n" <$> readFile "14.test"
   --teqt "tests" [31,165,13312,180697,2210736,0] $ solve "FUEL" 1 "ORE" <$> tests
   print $ solve "STAINLESS" 1 "ORE" (last tests)

solve :: String -> Int -> String -> [Reaction] -> Int
solve p n r u = calq (Ch r) u Map.empty (P (Ch p) n)

calq :: Ch -> [Reaction] -> Map Ch Int -> Product -> Int
-- r basic resource chemical
-- u universe of all reactions
-- l leftover
-- p target product
calq r u l p = minimum . map resource . part (qt p - i) $ search (ch p) u  # "calq" ++ show p ++ show l
   where
   i = fromMaybe 0 $ Map.lookup (ch p) l
   resource :: [Reaction] -> Int
   resource rs = sum $ map go rs
      where
      go (R dbg ps)
         | False  # "calculating resource for:" ++ show dbg ++ show ps = undefined
         | [P c q] <- ps , c == r = q  # "found r " ++ show ps
         | otherwise = sum $ map (calq r u l') ps  # "not found " ++ show (sum(map (qt.produce)rs),qt p) ++ " " ++ show l'
      l' = Map.insert (ch p) (sum (map (qt . produce) rs) - qt p) l -- # show (sum (map (qt.produce) rs) - qt p,p,rs)

-- from a list of recipes
-- list every possible combination of obtaining x Ch
part :: Int -> [Reaction] -> [[Reaction]]
part n rs = echo tree
   where
   rs' = sortOn (qt . produce) rs -- sortBy (on compare (qt . produce)) rs -- sortBy (comparing (qt . produce)) rs
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
      go acc (Root ts) = ts >>= go acc -- join $ go acc <$> ts
      go acc (Tree x []) = [x:acc]
      go acc (Tree x ts) = ts >>= go (x : acc) -- join $ go (x:acc) <$> ts

search :: Ch -> [Reaction] -> [Reaction]
search _ [] = []
search c (x:xs)
   | P c' _ <- produce x, c' == c = x : search c xs
   | otherwise = search c xs

