{-# LANGUAGE PartialTypeSignatures #-}

import Zero.Zero
import Data.List.Split
import Data.Foldable
import Data.Maybe
import Data.Bifunctor
import qualified Data.Map as M
import Control.Arrow

parse :: String -> M.Map Ch (Integer,[(Ch,Integer)])
parse = M.fromList . fmap parsel . splitOn "\n"
   where
   parsel :: String -> (Ch,(Integer,[(Ch,Integer)]))
   parsel s = (ch,(n,is')) -- # show (ch,n)
      where
      [is,p] = splitOn "=>" s -- # s
      (ch,n) = parse' p
      is' = fmap parse' $ splitOn ", " is
      parse' s = let ~[a,b] = words s in (b,read a)

test :: IO ()
test = do
   input <- fmap parse . init . splitOn "\n\n" <$> readFile "14.test"
   teqt "part 1" [31,165,13312,180697,2210736] $ η "ORE" <$> input
   teqt "part 2" [82892753,5586022,460664] $ β 1000000000000 <$> drop 2 input

main :: IO ()
main = do
   input <- parse . init <$> readFile "14.txt"
   print $ η "ORE" input
   print $ β 1000000000000 input

type Ch = String

φ :: Integer -> Integer -> Integer
φ _ 0 = 1
φ p n = succ $ div (pred n) p

-- how much ch is needed to produce the FUEL? (FUEL happens to be always 1)
η :: Ch -> M.Map Ch (Integer,[(Ch,Integer)]) -> Integer
η ch m = φ p $ sum $ fmap (uncurry (*) . (flip η m . fst &&& snd)) is -- # show (prod,is)
   where
   p = maybe 1 fst $ m M.!? ch
   is :: [(Ch,Integer)]
   is = M.toList $ M.map fromJust $ M.filter isJust $ M.map (fmap snd . find ((== ch) . fst) . snd) m

-- part 2

-- how much ore is needed to produce t * FUEL?
μ :: Integer -> M.Map Ch (Integer,[(Ch,Integer)]) -> Integer
μ t = η "ORE" . M.adjust (bimap (t*) (fmap (t*) <$>)) "FUEL"

-- how many FUEL can be produced with i ORE ? (binary search)
β :: Integer -> M.Map Ch (Integer,[(Ch,Integer)]) -> Integer
β i m = go 0 1000000000 1
   where
   go :: Integer -> Integer -> Integer -> Integer
   go lo hi n
      | False  # show n = undefined
      | n == lo || n == hi = lo
      | μ n m < i = go n hi (n + div (hi - n) 2)
      | μ n m > i = go lo n (n - div (n - lo) 2)
      | otherwise = n

