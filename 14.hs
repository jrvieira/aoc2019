{-# LANGUAGE PartialTypeSignatures #-}

import Zero.Zero
import Data.List.Split
import Data.Foldable
import Data.Maybe
import Data.Map as M
import Control.Arrow

parse :: String -> M.Map Ch (Int,[(Ch,Int)])
parse s = M.fromList ls
   where
   ls = fmap parsel $ splitOn "\n" s
   parsel :: String -> (Ch,(Int,[(Ch,Int)]))
   parsel s = (ch,(n,is')) -- # show (ch,n)
      where
      [is,p] = splitOn "=>" s  # s
      (ch,n) = parse' p
      is' = fmap parse' $ splitOn ", " is
      parse' s = let ~[a,b] = words s in (b,read a)

test :: IO ()
test = do
   input <- fmap parse . init . splitOn "\n\n" <$> readFile "14.test"
   teqt "part 1" [31,165,13312,180697,2210736] $ fmap (flip η "ORE") input

main :: IO ()
main = do
   input <- parse . init <$> readFile "14.txt"
   print $ η input "ORE"

type Ch = String

(><) :: Int -> Int -> Int
_ >< 0 = 1
p >< n = succ $ div (pred n) p

η :: M.Map Ch (Int,[(Ch,Int)]) -> Ch -> Int
η m ch = prod >< (sum $ fmap (uncurry (*) . (η m . fst &&& snd)) is) -- # show (prod,is)
   where
   is :: [(Ch,Int)]
   is = M.toList $ M.map fromJust $ M.filter isJust $ M.map (fmap snd . find ((== ch) . fst) . snd) $ m
   prod = maybe 1 fst $ m M.!? ch

