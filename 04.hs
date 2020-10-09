import Data.Char
import Data.List
import Control.Arrow

main :: IO ()
main = print $ (length &&& part_2) codes

range = map show [236666..699999] -- non descending, at least 2 adjacent

codes :: [String]
codes = filter valid range

valid :: String -> Bool
valid s = go False (head s) (tail s)
   where
   go :: Bool -> Char -> String -> Bool
   go a _ [] = a
   go a prev (x:xs)
      | digitToInt prev > digitToInt x = False
      | otherwise = go (a || prev == x) x xs

part_2 :: [String] -> Int
part_2 cs = go cs 0
   where
   go [] = id
   go (x:xs)
     | elem 2 . map length . group $ x = go xs . succ
     | otherwise = go xs
