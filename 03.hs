import Data.List
import Data.List.Split
import Data.Ord
import Control.Arrow
import Control.Monad

type Move = (Char,Int)
type Pos = (Int,Int)
type Wire = [Pos]
data Pointer = Pointer { pos :: Pos, path :: Wire } 
   deriving Show

main :: IO ()
main = do
   --readFile "03.txt" >>= print . solve . (head &&& head . tail) . map parse . lines
   readFile "03.txt" >>= print . final . (head &&& head . tail) . map parse . lines

parse :: String -> [Move]
parse = map (head &&& read . tail) . splitOn ","

final :: ([Move],[Move]) -> Int
final ms = minimum $ map signal intersections
   where
   signal :: Pos -> Int
   signal i = sig 0 (fst wires) + sig 0 (snd wires)
      where
      sig n [] = n 
      sig n (p:ps) 
         | p == i = sig 0 ps
         | otherwise = sig (succ n) ps
   intersections = init $ uncurry intersect wires
   wires = join (***) wire $ ms

solve :: ([Move],[Move]) -> Int
--solve = snd . minimumBy (comparing fst) . intersections
solve = minimum {-. map fst-} . intersections
   where
   intersections = init . map (uncurry (+) . join (***) abs {-&&& id-}) . uncurry intersect . join (***) wire

wire :: [Move] -> Wire
wire ms = pos g : path g
   where
   g = go ms (Pointer (0,0) []) 
   go :: [Move] -> Pointer -> Pointer
   go [] = id
   go ((m,n):ms)
      | n > 0 = go ((m,n-1):ms) . (step m)
      | n == 0 = go ms
      | otherwise = error "invalid move"

step :: Char -> Pointer -> Pointer
step m (Pointer p@(x,y) w)
   | 'R' <- m = Pointer (x+1,y) (p:w)
   | 'L' <- m = Pointer (x-1,y) (p:w)
   | 'U' <- m = Pointer (x,y+1) (p:w)
   | 'D' <- m = Pointer (x,y-1) (p:w)
   | otherwise = error "invalid direction"

