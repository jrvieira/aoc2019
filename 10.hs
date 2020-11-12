import Data.Map.Lazy (Map)
import qualified Data.Map as Map
import Data.List
import Data.Ord
import Control.Arrow
import Control.Monad
import Data.Bifunctor
import Data.Function

import Debug.Trace

infixr 0 #
(#) = flip trace

test = "\
  \.##.#\n\
  \#..#.\n\
  \##...\n\
  \#....\n\
  \.#.#.\n"

main :: IO ()
main = do
   space <- parse <$> readFile "10.txt"
   let asteroids = Map.keys . Map.filter (== Astr) $ space
   let (o,v) = part1 asteroids
   let t = part2 o $ asteroids
   print $ unwords ["station",show o]
   print $ unwords ["monitoring",show v]
   print $ unwords ["hit #200: ",show t]
-- print . map (partTest (2,2)) $ [1..11]

part1 = maximumBy (comparing snd) . (\m -> map (id &&& length . visibles m) m) 
part2 o = fmap (unrelative o) . target 200 . sortOn (negate . angle . fst) . Map.toList . flip radar o
--partTest o t = fmap (unrelative o) . target t . sortOn (negate . angle . fst) . Map.toList . flip radar o . Map.keys . Map.filter (== Astr) . parse $ test


type Point = (Int,Int)
type Space = Map Point Entity 
data Vector = Vector { dir :: Point , δ :: Int }
   deriving Show
data Entity = None | Astr
   deriving Eq

instance Show Entity where
   show None = "."
   show Astr = "#"

parse :: String -> Space
parse = Map.fromList . go 0 0 
   where
   go _ _ "" = []
   go x y (c:cs)
      | '\n' <- c = go 0 (succ y) cs 
      | '#' <- c = ((x,y),Astr) : go (succ x) y cs
      | '.' <- c = ((x,y),None) : go (succ x) y cs
      | otherwise = error $ "unrecognized character: " ++ show c

radar :: [Point] -> Point -> Map Point [Int] -- Map (x,y) [δ]
radar ts o = Map.map sort . go Map.empty . map (relative o) . filter (/= o) $ ts 
   where
   go m [] = m
   go m (p:ps) = go (Map.insertWith (++) p' [d] m) ps
      where
      p' = join bimap (flip div d) p
      d = uncurry gcd p

radarVector :: [Point] -> Point -> [Vector]
radarVector ps o = map (vectorize . relative o) ps

visibles :: [Point] -> Point -> [Point]
visibles ps o = map fst . Map.toList . foldl' go Map.empty . map (relative o) $ ps
   where
   -- add to map id distance (gcd x y) is lowest for vector
   go :: Map Point Int -> Point -> Map Point Int
   go m (0,0) = m -- ignore origin
   go m (x,y) = Map.insertWith min (div x d,div y d) d m  -- # show m
      where
      d = gcd x y

relative :: Point -> Point -> Point
relative (ox,oy) (x,y) = (x-ox,y-oy)

unrelative :: Point -> Point -> Point
unrelative (ox,oy) (x,y) = (x+ox,y+oy)

vectorize :: Point -> Vector
vectorize p = Vector (join bimap (flip div d) p) d
   where
   d = uncurry gcd p 

unvectorize :: Vector -> Point
unvectorize (Vector p d) = join bimap (*d) p 

-- Part II

angle :: RealFloat a => Point -> a
angle = uncurry atan2 . join bimap realToFrac

target :: Int -> [(Point,[Int])] -> Maybe Point
target _ [] = Nothing
target n ((p,d):ts)
   | n <= 0 = Nothing
   | null d = target n ts
   | n == 1 = Just $ join bimap (* head d) p
   | otherwise = target (pred n) (ts ++ [(p,tail d)])

--
--aligned :: Point -> Point -> Bool
--aligned a b = reduce a == reduce b  # show (reduce a,reduce b)
--   where
--   reduce (x,y) = let f = gcd x y in (div x f,div y f)

