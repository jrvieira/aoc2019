import IntCode
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.List
import Data.List.Split
import Control.Arrow
import Draw

import Debug.Trace

infix 1 #
--(#) = flip trace
(#) = const

main :: IO ()
main = do
   intcode <- ini 0 . fmap read . splitOn "," <$> readFile "11.txt"
   let r = Robot (0,0) N intcode
   let part1 = panels . hull $ Ship r Map.empty
   print $ Map.size part1 
   print $ Map.lookup (0,0) part1
   draw "11_test" . Map.toList $ Map.map head part1
-- Part II
   let part2 = panels . hull $ Ship r (Map.fromList [((0,0),[True])])
   draw "11" . Map.toList $ Map.map head part2

-- iterate next until halt
hull :: Ship -> Ship
hull sh@(Ship (Robot _ _ b) _)
   | 99 <- op = sh
   | otherwise = hull $ next sh
   where
   n = memory b ? pointer b
   op = mod n 100

next :: Ship -> Ship
next sh = paint sh (enum o0) ~> move (enum o1) ~> update -- # show (o0,o1)
   where
   -- read panel 
   i = (toInteger . fromEnum $ tell sh) -- # "panel is " ++ show i
   -- operate
   (o0,s0) = signal &&& state $ amp i (brain $ robot sh)
   (o1,s1) = signal &&& state $ tick s0
   -- update robot memory
   update r = r { brain = s1 }
   -- helper function
   enum :: Enum a => Integer -> a
   enum = toEnum . fromInteger

data Turn = L | R 
   deriving (Enum,Show)
data Direction = N | E | S | W 
   deriving (Enum,Show)
data Robot = Robot { pos :: (Int,Int) , dir :: Direction , brain :: State }
data Ship = Ship { robot :: Robot , panels :: Map (Int,Int) [Bool] }

-- read color from pos
tell :: Ship -> Bool
tell (Ship r ps) = head $ Map.findWithDefault [False] (pos r) ps  # "tell " ++ show (head $ Map.findWithDefault [False] (pos r) ps)

-- change state panels
paint :: Ship -> Bool -> Ship
paint s@(Ship r ps) b = s { panels = Map.insertWith (++) (pos r) [b] ps }  # "paint " ++ show b

-- change state robot
infixl 1 ~>
(~>) :: Ship -> (Robot -> Robot) -> Ship
s@(Ship r _) ~> f = s { robot = f r }

move :: Turn -> Robot -> Robot
move t r@(Robot p d _) = r { pos = p' , dir = d' }  # "move " ++ show d' ++ " to " ++ show p'
   where
   d' = turn t d
   p' = walk d' p
   turn L N = W
   turn L d = pred d
   turn R W = N
   turn R d = succ d
   walk N (x,y) = (x  ,y-1)
   walk W (x,y) = (x-1,y  )
   walk E (x,y) = (x+1,y  )
   walk S (x,y) = (x  ,y+1)
