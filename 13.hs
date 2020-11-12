import Zero
import IntCode
import Data.List
import Data.List.Split
import Data.Bifunctor
import Control.Monad
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Console.ANSI
import Control.Arrow

main :: IO ()
main = do
   intcode <- ini 0 . fmap read . splitOn "," <$> readFile "13.txt"
   let o = map (fromInteger . signal) . ticks $ amp 0 intcode
   let (_,s) = parse o -- last tick is Result
   print . length . filter (== Block) . map snd $ s
   -- part 2
   intcode <- ini 0 . (2 :) . tail . fmap read . splitOn "," <$> readFile "13.txt"
   let frames = map (draw . (score &&& Map.toList . Map.delete (-1,0) . screen)) $ game intcode
   mapM_ (\(i,t) -> writeFile ("13.io/" ++ show i ++ ".txt") t) $ zip [0..] frames
   pure ()

p :: Signal -> Bool
p s
   | (Signal _ _) <- s = False
   | (Result _ _) <- s = True

type Assoc a = [((Int,Int),a)]
    
data Tile = Empty | Wall | Block | Paddle | Ball 
   deriving (Eq,Enum,Show)

parse :: [Integer] -> (Int,Assoc Tile)
parse = go 0 . map fromInteger
   where
   go :: Int -> [Int] -> (Int,Assoc Tile)
   go s [] = (s,[])
   go _ (-1:0:s:rest) = go s rest
   go s (x:y:t:rest) = (((x,y),toEnum t) :) <$> go s rest
   go _ e = error $ "invalid program: rest " ++ show e

-- draw

draw :: (Int,Assoc Tile) -> String
draw (_,[]) = "START[]"
draw (s,m) = unlines . (show s :) . chunksOf (succ maxx) . map char $ go 0 0
   where
   go :: Int -> Int -> [Tile]
   go x y
      | y > maxy = []
      | x > maxx = go 0 (succ y)
      | otherwise = tile x y : go (succ x) y
   (maxx,maxy) = join bimap maximum . unzip $ map fst m
   tile x y = fromMaybe Empty (lookup (x,y) m)

char :: Tile -> Char
char c
   | Empty <- c = ' '
   | Wall <- c = 'x'
   | Block <- c = 'o'
   | Paddle <- c = '-'
   | Ball <- c = '.'

-- part 2

data Stat a = Running a | Done a
   deriving Show

data Game = Game { score :: Int , screen :: Map (Int,Int) Tile , ram :: State }

instance (Show Game) where
   show (Game sco scr ram) = "γ { sco: " ++ show sco ++ " , scr: " ++ show (Map.size scr) ++ "} "

game :: State -> [Game]
game s = go (Game 0 Map.empty s)
   where
   go :: Game -> [Game]
   go γ
      | Running g <- play γ = go g -- # show (play γ)
      | Done g <- play γ = g : go g # show (play γ)

fx :: Map (Int,Int) Tile -> Tile -> Maybe Int
fx s t
   | Map.null f = Nothing
   | otherwise = Just . fst . fst . head $ Map.toList f
   where
   f = Map.filter (== t) s

trick :: State -> (((Int,Int),Int),State)
trick s
   | Signal s0 st0 <- tick s , Signal s1 st1 <- tick st0 , Signal s2 st2 <- tick st1 = (((fromInteger s0,fromInteger s1),fromInteger s2),st2)
   | Signal s0 st0 <- tick s , Signal s1 st1 <- tick st0 , Result s2 st2 <- tick st1 = error "GAME OVER !"
   | otherwise = error "out of phase output"

play :: Game -> Stat Game
play g@(Game sco scr sta) = stat $ Game sco' scr' sta' { input = j } 
   where
   stat
      | Map.size scr' == Map.size scr && Map.null (Map.filter (== Block) scr') = Done
      | otherwise = Running 
   ((k,t),sta') = trick sta
   (sco',scr') 
      | fst k == -1 = (t,scr)
      | otherwise = (sco,Map.insert k (toEnum t) scr)
   j
      | bx == Nothing || px == Nothing = 0
      | bx < px = -1
      | bx > px = 1
      | otherwise = 0
      where
      bx = fx scr' Ball 
      px = fx scr' Paddle 

