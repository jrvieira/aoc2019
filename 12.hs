{-# LANGUAGE TupleSections #-}

--import Zero
import Data.List
import Data.List.Split

parse :: String -> System
parse = map moon . lines
   where
   moon l = Moon (Τ (read x) (read y) (read z)) (Τ 0 0 0)
      where
      [_,l0] = splitOn "<x=" l 
      [x,l1] = splitOn ", y=" l0
      [y,l2] = splitOn ", z=" l1
      [z,_] = splitOn ">" l2
 
main :: IO ()
main = do
   system <- parse <$> readFile "12.txt"
   let simulation = iterate step $ system
   print . sum . map energy . (!! 1000) $ simulation
-- print . floyd . simulation
-- plot "e" . take 1000 . map ((`div` 100) . sum . map energy) $ simulation
   print . coord τx system

data Τ = Τ { τx :: Int , τy :: Int , τz :: Int }
   deriving (Eq,Show)
data Moon = Moon { position :: Τ , velocity :: Τ }
   deriving (Eq)
type System = [Moon]

instance Show Moon where
   show (Moon p v) = unwords . ("Moon" :) . map show $ [τx p,τy p,τz p,τx v,τy v,τz v]

energy :: Moon -> Int
energy (Moon p v) = potential * kinetic
   where
   potential = sum . map abs $ τs p
   kinetic = sum . map abs $ τs v
   τs (Τ x y z) = [x,y,z] 

step :: System -> System
step s = map (update . g) (rel s) 
   where
   g (a,bs) = foldl' gravity a bs
   update m@(Moon p v) = m { position = Τ (u τx) (u τy) (u τz) }
      where
      u τ = τ p + τ v

gravity :: Moon -> Moon -> Moon
gravity a b = a { velocity = Τ (g τx) (g τy) (g τz) }  
   where
   g τ
      | p a > p b = v a - 1
      | p a < p b = v a + 1
      | otherwise = v a
      where
      v = τ . velocity
      p = τ . position

rel :: System -> [(Moon,[Moon])]
rel s = map isolate s
   where
   isolate x = (x,delete x s)

-- PartII

type SystemC = [MoonC]
data MoonC = MoonC { positionC :: Int , velocityC :: Int }

coord :: System -> (Τ -> Int) -> SystemC
coord s τ = map c s
   where
   c (Moon p v) = MoonC (τ p) (τ v)
