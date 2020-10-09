import Data.Tuple
import qualified Data.Map as M
import Data.Maybe
import Data.List

import Debug.Trace

infix 0 #
(#) = const -- flip trace

-- format: (sat,bod)
parse :: [String] -> M.Map String [String]
parse = go M.empty . map orbit
   where

   go :: M.Map String [String] -> [(String,String)] -> M.Map String [String]
   go map [] = map
   go map ((v,k):xs) = go (M.insertWith (++) k [v] map) xs

   orbit :: String -> (String,String)
   orbit "" = error "invalid orbit"
   orbit (x:xs)
      | ')' <- x = (xs,"")
      | otherwise = (x:) <$> orbit xs

main :: IO ()
main = do
   print $ nodes . tree . parse $ ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L"]
   print $ distance "I" "K" . tree . parse $ ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L"]
   orbitmap <- readFile "06.txt"
   print $ nodes . tree . parse . lines $ orbitmap 
   print $ (subtract 2) . distance "YOU" "SAN" . tree . parse . lines $ orbitmap
   
data Tree = Node { body :: String , branches :: [Tree] } deriving Show

-- make a tree from a list of orbits
tree :: M.Map String [String] -> Tree
tree m = go "COM"
   where
   go b = Node b (map go satellites)
      where
      satellites = M.findWithDefault [] b m 

-- get total
nodes :: Tree -> Int
nodes = go 0 
    where
    go d = (d +) . sum . map (go (succ d)) . branches

distance :: String -> String -> Tree -> Int
distance a b t = l -< a + l -< b
   where
   l = lca a b t
   x -< y = depth y - depth x  # show (depth y,depth x)
   depth :: String -> Int
   depth x = fromMaybe 0 $ go t
      where
      go n
         | body n == x = Just 0
         | null $ branches n = Nothing
         | otherwise = fmap succ . getJust $ map go (branches n)
         where
         getJust :: [Maybe Int] -> Maybe Int 
         getJust [] = Nothing
         getJust (x:xs)
            | isJust x = x
            | otherwise = getJust xs

lca :: String -> String -> Tree -> String
lca a b t = body $ go t (branches t)
   where
   go n [] = n
   go n (x:xs)
      | both x = go x (branches x)
      | otherwise = go n xs
      where
      both x = go x (False,False) == (True,True)
         where
         go :: Tree -> (Bool,Bool) -> (Bool,Bool)
         go x (xa,xb)
            | False  # show (xa && xb) ++ body x = undefined
            | body x == a = foldr go (True,xb) (branches x)
            | body x == b = foldr go (xa,True) (branches x)
            | otherwise = foldr go (xa,xb) (branches x)

-- triangular number (sum of consecutive nats) (useless)
tri n = n*(n+1) `div` 2
