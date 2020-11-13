module Zero.Zero where

import Zero.Draw
import Debug.Trace

infix 1 #
(#) :: a -> String -> a
(#) = flip trace

-- count the number of occurrences in a list
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- delete element from list
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete a (x:xs)
   | x == a = xs
   | otherwise = x : xs

-- remove element from assoc by key 
remove :: Eq k => k -> [(k,v)] -> [(k,v)]
remove _ [] = []
remove k (x:xs)
   | k == fst x = remove k xs
   | otherwise = x : remove k xs

-- remove element from assoc by value 
erase :: Eq v => v -> [(k,v)] -> [(k,v)]
erase _ [] = []
erase v (x:xs)
   | v == snd x = erase v xs
   | otherwise = x : erase v xs

-- tortoise and hare cycle finding
floyd :: Eq a => [a] -> Maybe (Integer,a)
floyd [] = Nothing
floyd (a:as) = go 1 1 a as
 where
  go _ _ _ [] = Nothing
  go pow lam x (y:ys)
   | x == y     = Just (lam,x)
   | pow == lam = go (2*pow) 1 y ys
   | otherwise  = go pow (1+lam) x ys

-- simple plot from list of ints
plot :: String -> [Int] -> IO ()
plot f = draw f . go 0 
   where
   go _ [] = []
   go n (x:xs) = [((n,y),True) | y <- [0..x]] ++ go (succ n) xs
