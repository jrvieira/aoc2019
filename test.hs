import Zero.Zero
import Control.Monad

main :: IO ()
main = print $ part 9 [4,3,2,1]

data Tree = Tree Int [Tree]

part :: Int -> [Int] -> [[Int]]
part n ls = join . map echo $ tree n ls

tree :: Int -> [Int] -> [Tree]
tree _ [] = []
tree i (x:xs) 
   | i > 0 = Tree x (tree (i-x) (x:xs)) : tree i xs
   | otherwise = []

echo :: Tree -> [[Int]]
echo t = go [] t
   where
   go acc (Tree x []) = [(x:acc)]
   go acc (Tree x ts) = join $ go (x:acc) <$> ts
   
