import System.IO
import Debug.Trace

infix 0 #
(#) = flip trace

ingest :: String -> IO [Int]
ingest f = do
   x <- readFile f
   pure . map read . lines $ x

main :: IO ()
main = do
   modules <- ingest "01.txt"
   print . sum . map fuel' $ modules

-- calculate how much fuel a module needs
fuel :: Int -> Int
fuel m = max 0 $ div m 3 - 2

-- recursively account for the fuel itself
fuel' :: Int -> Int
fuel' = go 0
   where
   go :: Int -> Int -> Int
   go acc x
      | fuel x < 1 = acc -- # show (acc,x)
      | otherwise = go (acc + fuel x) (fuel x) -- # show (acc,x)

-- same as:
-- fuel' m = sum . takeWhile (>0) . tail . iterate fuel M
