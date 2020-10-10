import Data.List.Split
import Control.Arrow
import Data.List
import Data.Ord

main :: IO ()
main = do
   readFile "08.txt" >>= print . uncurry (*) . (count '1' &&& count '2') . minimumBy (comparing $ count '0') . init . chunksOf (25*6)
   readFile "08.txt" >>= print . map (head . dropWhile (== '2')) . transpose . init . chunksOf (25*6)
-- readFile "08.txt" >>= putStrLn . concat . intersperse "\n" . chunksOf 25 . map (paint . head . dropWhile (== '2')) . transpose . init . chunksOf (25*6)

count x = length . filter (== x)

-- paint '1' = 'X'
-- paint '0' = ' '
