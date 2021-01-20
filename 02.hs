import Debug.Trace
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M

infix 1 #
(#) = flip trace

ingest :: String -> IO Memory
ingest f = do
   x <- readFile f
   pure . M.fromList . zip [0..] . map read . splitOn "," $ x

main :: IO ()
main = do
   memory <- ingest "02.txt"
   print $ M.lookup 0 <$> run (set [Op 1 12,Op 2 2] memory)
   print $ reveng memory 19690720

type Memory = M.Map Int Int
data Op = Op { address :: Int , value :: Int }
   deriving Show

op :: Op -> Memory -> Memory
op (Op addr val) = M.insert addr val 

set :: [Op] -> Memory -> Memory
set os mem = foldr op mem os

run :: Memory -> Either String Memory
run = go 0
   where
   go :: Int -> Memory -> Either String Memory
   go i mem
      | Just 1 <- code , check 3 , (Just n1,Just n2) <- (a1,a2) = go next $ op (Op addr (n1 + n2)) mem -- # show ("+",code,a1,a2,addr,mem)
      | Just 2 <- code , check 3 , (Just n1,Just n2) <- (a1,a2) = go next $ op (Op addr (n1 * n2)) mem -- # show ("*",code,a1,a2,addr,mem) 
      | Just 99 <- code = Right mem -- # show "~"
      | Nothing <- code = Left (show i ++ "not in " ++ show (length mem))
      | otherwise = Left "invalid instruction code"
      where
      check :: Int -> Bool
      check n = all (/= Nothing) $ map a [0..n]
      a :: Int -> Maybe Int
      a n = M.lookup (i + n) mem
      code = a 0
      a1 = flip M.lookup mem =<< a 1
      a2 = flip M.lookup mem =<< a 2
      addr = fromJust $ a 3
      next = i + 4
   

-- PART 2

-- noun = Op 1 x
-- verb = Op 2 y

-- all possible [noun,verb] combinations
poss :: Memory -> [[Op]]
poss mem = map (uncurry (\x y -> [Op 1 x,Op 2 y])) $ (,) <$> [0..99] <*> [0..99]

-- reverse engeneer which Input produces n
reveng :: Memory -> Int -> [[Op]]
reveng mem n = go [] (poss mem)
   where
   go :: [[Op]] -> [[Op]] -> [[Op]]
   go acc [] = acc
   go acc (i:is)
      | (M.lookup 0 <$> run (set i mem)) == Right (Just n) = go (i:acc) is
      | otherwise = go acc is
      
