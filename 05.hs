import Debug.Trace

import Data.List.Split
import qualified Data.IntMap.Lazy as M

infix 0 #
(#) = const 
{-
(#) = flip trace
   -}

data Memory = Memory { memory :: M.IntMap Int , pointer :: Int , input :: Int , output :: [Int]}
   deriving Show
data Op = Op { code :: Int , parameters :: Int }
   deriving Show

ingest :: String -> IO (M.IntMap Int)
ingest f = do
   x <- readFile f
   pure . M.fromList . zip [0..] . map read . splitOn "," $ x

main :: IO ()
main = do
   m <- ingest "05.txt"
   print . run $ Memory m 0 1 [0]
   print . run $ Memory m 0 5 [0]
{-
   let m2 = M.fromList . zip [0..] $ [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
   print . (== 0) . run $ Memory m2 0 0 [0]
   print . (== 1) . run $ Memory m2 0 1 [0]
   print . (== 1) . run $ Memory m2 0 2 [0]
   let m3 = M.fromList . zip [0..] $ [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] 
   print . (== 0) . run $ Memory m3 0 0 [0]
   print . (== 1) . run $ Memory m3 0 1 [0]
   print . (== 1) . run $ Memory m3 0 2 [0]
   let m4 = M.fromList . zip [0..] $ [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
   print . (== 999) . run $ Memory m4 0 7 [0]
   print . (== 1000) . run $ Memory m4 0 8 [0]
   print . (== 1001) . run $ Memory m4 0 9 [0]
   putStrLn "--"
   -}

run :: Memory -> Int
run mem@(Memory m p _ _)
   | n == 99 = diagnostic mem  # "-- done! --"
   | otherwise = run $ step op mem
--   | otherwise = diagnostic mem + 100000  # show (M.toList m)
   where
   op = Op (mod n 100) (div n 100)
   n = m M.! p
   diagnostic = head . output

step :: Op -> Memory -> Memory
step op mem@(Memory m p i o) 
   | False # show (op,memory mem M.! p,memory mem M.! (p+1),memory mem M.! (p+2)) = undefined
   | 1 <- c = Memory (M.insert (m M.! (p + 3)) (at 1 + at 2) m) (p + 4) i o  -- # "op: " ++ show (c,at 1,at 2,at 3)
   | 2 <- c = Memory (M.insert (m M.! (p + 3)) (at 1 * at 2) m) (p + 4) i o  -- # "op: " ++ show (c,at 1,at 2,at 3)
   | 3 <- c = Memory (M.insert (m M.! (p + 1)) i m) (p + 2) i o  -- # "op: " ++ show (c,at 1,i)
   | 4 <- c = Memory m (p + 2) i (at 1 : o)  # "op: " ++ show (c,at 1)
   | 5 <- c , at 1 /= 0 = Memory m (at 2) i o  -- # "op: " ++ show (c,at 1,at 2)
   | 5 <- c = Memory m (p + 3) i o  -- # "op: " ++ show (c)
   | 6 <- c , at 1 == 0 = Memory m (at 2) i o  -- # "op: " ++ show (c,at 1,at 2)
   | 6 <- c = Memory m (p + 3) i o  -- # "op: " ++ show (c)
   | 7 <- c = Memory (M.insert (m M.! (p + 3)) (bint $ at 1 < at 2) m) (p + 4) i o  -- # "op: " ++ show (c,at 1,at 2)
   | 8 <- c = Memory (M.insert (m M.! (p + 3)) (bint $ at 1 == at 2) m) (p + 4) i o  -- # "op: " ++ show (c,at 1,at 2)
   | otherwise = error $ "invalid instruction code: " ++ show c
   where
   c = code op
   at x
--      | False # "at " ++ show (x,mode,arg) = undefined
      | mode == 0 = m M.! arg 
      | mode == 1 = arg
      | otherwise = error $ "invalid mode: " ++ show mode
      where
      mode = op <!> x
      arg = m M.! (p + x) 
   bint True = 1
   bint False = 0

-- parameter i
(<!>) :: Op -> Int -> Int
op <!> i = (parameters op `mod` 10^i) `div` 10^(i-1) 

