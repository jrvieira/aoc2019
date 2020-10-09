module IntCode where
   
import Data.List.Split
import qualified Data.IntMap.Lazy as M
--import Debug.Trace

--infix 0 #
--(#) = flip trace

data Memory = Memory { memory :: M.IntMap Int , pointer :: Int , input :: [Int] , output :: [Int]}
   deriving Show
   
data Op = Op { code :: Int , parameters :: Int }
   deriving Show

-- get program from file
ingest :: String -> IO (M.IntMap Int)
ingest f = do
   x <- readFile f
   pure . memo . map read . splitOn "," $ x

-- get program from [Int]
memo :: [Int] -> M.IntMap Int
memo = M.fromList . zip [0..]

ini :: M.IntMap Int -> Memory
ini mem = Memory mem 0 [] []

run :: Memory -> Int
run mem@(Memory m p _ o)
   | n == 99 = head o
   | otherwise = run $ step op mem
   where
   op = Op (mod n 100) (div n 100)
   n = m M.! p

data Signal = Output Memory Int | Return Int

run' :: Memory -> Int -> Signal
run' mem s = go $ mem { input = input mem ++ [s] }
   where
   go :: Memory -> Signal
   go mem@(Memory m p i o)
      | Op 4 _ <- op = Output mem' . head $ output mem'
      | Op 99 _ <- op = Return . head $ output mem
      | otherwise = go mem' 
      where
      n = m M.! p
      op = Op (mod n 100) (div n 100)
      mem' = step op $ mem

step :: Op -> Memory -> Memory
step op mem@(Memory m p is o) 
   -- sum
   | 1 <- c = Memory (M.insert (m M.! (p + 3)) (at 1 + at 2) m) (p + 4) is o
   -- mul
   | 2 <- c = Memory (M.insert (m M.! (p + 3)) (at 1 * at 2) m) (p + 4) is o
   -- read in (write input to pos)
   | 3 <- c = Memory (M.insert (m M.! (p + 1)) i m) (p + 2) ii o
   -- write out
   | 4 <- c = Memory m (p + 2) is (at 1 : o)
   -- jump
   | 5 <- c , at 1 /= 0 = Memory m (at 2) is o
   | 5 <- c = Memory m (p + 3) is o
   | 6 <- c , at 1 == 0 = Memory m (at 2) is o
   | 6 <- c = Memory m (p + 3) is o 
   -- conditional insert
   | 7 <- c = Memory (M.insert (m M.! (p + 3)) (bint $ at 1 < at 2) m) (p + 4) is o
   | 8 <- c = Memory (M.insert (m M.! (p + 3)) (bint $ at 1 == at 2) m) (p + 4) is o
   -- error
   | otherwise = error $ "invalid instruction code: " ++ show c
   where
   c = code op
   i = head is
   ii = tail is
   at x
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
