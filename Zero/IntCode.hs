module IntCode (State (..),Signal (..),empty,ini,run,tick,amp,ticks,(?)) where
   
--import Zero
import qualified Data.Map.Lazy as M

type Mem = M.Map Integer Integer

infixr 1 ?
(?) :: Mem -> Integer -> Integer
m ? k = M.findWithDefault 0 (fromInteger k) m

data State = State { memory :: Mem , pointer :: Integer , input :: Integer , output :: Integer , relbase :: Integer }
   deriving Show

data Op = Op { code :: Integer , parameters :: Integer }
   deriving Show

data Signal = Signal { signal :: Integer , state :: State } 
            | Result { signal :: Integer , state :: State }

instance Show Signal where
   show (Signal sig st) = "Σ " ++ show sig ++ " " ++ show (output st)
   show (Result sig st) = "Ρ " ++ show sig ++ " " ++ show (output st)

-- empty memory
empty :: State
empty = State M.empty 0 0 0 0

-- get program from [Integer]
ini :: Integer -> [Integer] -> State
ini i l = empty { memory = M.fromList $ zip [0..] l , input = i }

-- run until halt
run :: State -> Signal
run μ
   | Result _ _ <- sig = sig
   | otherwise = run $ state sig
   where
   sig = tick μ

-- (!! n) . ticks
-- take n . ticks
ticks :: Signal -> [Signal]
ticks s
   | Signal sig μ <- s = s : ticks (amp sig μ)
   | Result _ _ <- s = []

-- run amp (i/o)        -> (Integer,State)
amp :: Integer -> State -> Signal
amp s μ = tick $ μ { input = s }

-- step until output or halt
tick :: State -> Signal
tick μ@(State m p _ o _)
   | Op 4 _ <- op = Signal (output μ') μ' -- # "signal " ++ show (head $ output μ')
   | Op 99 _ <- op = Result o μ -- # "HALT " ++ show (head o)
   | otherwise = tick μ' 
   where
   n = m ? p
   op = Op (mod n 100) (div n 100)
   μ' = step op $ μ

step :: Op -> State -> State
step op μ@(State m p i _ r) 
-- | False # show (op,o) = undefined
   -- sum
   | 1 <- c = μ { memory = set 3 (get 1 + get 2) , pointer = p + 4 }
   -- mul
   | 2 <- c = μ { memory = set 3 (get 1 * get 2) , pointer = p + 4 }
   -- read (write in)
   | 3 <- c = μ { memory = set 1 i , pointer = p + 2 } 
   -- write out
   | 4 <- c = μ { pointer = p + 2 , output = get 1 }
   -- jump
   | 5 <- c , get 1 /= 0 = μ { pointer = get 2 }
   | 5 <- c = μ { pointer = p + 3 }
   | 6 <- c , get 1 == 0 = μ { pointer = get 2 }
   | 6 <- c = μ { pointer = p + 3 }
   -- conditional insert
   | 7 <- c = μ { memory = set 3 (toInteger . fromEnum $ get 1 < get 2) , pointer = p + 4 }
   | 8 <- c = μ { memory = set 3 (toInteger . fromEnum $ get 1 == get 2) , pointer = p + 4 }
   -- relative base adjustment
   | 9 <- c = μ { relbase = get 1 + r , pointer = p + 2 } 
   -- error
   | otherwise = error $ "invalid instruction code: " ++ show c
   where
   c = code op
   -- parameter modes
   get t
      -- position mode
      | mode == 0 = m ? arg 
      -- immediate mode 
      | mode == 1 = arg
      -- relative mode
      | mode == 2 = m ? arg + r
      | otherwise = error $ "invalid get mode: " ++ show mode
      where
      mode = op <?> t
      arg = m ? p + t 
   set t x = M.insert target x m
      where
      mode = op <?> t
      arg = m ? p + t
      target
         | mode == 0 = arg
         | mode == 2 = arg + r
         | otherwise = error $ "invalid set mode: " ++ show mode

-- parameter i
(<?>) :: Op -> Integer -> Integer
op <?> i = (parameters op `mod` 10^i) `div` 10^(i-1) 

