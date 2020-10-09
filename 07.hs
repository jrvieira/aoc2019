import IntCode
import Prelude
import Control.Arrow
import Data.List
import Data.Ord
import Debug.Trace

infix 0 #
(#) = flip trace

-- test cases:

test0 = memo [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
rslt0 = 43210
phsq0 = [4,3,2,1,0]

test1 = memo [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
rslt1 = 54321
phsq1 = [0,1,2,3,4]

test2 = memo [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
rslt2 = 65210
phsq2 = [1,0,4,3,2]

test3 = memo [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
rslt3 = 139629729
phsq3 = [9,8,7,6,5]

test4 = memo [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
rslt4 = 18216
phsq4 = [9,7,8,5,6]

--

acs = memo [3,8,1001,8,10,8,105,1,0,0,21,34,59,68,89,102,183,264,345,426,99999,3,9,102,5,9,9,1001,9,5,9,4,9,99,3,9,101,3,9,9,1002,9,5,9,101,5,9,9,1002,9,3,9,1001,9,5,9,4,9,99,3,9,101,5,9,9,4,9,99,3,9,102,4,9,9,101,3,9,9,102,5,9,9,101,4,9,9,4,9,99,3,9,1002,9,5,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99]

main :: IO ()
main = do
   print . (== (phsq0,rslt0)) $ maxThrustAlt test0
   print . (== (phsq1,rslt1)) $ maxThrustAlt test1
   print . (== (phsq2,rslt2)) $ maxThrustAlt test2
   print $ maxThrustAlt acs
   print . maximumBy (comparing snd) . map (id &&& thrust) $ permutations [0..4]
   -- Part II
   print . (== (phsq3,rslt3)) . maximumBy (comparing snd) . map (id &&& rthrust (ini test3)) $ permutations [5..9]
   print . (== (phsq4,rslt4)) . maximumBy (comparing snd) . map (id &&& rthrust (ini test4)) $ permutations [5..9]
   print . maximumBy (comparing snd) . map (id &&& rthrust (ini acs)) $ permutations [5..9]

-- alternative, monolith function
maxThrustAlt m = maximumBy (comparing snd) . map (id &&& (\phsq -> foldl' (\s p -> run $ Memory m 0 [p,s] []) 0 phsq)) $ permutations [0..4]

thrust :: [Int] -> Int
thrust phaseSequence = foldl' amp 0 phaseSequence

amp :: Int -> Int -> Int
amp signal phase = run $ Memory acs 0 [phase,signal] []

-- Part II

rthrust :: Memory -> [Int] -> Int
rthrust mem = feed . (id &&& const 0) . map (\i -> mem { input = [i] }) 

feed :: ([Memory],Int) -> Int
feed ([],s) = s
feed (m:ms,s)
   | Output m' s' <- run' m s = feed (ms++[m'],s') -- # "..."
   | Return s' <- run' m s = feed (ms,s') -- # "amp halted" ++ show (pointer m,input m,output m)

