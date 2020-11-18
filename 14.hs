{-# LANGUAGE BangPatterns #-}

import Zero.Zero
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
   print "ok"
   print solution

solution :: [Integer]
solution = (2^) <$> [0..63]

