import IntCode
import Data.List.Split
import Control.Arrow

main :: IO ()
main = do
   print . (== reverse [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]) . output . state . run $ ini [] [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
   print . (== 16) . length . show . signal . run $ ini [] [1102,34915192,34915192,7,4,7,99,0]
   print . (== 1125899906842624) . signal . run $ ini [] [104,1125899906842624,99]
   readFile "09.txt" >>= print . run . ini [1] . map read . splitOn ","
   readFile "09.txt" >>= print . run . ini [2] . map read . splitOn ","
