
import Data.List
import Data.Ratio
import Numeric
import System.Environment
import Data.Scientific
import Data.Number.Fixed
import Control.Monad

type P = Fixed Prec500

nums :: [P]
nums = map (\i -> 2 * js i / js2 i) [0,1..]


js :: Int -> P
js i = foldl' (*) 1 $ map fromIntegral [i,i-1..1]


js2 :: Int -> P
js2 i = foldl' (*) 1 $ map fromIntegral [i*2+1,i*2-1..3]

-- rel :: Int -> Double
-- rel n = let r =sum $ take n nums
--         in (fromIntegral $ numerator r) / (fromIntegral $ denominator r)

rel1 :: Int -> P
rel1 n =  sum $ take n nums

-- Rational

groupN :: Int -> [a] -> [[a]]
groupN n [] = []
groupN n ls = a : groupN n b
  where (a,b) = splitAt n ls

main :: IO ()
main = do
  args:_ <- getArgs
  let num = read args
      r = rel1 num
      num' = groupN 5 $ groupN 10 $ drop 2 $ show r
  forM_ num' print

