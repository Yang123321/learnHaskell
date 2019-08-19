{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where
import qualified T.TO as T
import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import Data.Text (Text)
import Data.Binary (Binary)
import qualified Data.Binary as B
import Text.Pretty.Simple
import qualified Data.ByteString.Lazy as BL

type Name = String
type Age = Int
type Address = String

data Person =
  Person Name Age Address
  deriving (Show, Eq, Read)

data Sex
  = B
  | G
  deriving (Show, Ord, Eq, Read, Generic, ToJSON, FromJSON, Binary)

data Person1
  = Person1
      { name :: String
      , age :: Int
      , address :: String
      }
  | Person2
      { name :: String
      , sex :: Sex
      , p :: Person1
      }
  | Leafasdjfosaidfj String
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, Binary)

data Tree a
  = Leaf a
  | Node a [Tree a] --(Tree a) (Tree a)
  | Node1 a (K a) (Tree a)

data K a
  = LF
  | FK1 (Tree a)

data W
  = X1
  | X2
  | X3
  | X4
  | X5
  | X6
  | X7
  deriving (Show, Eq, Ord)
tw = X1

-- ADT + *
--  take 10 $ sort [a,b,c,d]

-- Void |  + 
-- ()       * 
data School = School String String
data FuZhang
  = XJ Int
  | XYK String



f = XJ (100)
f1 = XYK "asdfsadfsadfs"

ss = School "fsadf" "adfsad"



data Nt = Nt String ()  -- ~ String
-- (1,2,3,4)  (a,b,c,d)
-- (1,2,3,4,a,b,c,d)
-- ((1,()), ~ 1
-- )
--k  1 + 2 * 3 + 4
-- + *
-- 0 1
-- (1,a) (1,b)
list = [1, 2, 3, 4, 5] :: [Int]

n = zip [0..] list
-- take 3 [1,2,3,4,5]

a = [1,22,3,4,5] :: [Double]
data TP
  = TD Double
  | TI Int

tp = [TD 1.232, TI 3]

t1o :: Int -> Int -> Int
t1o a b = a + b

tfo s = "adfsadf" ++ s
t2o i = map (\a -> a) $ tfo (show (t1o 2 i))

t11 = filter (\t -> (== 10) $ snd t) $ zip [0 ..] [1 .. 100]
t12 = filter ((== 10) . fst) $ zip [0 ..] [1 .. 100]

showTP :: TP -> String
showTP (TD d) = "this is a Double"
showTP (TI i ) = "this is a  Int"

owl  = (.) $ (.) $ (.) . (.)

mylength :: [a] -> Int
mylength [] = 0
mylength (_:xs) = 1 + mylength xs

-- fib 1 1 2 3 5 8
fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)
--fib        1 : 1 : 2 : 3.....
--(tail fib) 1 : 2 : 3 : 5
-- (+)       2 : 3 : 5



fib1 :: Integer -> Integer
fib1 1 = 1
fib1 2 = 1
fib1 n = fib1 (n - 1) + fib1 (n - 2)


-- p :: Person1
-- p = Person1 "yang" 23 "ChengDu"

tp1 :: IO ()
tp1 = pPrint p1 

p1 :: Person1
p1 = Person1 {name = "yang", age = 23, address = "Chang"}
-- p2 :: Person1
-- p2 = Person2 "newYang" B

someFunc :: IO ()
someFunc = putStrLn "someFunc"

t :: Int
t = 1 + floor (2:: Double)

inNum :: Bool
inNum = elem 1  [2,3]

myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x:_) = Just x

myList :: [Integer]
myList = 1 : 2 : 3 : 4 : []
myList1 :: [Integer]
myList1 = [1,2,3,4]


