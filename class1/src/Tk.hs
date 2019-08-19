{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
module Tk where
import Data.List

tf :: IO ()
tf = print "ncie"

t = sort [1,2,3,1,2,3,5]

-- data MyMaybe a = MyNothing | MyJust a
-- add = 1 + 1
dev :: Int -> Int ->  Int
dev a b =a `div` b

dev1 :: Int -> Int -> Maybe Int
dev1 a b =
  if b == 0
    then Nothing
    else Just $ a `div` b
-- Except

dev2 :: Int -> Int -> Either String Int
dev2 a b =
  if b == 0
    then Left " div 0 "
    else Right  $ a `div` b


new a =
  let res = dev1 9 3
   in case res of
        Nothing -> undefined
        Just x -> undefined

-- Either

t1 x = W "hello" x
t2 x = W "World " x

data W a =
  W String a
  deriving (Show, Eq, Functor)

instance Applicative W  where
  pure a = W "" a
  (W s1 f) <*> (W s2 a) = W (s1 <> s2) (f a)

instance Monad W where
  return = pure
  (W s1 a) >>= f =
    let W s2 b = f a
     in W (s2 <> s1) b

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = quickSort a ++ [x] ++ quickSort b
  where
    (a, b) = (filter (x >) xs, filter (x <=) xs)

quickSortM :: (Show a, Ord a) => [a] -> W [a]
quickSortM [] =W "empyt\n"  []
quickSortM [x] = W (show x ++ " \n") [x]
quickSortM (x:xs) = do
  W ( show a ++" <-|-> " ++ show b++ "\n") ()
  xl <- quickSortM a
  xr <- quickSortM b
  return $ xl ++ [x] ++ xr
  where
    (a, b) = (filter (x >) xs, filter (x <=) xs)

r = do
  let l = [2,3,4,1,6,4,2,3,5,6,7,8,3,1,3,4,5,6,7,2]
      W s b = quickSortM  l
  putStrLn s
  print l
  print b


megSort :: Ord a => [a] -> [a]
megSort [] = []
megSort [x] = [x]
megSort xs = meg (megSort a) (megSort b)
  where
    l = length xs `div` 2
    (a, b) = splitAt l xs


meg :: Ord a => [a] -> [a] -> [a]
meg [] ys = ys
meg xs [] = xs
meg (x:xs) (y:ys) =
  if x < y
    then x : meg xs (y : ys)
    else y : meg (x : xs) ys



megSortM :: (Show a ,Ord a) => [a] -> W [a]
megSortM [] =return []
megSortM [x] = return [x]
megSortM xs = do
  W (show a ++ " <-|-> " ++ show b ++ "\n") ()
  la <- megSortM a
  W (show la ++ " <- \n") ()
  ra <- megSortM b
  W ( "-> "++ show ra ++ " \n") ()
  return $ meg la ra
  where
    l = length xs `div` 2
    (a, b) = splitAt l xs

r1 = do
  let W a b = megSortM [3,4,2,1,3,4,5,6,7]
  -- let W a b = megSortM "adfasodfjsaoidfjasojdfba"
  print b
  putStrLn a


-- quickSortM :: Ord a => [a] -> W



-- Int Doub (W "1212" Int)

a1 x = W "hello " x

a2 x = W "World " x

-- Maybe Except State IO




