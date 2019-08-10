{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Data.Binary (Binary,encode,decode)
import GHC.Generics
import Text.Pretty.Simple
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import Text.Read (readMaybe,readEither)
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Text.Pretty.Simple

data Person =
  Person
    { name :: String
    , age :: Int
    , address :: String
    }
  deriving (Show, Read, Generic, Binary, A.ToJSON, A.FromJSON)

test = encode $ Person "yang" 27 "sichuang"

test' = A.encode $ Person "yang" 27 "sichuang"

test1 = decode $ test :: Person

test1' = A.decode $ test' ::Maybe Person





data Writer a = Writer String a



data Tree a
  = Leaf a
  | Node a (Tree a) (Tree a)
  deriving (Show, Read, Binary, Generic, A.ToJSON, A.FromJSON)

-- t :: MaybeT (MaybeT (ExceptT String (MaybeT (ExceptT String IO)))) ()
-- t :: MonadIO m => m ()
-- t :: IO ()
t = pPrint $ Node "yang" (Leaf "one") (Leaf "two")


tm = do
  print "nice"
  t

test2 =
  encode $ Node "yang" (Node "yang" (Leaf "one") (Leaf "two")) (Leaf "two")

test2' =
  A.encode $ Node "yang" (Node "yang" (Leaf "one") (Leaf "two")) (Leaf "two")


test3 = decode test2 :: Tree String
test3' = A.decode test2' :: Maybe (Tree String)

newtype Sum =
  Sum Int
  deriving (Show, Read)

instance Semigroup Sum where
  Sum a <> Sum b = Sum (a + b)

instance Monoid Sum where
  mempty = Sum 0


newtype Product =
  Product Int
  deriving (Show, Read)

instance Semigroup Product where
  Product a <> Product b = Product (a * b)

instance Monoid Product where
  mempty = Product 1


fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)

lengthEqual100 :: [Integer] -> [Integer]
lengthEqual100 ls =
  map fst $ take 4 $ filter ((== 8) . length . snd) $ zip [0 ..] $ map show ls
  -- map fst $ take 5 $ filter ((== 8) . length . snd) $ zip [0 ..] $ map show ls


getNumber :: IO (Maybe Int)
getNumber = do
  print "please input a number: "
  ls <-fmap words $ getLine
  case ls of
    [] -> return Nothing
    x:xs -> return $ readMaybe x

addNumber :: IO (Maybe Int)
addNumber = do
  x1 <- getNumber
  x2 <- getNumber
  case x1 of
    Nothing -> return Nothing -- error "can't parse the first number"
    Just x1' ->
      case x2 of
        Nothing -> return Nothing -- error "can't parse the first number"
        Just x2' -> return $ Just (x1' + x2')

getNumber' :: MaybeT IO Int
getNumber' = do
  liftIO $ print "please input a number: "
  ls <- fmap words $ liftIO getLine
  case ls of
    [] -> MaybeT $ return Nothing
    x:xs -> MaybeT $ return $ readMaybe x

test4' = runMaybeT getNumber'

addNumber' :: MaybeT IO Int
addNumber' = do
  x1 <- getNumber'
  x2 <- getNumber'
  x3 <- getNumber'
  x4 <- getNumber'
  return (x1 + x2 + x3 + x4)

test5 = runMaybeT addNumber'

tf :: StateT Int (MaybeT IO ) Int
tf = do
  v <- lift $ msum $ repeat getNumber'
  put v
  return (v+2)

tft =runMaybeT $ runStateT tf 0


ntf :: MaybeT (StateT Int IO ) Int
ntf = do
  MaybeT $ return Nothing
  lift $ put 10
  return 1


ntft = runStateT (runMaybeT ntf) 0


getNumber'' :: Int -> ExceptT String IO Int
getNumber'' i = do
  liftIO $ print "please input a number: "
  ls <- fmap words $ liftIO getLine
  case ls of
    [] ->
      ExceptT $
      return $ Left $ "the number " ++ show i ++ " arg error: " ++ "empty input"
    x:xs ->
      let r =
            case readEither x of
              Left e -> Left $ "the number " ++ show i ++ " arg error: " ++ e
              Right v -> Right v
       in ExceptT $ return $ r

-- test4' = runMaybeT getNumber'

addNumber'' :: ExceptT String IO Int
addNumber'' = do
  x1 <- getNumber'' 1
  x2 <- getNumber'' 2
  x3 <- getNumber'' 3
  x4 <- getNumber'' 4
  x5 <- getNumber'' 5
  return (x1 + x2 + x3 + x4)

test6 = runExceptT addNumber''


check :: Int -> Bool
check i
  | i > 1000 = False
  | otherwise = True

newGetNumber :: ExceptT String IO Int
newGetNumber = do
  i <- getNumber'' 1
  if check i
    then do
      ExceptT $ return $ Right i
    else do
      liftIO $ print $ "the number is too big."
      ExceptT $ return $ Left "error"

getANumber :: ExceptT String IO Int
getANumber = do
  msum $  repeat newGetNumber



quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = quickSort a ++ [x] ++ quickSort b
  where
    (a, b) = (filter (< x) xs, filter (>= x) xs)
    -- (a1, b1) = foldr f ([], []) xs
    -- f a (as, bs) =
    --   if a < x
    --     then (a : as, bs)
    --     else (as, a : bs)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

ttt =
  traverse
    (\i -> do
       print i
       return i)
    [1 .. 10]

ttt1  = traverse (\i -> Nothing) [1..10]

data Tree1 a
  = Empty
  | Leaf1 a
  | Node1 (Tree1 a) a (Tree1 a)
  deriving (Show, Read, Eq, Functor, Foldable,Traversable)

-- instance Traversable Tree1 where
--   traverse f Empty = pure Empty
--   traverse f (Leaf1 x) = Leaf1 <$> f x
--   traverse f (Node1 l k r) = Node1 <$> traverse f l <*> f k <*> traverse f r

tint :: Tree1 Int
tint =
  Node1
    (Node1 (Leaf1 1) 2 (Node1 (Node1 (Leaf1 3) 4 (Leaf1 5)) 6 Empty))
    7
    (Node1 (Node1 (Leaf1 8) 9 (Leaf1 10)) 11 Empty)

tintP = traverse (\i -> print i >> return (show i)) tint >>= pPrint


