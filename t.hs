{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Control.Monad.Trans
import Text.Read (readMaybe)
import Control.Monad
import Text.Pretty.Simple


type Cal = StateT Int (MaybeT IO)

getNumber :: Cal ()
getNumber = do
  liftIO $ print "please input a Number!"
  l:_ <- fmap words $ liftIO $ getLine
  n <- lift $ MaybeT $ return $ readMaybe l
  guard $ valid n
  modify' (+ n)

valid :: Int -> Bool
valid i =
  if i < 10 || i > 100
    then False
    else True


add :: Cal Int
add = do
  getNumber
  getNumber
  getNumber
  getNumber
  get


res = runMaybeT $ runStateT add 0

type Cal1 = ContT Int IO

getNumber1 :: Int -> Cal1 Int
getNumber1 i = do
  liftIO $ print $ "please input " ++ show i ++ " Number!"
  fun <-
    callCC $ \k -> do
      let f = k f
      return f
  l <- fmap words $ liftIO $ getLine
  case l of
    [] -> do
      liftIO $ print $ "empty input, please input " ++ show i ++ " Number"
      fun
    x:_ ->
      case readMaybe x of
        Nothing -> do
          liftIO $ print $ "input error,please input " ++ show i ++ " Number"
          fun
        Just i' ->
          if valid i'
            then return i'
            else do
              liftIO $
                print $ "the Number is not in range " ++ show i ++ " Number"
              fun

add1 :: Cal1 Int
add1 = do
  n1 <- getNumber1 1
  n2 <- getNumber1 2
  n3 <- getNumber1 3
  n4 <- getNumber1 4
  return $ n1 + n2 + n3 + n4

seqs = map getNumber1 [1..3]

res1 = runContT add1 (\i -> return i)

res2 = do
  i <- runContT (sequence seqs) (return . sum)
  print i

data T a
  = L a
  | T a (T a) (T a)
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

tt = T 1 (T 4 (L 2) (T 5 (L 6) (L 7))) (L 3)

tt' :: IO ()
tt' = pPrint tt

res3 = do
  i <- runContT (sequence $ fmap getNumber1 tt) (return . sum)
  print i


tt1 = T (Left "start") (L (Left "center")) (L (Left "right"))

tt1' = sequence tt1

type Num1 = MaybeT IO Int

getNumber2 :: MaybeT IO Int
getNumber2 = do
  liftIO $ print "please input a number: "
  l <- fmap words $ liftIO $ getLine
  case l of
    [] -> do
      liftIO $ print "the input is empty"
      MaybeT $ return Nothing
    x:_ ->
      case readMaybe x of
        Nothing -> do
          liftIO $ print "the input is  not a number"
          MaybeT $ return Nothing
        i -> MaybeT $ return i


n = msum $ repeat getNumber2

nnn :: MaybeT IO Int
nnn = do
  n1 <- n
  n2 <- n
  return $ n1 + n2

res4 = runMaybeT $ nnn

type Count = Int

-- type Cal2 = StateT Int (ContT Int IO)
type Cal3 = ContT Int (StateT (Count, Int) IO)


getNumber3 :: Cal3 ()
getNumber3 = do
  fun <-
    callCC $ \k -> do
      let f = k f
      return f
  liftIO $ print "please input a Number!"

  lift $ modify' (\(a, b) -> (a + 1, b))

  l <- fmap words $ liftIO $ getLine
  case l of
    [] -> do
      liftIO $ print "the input is empty"
      fun
    x:_ ->
      case readMaybe x of
        Nothing -> do
          liftIO $ print "not a number!"
          fun
        Just i -> lift $ modify' (\(a, b) -> (a, b + i))

res5 =
  let a = runContT getNumber3 (\_ -> return 1)
   in fmap snd $ runStateT a (0, 0)



data MyState =
  MyState
    { counter :: Int
    , number :: Int
    , errorInfor :: String
    }
  deriving (Show, Read, Eq, Ord)

type Cal4 = ContT () (StateT (Count, Int) (MaybeT IO))
type Cal5 = ContT () (StateT MyState (MaybeT IO))


updateCounter :: MyState -> MyState
updateCounter s = s {counter = counter s + 1}

updateNumber :: Int -> MyState -> MyState
updateNumber n s = s {number = number s + n}

addErrorInfot :: String -> MyState -> MyState
addErrorInfot st s = s {errorInfor = st ++ "\n" ++ errorInfor s}

getNumber5 :: Cal5 Int
getNumber5 = do
  fun <-
    callCC $ \k -> do
      let f = k f
      return f
  lift $ modify' updateCounter
  liftIO $ print "please input  a Number:"
  l <- fmap words $ liftIO $ getLine
  case l of
    [] -> do
      liftIO $ print "input empty"
      lift $ modify' $ addErrorInfot "input empty."
      fun
    x:_ ->
      case readMaybe x of
        Nothing -> do
          liftIO $ print "parse error "
          lift $ modify' $ addErrorInfot "parse error."
          lift $ lift $ MaybeT $ return $ Nothing
        Just i -> do
          lift $ modify' $ updateNumber i
          return i


add3 :: Cal5 Int
add3 = do
  n1 <- getNumber5
  n2 <- getNumber5
  return $ n1 + n2


res7 =
  runMaybeT $
  runStateT (runContT add3 (\_ -> return ())) (MyState  0 0 "log start")

getNumber4 :: Cal4 Int
getNumber4 = do
  fun <-
    callCC $ \k -> do
      let f = k f
      return f
  lift $ modify' (\(a, b) -> (a + 1, b))
  liftIO $ print "please input  a Number:"
  l <- fmap words $ liftIO $ getLine
  case l of
    [] -> do
      liftIO $ print "input empty"
      fun
    x:_ ->
      case readMaybe x of
        Nothing -> do
          liftIO $ print "parse error "
          lift $ lift $ MaybeT $ return $ Nothing
        Just i -> do
          lift $ modify' (\(a, b) -> (a, b + i))
          return i


add2 :: Cal4 Int
add2 = do
  n1 <- getNumber4
  n2 <- getNumber4
  return $ n1 + n2


res6 = runMaybeT $ runStateT (runContT add2 (\_ -> return ())) (0, 0)

type Cal6 = ContT () (MaybeT (StateT MyState IO))

-- newtype Verify e a = Verify {
--       --    StateT st (ErrorT e (Reader env)) a
--       -- == st -> env -> Either e (a, st)
--       unVerify :: StateT (GlobalToilState, [LogEvent]) (ExceptT e WithVerifyEnv) a
--     }
--   deriving (Functor, Applicative, Monad)

-- newtype WithVerifyEnv a = WithVerifyEnv {
--       unWithVerifyEnv :: Reader VerifyEnv a
--     }
--   deriving (Functor, Applicative, Monad)



getNumber6 :: Cal6  Int
getNumber6 = do
  fun <-
    callCC $ \k -> do
      let f = k f
      return f
  lift $ lift $ modify' updateCounter
  liftIO $ print "please input  a Number:"
  l <- fmap words $ liftIO $ getLine
  case l of
    [] -> do
      liftIO $ print "input empty"
      lift $ lift $ modify' $ addErrorInfot "input empty."
      fun
    x:_ ->
      case readMaybe x of
        Nothing -> do
          liftIO $ print "parse error "
          lift $ lift $ modify' $ addErrorInfot "parse error."
          -- lift $ MaybeT $ return $ Nothing
          fun
        Just i -> do
          lift $ lift $ modify' $ updateNumber i
          lift $
            lift $
            modify' $
            addErrorInfot $ "parse success, number is " ++ show i ++ "."
          return i

add4 :: Cal6 Int
add4 = do
  n1 <- getNumber6
  n2 <- getNumber6
  return $ n1 + n2


res8 = do
  r <-
    runStateT
      (runMaybeT $ (runContT add4 (\_ -> return ())))
      (MyState 0 0 "log start")
  print r
  putStrLn $ errorInfor $ snd r
  print $ counter $ snd r
  print $ number $ snd r
