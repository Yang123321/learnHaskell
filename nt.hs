
import Text.Read
import Control.Monad.Trans.Maybe
-- import Control.Monad.Trans.


r1 :: Int
r1 = 1 + 1

-- r2 :: IO Int

getNumber :: IO Int
getNumber = do
  print "please input a number: "
  l <- getLine
  case words l of
    [] -> print "input empty! please input a number: " >> getNumber
    x:_ ->
      case readMaybe x of
        Nothing -> print "parse error ! please input a number: " >> getNumber
        Just i -> return i


r2 :: IO Int
r2 = do
  n1 <- getNumber
  n2 <- getNumber
  return $ n1 + n2









