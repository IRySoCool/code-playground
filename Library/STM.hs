module Library.STM where

import Control.Concurrent.STM
import Control.Concurrent 
import Control.Exception
import System.Timeout

type Account = TVar Integer

data MyException = NotEnoughMoney deriving Show

instance Exception MyException

runTransfer :: IO ()
runTransfer = do
    bob <- newAccount 0
    jill <- newAccount 4000
    forkIO $ atomically $ transfer 2000 bob jill
    -- keep transfering money from bob to jill until bob put enough money to his account
    forkIO $ timeout 10000000 (saving bob) >> pure ()
    -- wait for bob to save his money with 10s timeout
    monitoring bob jill
    -- tracking transfer status

repeatIO :: Integer -> IO a -> IO a
repeatIO 1 m = m
repeatIO n m = m >> repeatIO (n - 1) m

newAccount :: Integer -> IO Account
newAccount = newTVarIO

credit :: Integer -> Account -> STM ()
credit amount account = do
    current <- readTVar account
    writeTVar account (current + amount)

debit :: Integer -> Account -> STM ()
debit amount account = do
    current <- readTVar account
    writeTVar account (current - amount)

transfer :: Integer -> Account -> Account -> STM ()
transfer amount from to = do
    fromVal <- readTVar from
    if (fromVal - amount) >= 0
        then debit amount from >> credit amount to
        -- if using this line instead of retry, then no money will be transfered
        -- else throwSTM NotEnoughMoney
        else retry

monitoring :: Account -> Account -> IO ()
monitoring from to = repeatIO 10 $ do
    fromBalance <- readTVarIO from
    toBalance <- readTVarIO to
    putStrLn ("from balance: " ++ show fromBalance ++ ", to balance: " ++ show toBalance)
    threadDelay 1000000

saving :: Account -> IO ()
saving account = do
    amount <- fmap read getLine
    atomically (credit amount account) >> saving account