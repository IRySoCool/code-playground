import Control.Concurrent
import System.Random
import Control.Concurrent.Async
import Data.IORef

-- *Main> takeWithMVar 
-- [Just 1,Just 2,Just 3,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
takeWithMVar :: IO ()
takeWithMVar = do
    list <- newMVar [1..3]
    result <- mapConcurrently (\_ -> take list) [1..10]
    print result
    where
        take mVar = do
            list <- takeMVar mVar
            print list
            threadDelay 1000
            case list of
                []   -> putMVar mVar [] >> pure Nothing
                x:xs -> putMVar mVar xs >> pure (Just x)

-- No locking on list
-- *Main> takeWithIORef 
-- [Just 1,Just 1,Just 1,Just 1,Just 1,Just 1,Just 1,Just 1,Just 1,Just 1]
takeWithIORef :: IO ()
takeWithIORef = do
    list <- newIORef [1..3]
    result <- mapConcurrently (\_ -> take list) [1..10]
    print result
    where
        take ref = do
            list <- readIORef ref
            threadDelay 1000
            case list of
                []   -> writeIORef ref [] >> pure Nothing
                x:xs -> writeIORef ref xs >> pure (Just x)

-- ====================================================================
-- Just some test on deadlock
act1 :: (MVar Int, MVar Int) -> IO ()
act1 (a, b) = do
    x <- takeMVar a
    putMVar b (x + 1)

act2 :: (MVar Int, MVar Int) -> IO ()
act2 (a, b) = do
    x <- takeMVar b
    putMVar a (x + 1)

deadlock :: IO ()
deadlock = do
    a <- newEmptyMVar 
    b <- newEmptyMVar 

    let mVarPairs = (a, b)
    forkIO $ act1 mVarPairs
    forkIO $ act2 mVarPairs

    xs <- mapM takeMVar [a, b]

    print $ sum xs
