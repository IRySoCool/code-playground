import Control.Concurrent
import System.Random
import Control.Concurrent.Async

take' :: MVar [Int] -> IO (Maybe Int)
take' mVar = do
    delay <- randomRIO (1000, 10000)
    threadDelay delay
    list <- takeMVar mVar
    case list of
        []   -> do
            putMVar mVar []
            pure Nothing
        x:xs -> do
            putMVar mVar xs
            pure (Just x)

main :: IO ()
main = do
    list <- newMVar [1..10]
    -- result <- mapM (\_ -> take' list) [1..5]
    result <- mapConcurrently (\_ -> take' list) [1..20]
    print result

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
