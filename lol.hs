module Lol where

import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types
import Control.Concurrent

processes :: [(String, [String])]
processes = [ ("runhaskell", ["gee.hs"])
            ]

main :: IO ()
main = do
    pList <- newMVar []
    installHandler sigINT
       (Catch $ term pList) Nothing
    mapM_ (spawn pList) processes
    threadDelay 3000000
    term pList

term :: MVar [ProcessID] -> IO ()
term pList = withMVar pList . mapM_ $ \p -> do
    putStrLn $ "Killing process tree of " ++ show p
    executeFile "bash" True ["killTree.sh", show p] Nothing
    -- signalProcess sigTERM p

spawn :: MVar [ProcessID] -> (FilePath, [String]) -> IO ProcessID
spawn pList (path, args) = do
    pid <- forkProcess $ do
             -- _ <- createSession
             executeFile path True args Nothing
    -- _ <- createProcessGroupFor pid
    modifyMVar_ pList (return . (pid :))
    return pid
