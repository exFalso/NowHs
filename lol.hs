module Lol where

import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types
import Control.Concurrent
import Control.Monad (forM_)

processes :: [(String, [String])]
processes = [ ("./dist/build/testserver/testserver", [])
            , ("bash", ["-c", "python3 -m http.server 7357 || python -m SimpleHTTPServer 7357"])
            ]

main :: IO ()
main = do
    pList <- newMVar []
    sema <- newMVar ()
    _ <- takeMVar sema
    installHandler sigINT (Catch $ intHandler pList sema) Nothing
    mapM_ (spawn pList) processes
    --threadDelay 1500000
    withMVar pList print
    takeMVar sema
    --threadDelay 1500000
    --term pList

intHandler :: MVar [ProcessID] -> MVar () -> IO ()
intHandler pList sema = term pList >> putMVar sema ()

term :: MVar [ProcessID] -> IO ()
term pList = modifyMVar_ pList $ \l -> do
    forM_ l $ \p -> do
        putStrLn $ "Killing process tree of " ++ show p
        forkProcess $ executeFile "bash" True ["killTree.sh", show p] Nothing
        -- signalProcess sigTERM p
    return []

spawn :: MVar [ProcessID] -> (FilePath, [String]) -> IO ProcessID
spawn pList (path, args) = do
    pid <- forkProcess $ do
             -- _ <- createSession
             executeFile path True args Nothing
    -- _ <- createProcessGroupFor pid
    modifyMVar_ pList (return . (pid :))
    return pid
