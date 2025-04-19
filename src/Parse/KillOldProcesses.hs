module Parse.KillOldProcesses where

import System.Process 
import System.Posix.Signals (signalProcess, sigTERM)
import System.Exit
--import System.Posix.Types (ProcessID)
--import Control.Concurrent (threadDelay)
import Control.Monad (forM_, filterM)
import Data.List.Split

-- Function to kill processes by executable name
killOldProcesses :: Int -> String -> IO ()
killOldProcesses threshold executable = do
    processIds <- getProcessIdsByExecutable executable
    oldProcessIds <- filterM (\pid -> isOld pid threshold) processIds
    terminateProcessesByIds oldProcessIds

-- Function to get process IDs by executable name
getProcessIdsByExecutable :: String -> IO [String]
getProcessIdsByExecutable executable = do
    (existCode, output, _) <- readCreateProcessWithExitCode (shell $ "pgrep " ++ executable) ""
    if existCode == ExitSuccess 
        then return (lines output)
        else return []
        
-- Function to terminate processes by their IDs
terminateProcessesByIds :: [String] -> IO ()
terminateProcessesByIds processIds = do
    forM_ processIds $ \pid -> do
        signalProcess sigTERM (read pid)
    return ()

-- Function to check if a process has been running for a long time
isOld :: String -> Int -> IO Bool
isOld pid threshold = do
    {-(existCode, output, error) <- readCreateProcessWithExitCode (shell $ "ps -o etime= -p " ++ pid) ""
    if existCode == ExitSuccess 
        then do
            let elapsedTime = parseElapsedTime output
            return (elapsedTime >= threshold)
        else return False-}
    psOutput <- readCreateProcess (shell $ "ps -o etime= -p " ++ pid) ""
    let elapsedTime = parseElapsedTime psOutput
    return (elapsedTime >= threshold)

parseElapsedTime :: String -> Int
parseElapsedTime psOutput = do
    let timeArray = splitOneOf "-:" psOutput
    case timeArray of
        [days, hours, minutes, seconds] -> (read days) * 24 * 3600 + (read hours) * 3600 + (read minutes) * 60 + (read seconds)
        [hours, minutes, seconds] -> (read hours) * 3600 + (read minutes) * 60 + (read seconds)
        [minutes, seconds] -> (read minutes) * 60 + (read seconds)
        [seconds] -> (read seconds)
        _ -> 0
    