module Main where

import Control.Monad (replicateM)
import qualified Data.ByteString as BS
import Network.Simple.TCP
import System.Environment

import RL_Glue.Experiment
import RL_Glue.Network

main = do
  args <- getArgs
  let n = read $ head $ args
  runExperiment (doExperiments n)

runExperiments :: Int -> IO ()
runExperiments n = do
  putStrLn "Maybe later..."

doExperiments :: Int -> (Socket, SockAddr) -> IO ()
doExperiments n (sock, addr) = do
  taskSpec <- initExperiment sock
  putStrLn ("Sent task spec: " ++ show taskSpec)

  putStrLn "\n----------Running episodes----------"
  results <- replicateM n (prettyRunEpisode sock 0)
  let avgResult = (sum results) / (fromIntegral $ length results)
  putStrLn $ "Average result: " ++ show avgResult

  cleanupExperiment sock

prettyRunEpisode :: Socket -> Int -> IO Double
prettyRunEpisode sock steps = do
  terminal <- runEpisode sock steps
  totalSteps <- getNumSteps sock
  totalReward <- getReturn sock

  putStrLn $ "Ran for " ++ show totalSteps ++ " steps.\tGot a reward of " ++ show totalReward
  return totalReward
