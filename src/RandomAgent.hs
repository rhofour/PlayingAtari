module Main where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import System.Exit
import System.Random

import RL_Glue.Agent
import RL_Glue.Network
import RL_Glue.TaskSpec

main = do
  putStrLn "RandomAgent started"
  loadAgentDebug 0 (Agent onInit onStart onStep onEnd onCleanup onMessage) undefined
  putStrLn "RandomAgent ended"

onInit :: BS.ByteString -> StateT AbsDataType IO ()
onInit taskSpecStr = do
  TaskSpec _ _ _ actType _ _ <- lift $ toTaskSpecOrDie taskSpecStr
  put actType

onStart :: Observation -> StateT AbsDataType IO Action
onStart obs = do
  absType <- get
  lift $ absTypeToActions absType

onStep :: (Reward, Observation) -> StateT AbsDataType IO Action
onStep (reward, obs) = do
  absType <- get
  lift $ absTypeToActions absType

onEnd :: Reward -> StateT AbsDataType IO ()
onEnd reward = return ()

onCleanup :: StateT AbsDataType IO ()
onCleanup = return ()

onMessage :: BS.ByteString -> StateT AbsDataType IO BS.ByteString
onMessage msg = lift $ return BS.empty

absTypeToActions :: AbsDataType -> IO Action
absTypeToActions (AbsDataType iBounds dBounds nChars) = do
  iActs <- mapM generateIntAction iBounds
  dActs <- mapM generateDoubleAction dBounds
  gen <- getStdGen
  let bs = BSC.pack $ take nChars (randoms gen)
  return . Action $ RLAbstractType iActs dActs bs


generateIntAction :: DataBounds Int -> IO Int
generateIntAction (lb, ub) = do
  let lower = case lb of
        LowBound x -> x
        NegInf -> minBound
        LBUnspec -> minBound
  let upper = case ub of
        UpBound x -> x
        PosInf -> maxBound
        UBUnspec -> maxBound
  randomRIO (lower, upper)

generateDoubleAction :: DataBounds Double -> IO Double
generateDoubleAction (LowBound lower, UpBound upper) = randomRIO (lower, upper)
generateDoubleAction (LowBound lower, _) = randomRIO (lower, lower + 10*abs lower)
generateDoubleAction (_, UpBound upper) = randomRIO (upper - 10*abs upper, upper)
generateDoubleAction _ = randomRIO (-100, 100)
