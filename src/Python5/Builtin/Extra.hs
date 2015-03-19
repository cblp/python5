module Python5.Builtin.Extra where

import Control.Monad.State  ( State, runState )
import Data.IORef           ( IORef, newIORef, readIORef, writeIORef )

var :: a -> IO (IORef a)
var = newIORef

runVarState :: IORef s -> State s a -> IO a
runVarState ref modifier = do
    s <- readIORef ref
    let (a, s') = runState modifier s
    writeIORef ref s'
    return a
