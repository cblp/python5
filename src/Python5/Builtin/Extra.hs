module Python5.Builtin.Extra where

import Data.IORef           ( IORef, newIORef )

var :: a -> IO (IORef a)
var = newIORef
