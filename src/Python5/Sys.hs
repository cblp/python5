module Python5.Sys where

import Control.Exception ( SomeException )

print_exception :: SomeException -> IO ()
print_exception = print
