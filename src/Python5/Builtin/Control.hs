module Python5.Builtin.Control where

import Control.Monad ( forM_ )

for :: Monad m => [a] -> (a -> m b) -> m ()
for = forM_
