module            Python5.Builtin         ( ($), (**), (+), (/), (//), (<)
                                          , Integer, String
                                          , abs
                                          , all
                                          , input
                                          , print, end
                                          )
    where

import qualified  Prelude
import            Prelude                 ( ($), (**), (+), (.), (/), (<)
                                          , Bool, IO, Integer, String
                                          )
import            Python5.Builtin.Print   ( print, end )
import            Python5.Collections.ABC ( Iterable(iter) )

(//) :: Prelude.RealFrac a => a -> a -> a
x // y = Prelude.fromInteger $ Prelude.floor (x / y)

abs :: Prelude.Num a => a -> a
abs = Prelude.abs

all :: Iterable iterable => iterable Bool -> Bool
all = Prelude.and . iter

input :: String -> IO String
input prompt = do
    Prelude.putStr prompt
    Prelude.getLine
