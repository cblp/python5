module            Python5.Builtin         ( ($), (**), (*=), (+), (/), (//), (<)
                                          , Bool(False, True)
                                          , Proc
                                          , String
                                          , abs
                                          , all
                                          , complex
                                          , for
                                          , input
                                          , int
                                          , print, end
                                          , var
                                          ) where

import qualified  Prelude
import            Prelude                 ( ($), (**), (+), (.), (/), (<)
                                          , Bool, Double, IO, Integer, String
                                          , id
                                          )

import qualified  Data.Complex            as Complex
import            Data.Complex            ( Complex )
import            Control.Lens            ( (*~) )
import            Data.IORef              ( IORef, modifyIORef )
import            Python5.Builtin.Abs     ( abs )
import            Python5.Builtin.Control ( for )
import            Python5.Builtin.Extra   ( var )
import            Python5.Builtin.Print   ( print, end )
import            Python5.Collections.ABC ( Iterable(iter) )

type Proc = IO ()

(*=) :: Prelude.Num num => IORef num -> num -> IO ()
v *= x = modifyIORef v $ id *~ x

(//) :: Prelude.RealFrac a => a -> a -> a
x // y = Prelude.fromInteger $ Prelude.floor (x / y)

all :: Iterable iterable => iterable Bool -> Bool
all = Prelude.and . iter

complex :: (Double, Double) -> Complex Double
complex(a, b) = a Complex.:+ b

input :: String -> IO String
input prompt = do
    Prelude.putStr prompt
    Prelude.getLine

int :: Integer -> Integer
int = id
