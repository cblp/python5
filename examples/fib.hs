{-# LANGUAGE  FlexibleContexts
            , FlexibleInstances
            , NamedFieldPuns
            , OverlappingInstances
            , RecordWildCards
            , ScopedTypeVariables
            , UndecidableInstances
            #-}
import Prelude hiding (print)
import qualified Prelude
import Data.Functor.Identity

{- # Python 3: Fibonacci series up to n
>>> def fib(n):
>>>     a, b = 0, 1
>>>     while a < n:
>>>         print(a, end=' ')
>>>         a, b = b, a+b
>>>     print()
>>> fib(1000)
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987
-}

fib(n :: Integer) = do
    let fibrec(n, a, b) = do
            if a < n then do
                print(a, End " ")
                fibrec(n, b, a + b)
            else
                print()
    fibrec(n, 0, 1)

main = fib(1000)

--------------------------------------------------------------------------------

data PrintOption = End String -- TODO remove and use 'end' field
data PrintOptions = PrintOptions {end :: String}
data PrintArgState = PrintArgState [String] PrintOptions

print :: PrintArgs a => a -> IO ()
print = printImpl $ PrintArgState [] PrintOptions{end = "\n"}

class PrintArg a where
    modifyPrintState :: a -> PrintArgState -> PrintArgState

instance PrintArg PrintOption where
    modifyPrintState (End end) (PrintArgState strs opts) =
        PrintArgState strs opts{end}

instance Str a => PrintArg a where
    modifyPrintState a (PrintArgState strs opts) =
        PrintArgState (strs ++ [str a]) opts

class PrintArgs a where
    printImpl :: PrintArgState -> a -> IO ()

instance PrintArgs () where
    printImpl (PrintArgState strs PrintOptions{..}) () =
        putStr $ concat strs ++ end

instance PrintArg a => PrintArgs (Identity a) where
    printImpl state (Identity a) = printImpl (modifyPrintState a state) ()

instance (PrintArg a, PrintArgs (Identity b)) => PrintArgs (a, b) where
    printImpl state (a, b) = printImpl (modifyPrintState a state) (Identity b)

class Str a where
    str :: a -> String

instance Str () where
    str () = ""
instance Str Integer where
    str = show
