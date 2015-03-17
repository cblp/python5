{-# LANGUAGE  FlexibleContexts
            , FlexibleInstances
            , NamedFieldPuns
            , OverlappingInstances
            , RecordWildCards
            , ScopedTypeVariables
            , TemplateHaskell
            , UndecidableInstances
            #-}
import Prelude hiding (print)
import qualified Prelude
import Control.Lens
import Control.Lens.TH (makeLenses)

--------------------------------------------------------------------------------

data PrintOption = End String -- TODO remove and use 'end' field

data PrintOptions = PrintOptions {_end :: String}
makeLenses ''PrintOptions

data PrintArgState = PrintArgState [String] PrintOptions

print :: PrintArgs a => a -> IO ()
print = printImpl $ PrintArgState [] PrintOptions{_end = "\n"}

class PrintArg a where
    modifyPrintState :: a -> PrintArgState -> PrintArgState

-- instance PrintArg (PrintOption -> a, a) where
--     modifyPrintState (End _end) (PrintArgState strs opts) =
--         PrintArgState strs opts{_end}

instance Str a => PrintArg a where
    modifyPrintState a (PrintArgState strs opts) =
        PrintArgState (strs ++ [str a]) opts

class PrintArgs a where
    printImpl :: PrintArgState -> a -> IO ()

instance PrintArgs () where
    printImpl (PrintArgState strs PrintOptions{..}) () =
        putStr $ concat strs ++ _end

instance PrintArg a => PrintArgs a where
    printImpl state a = printImpl (modifyPrintState a state) ()

instance (PrintArg a, PrintArgs b) => PrintArgs (a, b) where
    printImpl state (a, b) = printImpl (modifyPrintState a state) b

class Str a where
    str :: a -> String

instance Str () where
    str () = ""
instance Str Integer where
    str = show

(.=) :: a -> b -> (a, b)
(.=) = (,)

--------------------------------------------------------------------------------

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
    let fibrec :: (Integer, Integer, Integer) -> IO ()
        fibrec(n, a, b) = do
            if a < n then do
                print(a, end) -- TODO end=" "
                fibrec(n, b, a + b)
            else
                print()
    fibrec(n, 0, 1)

main = fib(1000)
