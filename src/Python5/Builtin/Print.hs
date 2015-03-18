{-# LANGUAGE  FlexibleInstances
            , NoImplicitPrelude
            , OverlappingInstances
            , TemplateHaskell
            , UndecidableInstances
            #-}

module Python5.Builtin.Print  ( end, print ) where

import Prelude                ( ($), (++), IO, String, concat, putStr)
import Control.Lens
import Python5.Builtin.Str    ( Str(str) )

data PrintOptions = PrintOptions {_end :: String}
makeLenses ''PrintOptions

data PrintArgState = PrintArgState [String] PrintOptions

print :: PrintArgs a => a -> IO ()
print = printImpl $ PrintArgState [] PrintOptions{_end = "\n"}

class PrintArg a where
    modifyPrintState :: a -> PrintArgState -> PrintArgState

instance PrintArg (PrintOptions -> PrintOptions) where
    modifyPrintState optModifier (PrintArgState strs opts) =
        PrintArgState strs (optModifier opts)

instance Str a => PrintArg a where
    modifyPrintState a (PrintArgState strs opts) =
        PrintArgState (strs ++ [str a]) opts

class PrintArgs a where
    printImpl :: PrintArgState -> a -> IO ()

instance PrintArgs () where
    printImpl (PrintArgState strs opts) () =
        putStr $ concat strs ++ (opts ^. end)

instance PrintArg a => PrintArgs a where
    printImpl state a = printImpl (modifyPrintState a state) ()

instance (PrintArg a, PrintArgs b) => PrintArgs (a, b) where
    printImpl state (a, b) = printImpl (modifyPrintState a state) b

instance (PrintArg a, PrintArgs (b, c)) => PrintArgs (a, b, c) where
    printImpl state (a, b, c) = printImpl (modifyPrintState a state) (b, c)
