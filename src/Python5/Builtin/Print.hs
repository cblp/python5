{-# LANGUAGE  FlexibleInstances
            , NoImplicitPrelude
            , OverlappingInstances
            , TemplateHaskell
            , UndecidableInstances
            #-}

module Python5.Builtin.Print  ( end, print ) where

import Prelude                ( ($), (++), IO, String, concat, putStr, return )

import Control.Lens
import Data.IORef             ( IORef, readIORef )
import Python5.Builtin.Str    ( Str(str) )

data PrintOptions = PrintOptions {_end :: String}
makeLenses ''PrintOptions

data PrintArgState = PrintArgState [String] PrintOptions

print :: PrintArgs a => a -> IO ()
print = printImpl $ PrintArgState [] PrintOptions{_end = "\n"}

class PrintArg a where
    modifyPrintState :: a -> PrintArgState -> IO PrintArgState

instance PrintArg (PrintOptions -> PrintOptions) where
    modifyPrintState optModifier (PrintArgState strs opts) =
        return $ PrintArgState strs (optModifier opts)

instance Str a => PrintArg a where
    modifyPrintState a (PrintArgState strs opts) =
        return $ PrintArgState (strs ++ [str a]) opts

instance Str a => PrintArg (IORef a) where
    modifyPrintState ref (PrintArgState strs opts) = do
        a <- readIORef ref
        return $ PrintArgState (strs ++ [str a]) opts

class PrintArgs a where
    printImpl :: PrintArgState -> a -> IO ()

instance PrintArgs () where
    printImpl (PrintArgState strs opts) () =
        putStr $ concat strs ++ (opts ^. end)

instance PrintArg a => PrintArgs a where
    printImpl state a = do
        state' <- modifyPrintState a state
        printImpl state' ()

instance (PrintArg a, PrintArgs b) => PrintArgs (a, b) where
    printImpl state (a, b) = do
        state' <- modifyPrintState a state
        printImpl state' b

instance (PrintArg a, PrintArgs (b, c)) => PrintArgs (a, b, c) where
    printImpl state (a, b, c) = do
        state' <- modifyPrintState a state
        printImpl state' (b, c)
