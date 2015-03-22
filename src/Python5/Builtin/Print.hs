{-
    Python5 — a hypothetic language
    Copyright (C) 2015 - Yuriy Syrovetskiy

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE  FlexibleInstances
            , OverlappingInstances
            , RecordWildCards
            , TemplateHaskell
            , UndecidableInstances
            #-}

module Python5.Builtin.Print  ( print, end, file, sep ) where

import Control.Lens           ( Setter', lens, makeLenses )
import Data.IORef             ( IORef, readIORef )
import Data.List              ( intercalate )
import Prelude                hiding ( print )
import Python5.Builtin.Str    ( Str(str) )
import Python5.IO             ( FileLike(write), ToFileLike(toFileLike) )
import System.IO              ( stdout )

data PrintOptions = PrintOptions  { _end    :: String
                                  , __file  :: FileLike
                                  , _sep    :: String
                                  }
makeLenses ''PrintOptions

file :: ToFileLike a => Setter' PrintOptions a
file = lens (error "`file` is write-only lens")
            (\opts a -> opts{__file = toFileLike a})

data PrintArgState = PrintArgState [String] PrintOptions

print :: PrintArgs a => a -> IO ()
print x = do
    let stdoutRef = toFileLike stdout
        state = PrintArgState []
                              PrintOptions  { _end    = "\n"
                                            , __file  = stdoutRef
                                            , _sep    = " "
                                            }
    printImpl state x

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
    printImpl (PrintArgState strs PrintOptions{..}) () =
        write __file $ intercalate _sep strs ++ _end

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
