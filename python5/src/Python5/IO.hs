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

{-# LANGUAGE FlexibleInstances #-}

module Python5.IO ( FileLike(write), ToFileLike(toFileLike)
                  , getvalue, stringIO
                  ) where

import Data.IORef ( IORef, modifyIORef, newIORef, readIORef )
import System.IO  ( Handle, hPutStr )

data FileLike = FileLike { write :: String -> IO () }

class ToFileLike a where
    toFileLike :: a -> FileLike

instance ToFileLike Handle where
    toFileLike handle = FileLike { write = hPutStr handle }

instance ToFileLike (IORef String) where
    toFileLike ref = FileLike { write = \str -> modifyIORef ref (++ str) }

stringIO :: () -> IO (IORef String)
stringIO() = newIORef ""

getvalue :: () -> IORef a -> IO a
getvalue() = readIORef
