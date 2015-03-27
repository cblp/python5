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

{-# LANGUAGE TypeFamilies #-}

module Python5.Builtin.Extra where

import Data.IORef ( IORef, newIORef, readIORef, writeIORef )

class RValue rvalue where
    type RValueData rvalue :: *
    readRValue :: rvalue -> IO (RValueData rvalue)

instance RValue (IO a) where
    type RValueData (IO a) = a
    readRValue = Prelude.id

instance RValue (IORef a) where
    type RValueData (IORef a) = a
    readRValue = readIORef

instance (RValue ra, RValue rb) => RValue (ra, rb) where
    type RValueData (ra, rb) = (RValueData ra, RValueData rb)
    readRValue (ra, rb) = do  a <- readRValue ra
                              b <- readRValue rb
                              return (a, b)

instance RValue Int where
    type RValueData Int = Int
    readRValue = return

class LValue lvalue where
    type LValueData lvalue :: *
    writeLValue :: (LValueData lvalue ~ a) => lvalue -> a -> IO ()

(=:) :: ( LValue lvalue
        , RValue rvalue
        , RValueData rvalue ~ LValueData lvalue
        ) =>
    lvalue -> rvalue -> IO ()
v =: rx = do  x <- readRValue rx
              writeLValue v x

instance LValue (IORef a) where
    type LValueData (IORef a) = a
    writeLValue = writeIORef

instance (LValue la, LValue lb) => LValue (la, lb) where
    type LValueData (la, lb) = (LValueData la, LValueData lb)
    writeLValue (la, lb) (a, b) = do  writeLValue la a
                                      writeLValue lb b

var :: a -> IO (IORef a)
var = newIORef

get :: IORef a -> IO a
get = readIORef
