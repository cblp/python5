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

{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, KindSignatures, LambdaCase, MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances #-}

module Python5.Collections.ABC where

import Data.IORef ( IORef, newIORef, readIORef, writeIORef )

--------------------------------------------------------------------------------
-- Iterator concept

class Iterator iterator where
    next :: iterator a -> IO (Either StopIteration a)

newtype IOListRef a = IOListRef (IORef [a])

instance Iterator IOListRef where
    next (IOListRef it) = do
        readIORef it >>= \case
            []    -> return $ Left StopIteration
            x:xs  -> do writeIORef it xs
                        return $ Right x

--------------------------------------------------------------------------------
-- Iterable concept

class Iterator iterator => Iterable iterator iterable | iterable -> iterator
  where
    iter :: iterable a -> IO (iterator a)

instance Iterable IOListRef [] where
    iter xs = IOListRef `fmap` newIORef xs

-- TODO instance Has__getitem__ => Iterable

--------------------------------------------------------------------------------

data StopIteration = StopIteration -- TODO find proper place
