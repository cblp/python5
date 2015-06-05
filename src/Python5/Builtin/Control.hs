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

module Python5.Builtin.Control where

import Python5.Builtin.Exceptions ( except )
import Python5.Builtin.Extra      ( RValue(readRValue), RValueData )
import Python5.Collections.ABC    ( Iterable(iter), StopIteration(..), next )

for :: Iterable iterator iterable => iterable a -> (a -> IO ()) -> IO ()
for iterable action = do
    iterator <- iter iterable
    loop iterator
  where
    loop iterator =
        do
            x <- next iterator
            action x
            loop iterator
        `except` \StopIteration ->
            pass

by :: (a -> b) -> a -> b
by = id

pass :: Monad m => m ()
pass = return ()

while ::  (RValue cond, RValueData cond ~ Bool) =>
          cond -> IO () -> IO ()
while cond proc = do
    cond_val <- readRValue cond
    if cond_val then do
        proc
        while cond proc
    else
        pass
