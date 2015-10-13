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

{-# LANGUAGE LambdaCase #-}

module            Python5.Builtin             ( (**), (*=), (+), (++), (.)
                                              , (/), (//), (<), (=:), (==), (?)
                                              , Action
                                              , Bool(False, True)
                                              , Char
                                              , Dict, dict, get, getdefault
                                              , Eq
                                              , Integer
                                              , Maybe(Just, Nothing), fromJust
                                              , Pair((:=))
                                              , Proc
                                              , StopIteration(..)
                                              , String
                                              , ValueError(..)
                                              , abs
                                              , all
                                              , and, or
                                              , complex
                                              , enumerate
                                              , except, safe_except
                                              , float
                                              , for, by
                                              , format, upper -- from str
                                              , input
                                              , int
                                              , iter
                                              , list
                                              , next
                                              , not
                                              , pass
                                              , print, end, file, sep
                                              , raise
                                              , return
                                              , var, val
                                              , when
                                              , while
                                              ) where

import qualified  Prelude
import            Prelude                     ( (++), (/), (==)
                                              , Bool(False, True)
                                              , Char
                                              , Double
                                              , Eq
                                              , IO
                                              , Integer
                                              , Maybe(Just, Nothing)
                                              , String
                                              , not
                                              , return
                                              )

import            Control.Category            ( (>>>) )
import            Control.Monad               ( when )
import qualified  Data.Complex                as Complex
import            Data.Complex                ( Complex )
import            Data.Hashable               ( Hashable )
import qualified  Data.HashMap.Strict         as HashMap
import            Data.HashMap.Strict         ( HashMap )
import            Data.Maybe                  ( fromJust, fromMaybe )
import            Python5.Builtin.Abs         ( abs )
import            Python5.Builtin.Control     ( for, by, pass, while )
import            Python5.Builtin.Exceptions  ( ValueError(..)
                                              , except, safe_except
                                              , raise
                                              )
import            Python5.Builtin.Extra       ( (=:), var, val )
                                                -- TODO replace val with RValue?
import            Python5.Builtin.List        ( list )
import            Python5.Builtin.Print       ( print, end, file, sep )
import            Python5.Builtin.Str         ( format, upper )
import            Python5.Collections.ABC     ( Iterable(iter)
                                              , Iterator(next)
                                              , StopIteration(..)
                                              )
import            Python5.Operator            ( (**), (*=), (+), (.), (//), (<)
                                              , Pair((:=))
                                              , and, or
                                              )

type Action = IO
type Proc = Action ()

type Dict = HashMap

(?) :: (a -> b) -> a -> b
f ? x = f x
infixr 0 ?

all :: Iterable iterator iterable => iterable Bool -> Action Bool
all coll = do
    res <- var True
    for coll `by` \i ->
        when (not i)?
            res =: False
            -- TODO break
    val res

complex :: (Double, Double) -> Complex Double
complex(a, b) = a Complex.:+ b

dict :: (Eq k, Hashable k) => [Pair k v] -> Dict k v
dict kvpairs = HashMap.fromList [(k, v) | k := v <- kvpairs]

get :: (Eq k, Hashable k) => k -> Dict k v -> Maybe v
get(k) = HashMap.lookup k

getdefault :: (Eq k, Hashable k) => (k, v) -> Dict k v -> v
getdefault(k, d) = HashMap.lookup k >>> fromMaybe d

enumerate :: [a] -> [(Integer, a)]
enumerate = Prelude.zip [0..]

float :: Double -> Double
float = Prelude.id

input :: String -> IO String
input prompt = do
    Prelude.putStr prompt
    Prelude.getLine

int :: Integer -> Integer
int = Prelude.id
