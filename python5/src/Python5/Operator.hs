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

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Python5.Operator where

import Data.IORef             ( IORef, modifyIORef )
import Python5.Builtin.Extra  ( RValue(readRValue), RValueData )

(**) :: Integer -> Integer -> Integer
(**) = (^)

(*=) :: Num num => IORef num -> num -> IO ()
v *= x = modifyIORef v (* x)

(.) :: a -> (a -> b) -> b
object.methodCall = methodCall object
infixl 9 .

(//) :: Integral a => a -> a -> a
(//) = div
-- TODO Prelude.fromInteger $ Prelude.floor (x / y)

data Pair k v = k := v

(+) ::  ( RValue rn1
        , RValue rn2
        , RValueData rn1 ~ n
        , RValueData rn2 ~ n
        , Prelude.Num n
        ) =>
    rn1 -> rn2 -> IO n
ra + rb = do
    a <- readRValue ra
    b <- readRValue rb
    return (a Prelude.+ b)
infixl 4 +

(<) ::  ( RValue rord1
        , RValue rord2
        , RValueData rord1 ~ RValueData rord2
        , Prelude.Ord (RValueData rord1)
        ) =>
    rord1 -> rord2 -> IO Bool
ra < rb = do
    a <- readRValue ra
    b <- readRValue rb
    return (a Prelude.< b)

and :: Bool -> Bool -> Bool
and = (Prelude.&&)
infixr 3 `and`

or :: Bool -> Bool -> Bool
or = (Prelude.||)
infixr 2 `or`
