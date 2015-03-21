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

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Python5.Builtin.Str where

import Data.Char as Char

class Str a where
    str :: a -> String

instance Str () where
    str () = ""

instance Str Double where
    str = show

instance Str Integer where
    str = show

instance Str String where
    str = id

upper :: () -> String -> String
upper() = map Char.toUpper
