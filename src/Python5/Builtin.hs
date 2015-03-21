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

module            Python5.Builtin         ( ($), (**), (*=), (+), (++), (.), (/)
                                          , (//), (<)
                                          , Bool(False, True)
                                          , Int
                                          , Proc
                                          , String
                                          , abs
                                          , all
                                          , complex
                                          , enumerate
                                          , float
                                          , for
                                          , input
                                          , int
                                          , list
                                          , print, end
                                          , upper -- from str
                                          , var, get
                                          ) where

import qualified  Prelude
import            Prelude                 ( ($), (+), (++), (/), (<)
                                          , Bool, Double, IO, Integer, String
                                          , id
                                          )

import            Control.Arrow           ( (>>>) )
import qualified  Data.Complex            as Complex
import            Data.Complex            ( Complex )
import            Python5.Builtin.Abs     ( abs )
import            Python5.Builtin.Control ( for )
import            Python5.Builtin.Extra   ( var, get )
import            Python5.Builtin.List    ( list )
import            Python5.Builtin.Print   ( print, end )
import            Python5.Builtin.Str     ( upper )
import            Python5.Collections.ABC ( Iterable(iter) )
import            Python5.Operator        ( (**), (*=), (.), (//) )

type Int = Prelude.Integer

type Proc = IO ()

all :: Iterable iterable => iterable Bool -> Bool
all = iter >>> Prelude.and

complex :: (Double, Double) -> Complex Double
complex(a, b) = a Complex.:+ b

enumerate :: [a] -> [(Integer, a)]
enumerate = Prelude.zip [0 :: Integer ..]

float :: Double -> Double
float = id

input :: String -> IO String
input prompt = do
    Prelude.putStr prompt
    Prelude.getLine

int :: Integer -> Integer
int = id
