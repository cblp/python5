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

{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

module Functions where

import Prelude ( ($) )
import Python5.Builtin
import Test.Hspec

spec :: Spec
spec =
    describe "functions" $
        it "declaration of recursive function" $
            let
            fib(n :: Int) =
                let
                fibrec(a, b) =
                    if a < n then
                        [a] ++ fibrec(b, a + b)
                    else
                        []
                in
                fibrec(0, 1)
            in
            fib(1000)
                `shouldBe`  [ 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233
                            , 377, 610, 987 ]
