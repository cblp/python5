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

import Local.Test.Prelude
import Prelude    ( ($) )
import Python5.Builtin
import Python5.IO as IO
import Test.Hspec

spec :: Spec
spec =
    describe "functions" $
        it "declaration of a function with mutables" $ do
            buffer <- IO.stringIO()
            let fib(n :: Integer) = do
                    a <- var(0)
                    b <- var(1)
                    while (a < n)? do
                        print(a, end .~ " ", file .~ buffer)
                        (a, b) =: (b, a + b)
                    print(file .~ buffer)
            fib(1000)
            buffer.getvalue() >>=
                shouldBe' "0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 \n"
