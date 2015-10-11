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

{-# LANGUAGE NoImplicitPrelude #-}

module Builtin where

import Prelude ( ($) )
import Python5.Builtin
import Test.Tasty.HUnit.X

spec :: TestTree
spec = testGroup "Builtin"
    [ testGroup "abs()"
          [ testCase "returns the absolute value of a number" $ do
                abs(int(1)) @?= 1
                abs(int(-2)) @?= 2
          , testCase "The argument may be an integer or a floating point number" $ do
                abs(int(3)) @?= 3
                abs(float(-4)) @?= 4
          , testCase "If the argument is a complex number, its magnitude is returned" $
                abs(complex(3, 4)) @?= float(5)
          ]
    , testGroup "all()"
          [ testCase "True if all elements of the iterable are true" $ do
                all [True] `assertEval` True
                all [False] `assertEval` False
                all [True, True, True] `assertEval` True
                all [True, True, False] `assertEval` False
          , testCase "True if the iterable is empty" $
                all [] `assertEval` True
          ]
    ]
