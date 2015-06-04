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

import Prelude ( ($), Eq, IO, Show )
import Python5.Builtin
import Test.Hspec

spec :: Spec
spec = do
    describe "__builtin__.abs" $ do
        it "returns the absolute value of a number" $ do
            abs(int(1)) `shouldBe` 1
            abs(int(-2)) `shouldBe` 2
        it "The argument may be an integer or a floating point number" $ do
            abs(int(3)) `shouldBe` 3
            abs(float(-4)) `shouldBe` 4
        it "If the argument is a complex number, its magnitude is returned" $
            abs(complex(3, 4)) `shouldBe` float(5)

    describe "__builtin__.all" $ do
        it "returns True if all elements of the iterable are true" $ do
            all [True] `shouldEvaluateTo` True
            all [False] `shouldEvaluateTo` False
            all [True, True, True] `shouldEvaluateTo` True
            all [True, True, False] `shouldEvaluateTo` False
        it "returns True if the iterable is empty" $
            all [] `shouldEvaluateTo` True

shouldEvaluateTo :: (Eq a, Show a) => IO a -> a -> IO ()
action `shouldEvaluateTo` valueExpected = do
    valueGot <- action
    valueGot `shouldBe` valueExpected
