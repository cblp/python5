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

module IO where

import Prelude          ( ($), (>>=), Eq, Show, flip )
import Python5.Builtin
import Python5.IO       as IO
import Test.Hspec

spec :: Spec
spec =
    describe "IO" $ do
        it "Simple output (with Unicode)" $ do
            buffer <- IO.stringIO()
            print("Hello, I'm Python5!", file .~ buffer)
            buffer.getvalue() >>= shouldBe' "Hello, I'm Python5!\n"

        it "Input" $ do
            buffer <- IO.stringIO()
            -- name <- input("What is your name?\n")
            let name = "Python5"
            print("Hi, {}.".format(name), file .~ buffer)
            buffer.getvalue() >>= shouldBe' "Hi, Python5.\n"

shouldBe' :: (Eq a, Show a) => a -> a -> Expectation
shouldBe' = flip shouldBe
