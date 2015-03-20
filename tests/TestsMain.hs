{-# LANGUAGE NoImplicitPrelude #-}

import Prelude          ( Double, Integer )
import Python5.Builtin
import Test.Hspec

main :: Proc
main = hspec $ do
    describe "__builtin__.abs" $ do
        it "returns the absolute value of a number" $ do
            abs(1 :: Integer) `shouldBe` 1
            abs(-2 :: Integer) `shouldBe` 2
        it "The argument may be an integer or a floating point number" $ do
            abs(3 :: Integer) `shouldBe` 3
            abs(-4 :: Double) `shouldBe` 4
        it "If the argument is a complex number, its magnitude is returned" $ do
            abs(complex(3, 4)) `shouldBe` (5 :: Double)

    describe "__builtin__.all" $ do
        it "returns True if all elements of the iterable are true" $ do
            all [True] `shouldBe` True
            all [False] `shouldBe` False
            all [True, True, True] `shouldBe` True
            all [True, True, False] `shouldBe` False
        it "returns True if the iterable is empty" $ do
            all [] `shouldBe` True
