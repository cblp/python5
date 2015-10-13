module Test.Tasty.HUnit.X (module Test.Tasty.HUnit.X, module I) where

import Test.Tasty       as I
import Test.Tasty.HUnit as I

assertEval :: (Eq a, Show a) => IO a -> a -> IO ()
action `assertEval` valueExpected = do
    valueGot <- action
    valueGot @?= valueExpected
