module Local.Test.Prelude ( (>>=), shouldBe' ) where

import Prelude    ( (>>=), Eq, Show, flip )
import Test.Hspec ( Expectation, shouldBe )

shouldBe' :: (Eq a, Show a) => a -> a -> Expectation
shouldBe' = flip shouldBe
