{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances #-}

module Python5.Builtin.Abs where

import Data.Complex

class Abs a where
    abs :: a -> Double

instance Real real => Abs real where
    abs = realToFrac . Prelude.abs

instance Abs (Complex Double) where
    abs = realPart . Prelude.abs
