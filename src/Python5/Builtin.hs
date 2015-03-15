module Python5.Builtin where

abs :: Num a => a -> a
abs = Prelude.abs

all :: [Bool] -> Bool
all = Prelude.and
