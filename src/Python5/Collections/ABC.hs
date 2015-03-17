{-# LANGUAGE MultiParamTypeClasses #-}

module Python5.Collections.ABC where

class Iterable iterable where
    iter :: iterable item -> [item]

-- TODO instance Has__getitem__ => Iterable

instance Iterable [] where
    iter xs = xs

class Iterator iterator where
    next :: iterator item -> Either StopIteration (item, iterator item)

instance Iterator [] where
    next []     = Left StopIteration
    next (x:xs) = Right (x, xs)

data StopIteration = StopIteration -- TODO find proper place
