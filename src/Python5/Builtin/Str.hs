{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Python5.Builtin.Str where

class Str a where
    str :: a -> String

instance Str () where
    str () = ""

instance Str Double where
    str = show

instance Str Integer where
    str = show

instance Str String where
    str = id
