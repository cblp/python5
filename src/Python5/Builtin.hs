module Python5.Builtin where

import Python5.Collections.ABC ( Iterable(iter) )

abs :: Num a => a -> a
abs = Prelude.abs

all :: Iterable iterable => iterable Bool -> Bool
all = Prelude.and . iter
