import Data.Maybe ( fromJust )
import Python5.Builtin
import Python5.Collections.ABC ( Iterator )

import Prelude (Either(Left, Right)) -- TODO remove

data Item = Item {is_good :: Bool}
    deriving Eq

create_error() = ValueError()

next_item :: Iterator it => it a -> Action (Maybe a)
next_item it = do
    ex <- next it
    return ? case ex of
        Left StopIteration -> Nothing
        Right x -> Just x

check_next_item(items) = do
    item <- next_item items
--     when (not (item.is_good))?
    when (item == Nothing `or` item.fromJust.is_good.not)?
--         create_error()
        raise(create_error())

main = do
    items <- iter([Item{is_good = True}])
    check_next_item(items)
    check_next_item(items)
