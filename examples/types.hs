import Python5.Builtin

-- TODO hide unused
import Control.Exception        ( SomeException )
import Data.Maybe               ( fromJust )
import Python5.Collections.ABC  ( Iterator )
import Python5.Sys              ( print_exception )

import Prelude                  ( undefined )

data Item = Item {is_good :: Bool}
    deriving Eq

create_error() = ValueError()

next_item :: Iterator it => it a -> Action (Maybe a) -- TODO hide
next_item it =
    do
        x <- next it
        return (Just x)
    `except` \StopIteration ->
        return Nothing

check_next_item(items) = do
    item <- next_item items
--     when (item.is_good.not)?
    when (item == Nothing `or` item.fromJust.is_good.not)?
--         create_error()
        raise(create_error())

main = do
    items <- iter([Item{is_good = True}])
    check_next_item(items)
    do
        check_next_item(items)
    `except` \e ->
        print_exception(e)
