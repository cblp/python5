module Main (main) where

{-
    Compound Data Types

    Lists (known as arrays in other languages) are
    one of the compound data types that Python understands.
    Lists can be indexed, sliced and manipulated with other built-in functions.

    # Python 3: List comprehensions
    >>> fruits = ['Banana', 'Apple', 'Lime']
    >>> loud_fruits = [fruit.upper() for fruit in fruits]
    >>> print(loud_fruits)
    ['BANANA', 'APPLE', 'LIME']

    # List and the enumerate function
    >>> list(enumerate(fruits))
    [(0, 'Banana'), (1, 'Apple'), (2, 'Lime')]
-}

main = do
    let fruits = ["Banana", "Apple", "Lime"]
    let loud_fruits = [string_upper fruit | fruit <- fruits]
    print(loud_fruits)
    -- ["BANANA", "APPLE", "LIME"]

    print(list(enumerate(fruits)))
    -- [(0, "Banana"), (1, "Apple"), (2, "Lime")]
