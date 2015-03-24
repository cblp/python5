import Python5.Builtin

{-
    All the Flow You’d Expect

    Python knows the usual control flow statements that other languages speak —
    if, for, while and range — with some of its own twists, of course.

    # For loop on a list
    >>> numbers = [2, 4, 6, 8]
    >>> product = 1
    >>> for number in numbers:
    ...    product = product * number
    ...
    >>> print('The product is:', product)
    The product is: 384
-}

main = do
    -- For loop on a list
    let numbers = [2, 4, 6, 8]
    product <- var(int(1))
    for numbers `by` \number -> do
        product *= number
    print("The product is:", product)
    -- The product is: 384
