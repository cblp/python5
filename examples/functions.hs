{-
    Functions Defined

    The core of extensible programming is defining functions.
    Python allows mandatory and optional arguments, keyword arguments,
    and even arbitrary argument lists.

    # Python 3: Fibonacci series up to n
    >>> def fib(n):
    >>>     a, b = 0, 1
    >>>     while a < n:
    >>>         print(a, end=' ')
    >>>         a, b = b, a+b
    >>>     print()
    >>> fib(1000)
    0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987
-}

import Python5.Builtin

fib(n) = do
    a <- var 0
    b <- var 1
    while (a < n)? do
        print(a, end:=" ")
        (a, b) =: (b, a + b)
    print()

main = fib(int(1000))
