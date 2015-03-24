{-# LANGUAGE NoImplicitPrelude #-}
import Python5.Builtin

{-
    Quick & Easy to Learn

    Experienced programmers in any other language
    can pick up Python very quickly,
    and beginners find the clean syntax and indentation structure easy to learn.
-}

main = do
    -- Python 3: Simple output (with Unicode)
    print("Hello, I'm Python5!")
    -- Hello, I'm Python5!

    -- Input, assignment
    name <- input("What is your name?\n")
    print("Hi, {}.".format(name))
    -- What is your name?
    -- Python5
    -- Hi, Python5.
