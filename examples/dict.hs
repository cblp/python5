main = do
    let animals = dict[ "dog" := int(1),
                        "cat" := 2 ]

    -- total <- animals.get("dog") + animals.get("cat")
    -- ^ error: Couldn't match type ‘Maybe Integer’ with ‘Integer’

    total <- animals.getdefault("dog", 0) + animals.getdefault("cat", 0)
    print(total)
