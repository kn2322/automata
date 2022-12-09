
m = do
    let l = do
        k <- readLn
        if k == 0 then do
            putStrLn "Cannot divide by zero!"
            l
        else return k
    d <- l
    return d
