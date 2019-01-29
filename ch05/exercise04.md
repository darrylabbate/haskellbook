1. `[a] -> [a] -> [a]` changes to `[Char] -> [Char]` because the String `" yo"` infers the `[Char]` type.
1. `myMult x = (x / 3) * 5` infers the type signature `myMult :: Fractional a => a -> a` because the `(/)` infix operator requires an instance of `Fractional`.
1. `myTake` infers the type `[Char]` with the usage of `"hey you"`
1. `myCom` only requires one argument, which the usage of `length` infers to be `Int`. The type signature is inferred as `Int -> Bool`.
1. Same as above, except `Char -> Bool`.
