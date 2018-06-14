## 5.8 Chapter Exercises

### Multiple Choice

1. A value of type `[a]` is
  * c) a list whose elements are all of some type `a`
1. A function of type `[[a]] -> [a]` could
  * a) take a list of strings as an argument
1. A function of type `[a] -> Int -> a`
  * b) returns one elemtn of type `a` from a list
1. A funciton a type `(a, b) -> a`
  * c) takes a tuple argument and returns the first value

### Determine the type

1.
    1. 54; `Num a => a`
    1. (0, "doge"); `Num a => (a, [Char])`
    1. (0, "doge"); `(Integer, [Char])`
    1. False; `Bool`
    1. 5; `Int`
    1. False; `Bool`
1. `w :: Num a => a`
1. `z :: Num a => a -> a` (y is now an argument)
1. `f :: Fractional a => a`
1. `f :: [Char]`

### Does it compile?

1. No. The `$` in `bigNum` is unnecessary, but `bigNum` is still a valid function. `wahoo` can only be valid if `bigNum` was an operator of some sort.
1. Yes, though I genuinely didn't know `print` was an actual Haskell function. I guess because it's not as useful as `putStrLn`.
1. No.
1. No.

### Type variable or specific type constructor?

1.
    * [0] is constrained to be of `Num`
    * [1] is fully polymorphic
    * [2] and [3] are concrete
1. 
    * `zed` is polymorphic (argument)
    * `Zed` and `Blah` are concrete, presumably based on Data constructors?
1.
    * `a` and `c` are fully polymorphic
    * `b` is constrained by `Enum`
1. 
    * All are fully polymorphic unless I'm stupid

### Write a type signature

```haskell
1. functionH :: [a] -> a
   functionH (x:_) = x -- This looks similar to `head`

2. functionC :: Ord a => a -> a ->  Bool -- a is constrained to `Ord`
   functionC x y = if (x > y) then True else False

3. functionS :: (a, b) -> b
   functionS (x, y) = y
```

### Given a type, write a function

1. `i x = x`
1. `c x y = x`
1. Yup.
1. `c' x y = y`
1. `r x = take 1 x`
1. `co a b c = b $ a c` This one fried my brain. No idea why it works.
1. `a x y = x` Type signature really doesn't make sense. The authors do this a lot and I find it to be learning-inhibitive. I got the end result which was the best I could do.
1. `a' x y = x y`

### Fix it
1. 
    ```haskell
    module Sing where

    fstString :: [Char] -> [Char]
    fstString x = x ++ " in the rain"

    sndString :: [Char] -> [Char]
    sndString x = x ++ " over the rainbow"

    sing :: [Char]
    sing = if (x > y) then fstString x else sndString y
      where x = "Singin"
            y = "Somewhere"
    ```
2. 
    ```haskell
    module Sing where

    fstString :: [Char] -> [Char]
    fstString x = x ++ " in the rain"

    sndString :: [Char] -> [Char]
    sndString x = x ++ " over the rainbow"

    sing :: [Char]
    sing = if (x < y) then fstString x else sndString y
      where x = "Singin"
            y = "Somewhere"
    ```
3.
    ```haskell
    module Arith3Fixed where

    Main :: IO ()
    Main = do
      print (1 + 2)
      putStrLn "10"
      print (negate (-1))
      print ((+) 0 blah)
        where blah = negate 1
    ```
