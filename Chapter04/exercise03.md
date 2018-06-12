## General

1. My guess was `length :: [Char] -> Integer`. GHCi 8.4.3 tells me it's actually `length :: Foldable t => t a -> Int`, which is not something that's yet been covered. I'm guessing the book is a little out-of-date here.
1.
    * 5
    * 3
    * 2
    * 5
1. `6 / length [1, 2, 3]` fails since `length` evaluates to `Int`, while `(/)` evaluates at `a`.
1. `6 `div` length [1, 2, 3]` evaluates fine and returns `2`.
1. Type: `Bool`. Result: `True`.
1. Type: `Bool`. Result: `False`.
1.
    * `length allAwesome == 2` returns `True`.
      * `length [["Papuchon","curry",":)"],["Quake","The Simons"]] == 2`
      * `2 == 2`
      * `True`
    * `length [1, 'a', 3, 'b']` returns an error; Literals `1` and `3` need to be *Chars* `'1'` and `'3'` to return a result, which would be `4`.
    * `(8 == 8) && ('b' < 'a')` returns `False`. Both are Boolean functions coinjoined by the infix operator `&&`.
      * `True && False`
      * `False`
    * `(8 == 8) && 9` returns an error; `9` is a literal and is neither `True` or `False`. It does not meet the necessary parameters to be included in a `&&` function.
1. 
    ```haskell
    isPalindrome :: (Eq a) => [a] -> Bool
    isPalindrome x = x == reverse x
    ```
1.
    ```haskell
    myAbs :: Integer -> Integer
    myAbs n = if n < 0 then n * (-1) else n
    ```
1.
    ```haskell
    f :: (a, b) -> (c, d) -> ((b, d), (a, c))
    f x y = ((snd x, snd y), (fst x, fst y))
    ```

## Correcting syntax

1. `F` should not be capitalized. `'x'` should be in backticks, ``x``.
  ```haskell
  x = (+)
  f xs = w `x` 1 where w = length xs
  ```
2. My guess is `\x -> x`
3. `f x = fst x`; `f (a, b)` returns scope errors, but `f ('a', 'b')`, `f (1, 2)`, etc works.

## Match the function names to their types

1. (c) `show :: Show a => a -> String`
2. (b) `(==) :: Eq a => a -> a -> Bool`
3. (c) `fst :: (a, b) -> a`
4. (d) `(+) :: Num a => a -> a -> a`
