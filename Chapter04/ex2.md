# Find the mistakes

```haskell
1. not True && true
2. not (x = 6)
3. (1 * 2) > 5
4. [Merry] > [Happy]
5. [1, 2, 3] ++ "look at me!"
```

1. `true` should be capitalized; `True` is a data constructor for the type `Bool`.
2. `=` should be `==`; `=` is not a Boolean comparison function.
3. This returns `False`, which is otherwise fine, except 2 (`(1 * 2)`) is not greater than 5.
4. `Merry` and `Happy` need to be surrounded in quotation marks, unless they are Data Constructors.. The function `["Merry"] > ["Happy"]` returns the Boolean value `True`.
5. `[1, 2, 3]` is a list of Nums, while `"look at me!"` is a `String`. Working concatenating solution: `['1', '2', '3'] ++ "look at me!"`.
