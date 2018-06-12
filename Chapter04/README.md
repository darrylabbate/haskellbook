# Chapter 4: Basic Datatypes

## 4.3 Data Declarations

`data Foo = Fizz | Buzz`
* `Foo` is a **type constructor**.
* `Fizz` and `Buzz` are **data constructors**.
* `|` is a pipe which indicates the **sum type** or logical disjunction. In this example, a `Foo` value is either `Fizz` *or* `Buzz`.

### Exercises: Mood Swing
See [exercise01.md](https://github.com/rootbeersoup/haskellbook/blob/master/Chapter04/exercise01.md) and [`mood.hs`](https://github.com/rootbeersoup/haskellbook/blob/master/Chapter04/mood.hs).

## Numeric Types

### Integral numbers
* `Int`: A fixed-precision integer with a theoretical range.
  * An artifact of legacy hardware support. Use `Integer`.
  * Use `maxBound` and `minBound` from `GHC.Int` to find limitations of `Int`, `Int8`, `Int16`, `Int32` and `Int64`.
    * Querying `:info` in the GHCi will reveal if a type has an instance of `Bounded`.
* `Integer`: Same as `Int`, but supports arbitrarily large or small numbers.

### Fractional numbers
* `Float`: Single-precision floating-point numbers.
* `Double`: Double-precision floating-point numbers.
* `Rational`: A fractional number represented as a ratio of two integers. `1 / 2 :: Rational` will be a value carrying two `Integer` values; the numerator `1` and the denominator `2`. It  represents a ratio of 1:2.
* `Scientific`: A space-efficient and nearly arbitrarily-precise scientific number type, e.g. 2 ^ 16.
  * `Scientific` is not bundled with GHC.

All numeric datatypes have typeclass intances of `Num`.

## Comparing Values

* `==`, `<`, `>`, `<=`, `>=` and `/=` are all Boolean comparison functions.
* `&&` is the infix operator for Boolean conjunction e.g. **and**.
* `||` is the infix operator for Boolean *disjunction* e.g. **or**.

## Conditionals with if-then-else
Haskell doesn't have 'if' statements, but does have 'if' *expressions*.
```haskell
Prelude> let t = "Truthin'"
Prelude> let f = "Falsin'"
Prelude> if True then t else f
"Truthin'"
Prelude> if False then t else f
"Falsin'"
```
* `True` returns `then` (`t`).
* `False` returns `else` (`f`).

If-expressions can only be evaluated to `Bool`.
