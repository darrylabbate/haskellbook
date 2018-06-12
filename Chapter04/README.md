# Chapter 4: Basic Datatypes

## 4.3 Data declarations

`data Foo = Fizz | Buzz`
* `Foo` is a **type constructor**.
* `Fizz` and `Buzz` are **data constructors**.
* `|` is a pipe which indicates the **sum type** or logical disjunction. In this example, a `Foo` value is either `Fizz` *or* `Buzz`.

### Exercises: Mood Swing

See [exercise01.md](https://github.com/rootbeersoup/haskellbook/blob/master/Chapter04/exercise01.md) and [`mood.hs`](https://github.com/rootbeersoup/haskellbook/blob/master/Chapter04/mood.hs).

## 4.4 Numeric types

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

## 4.5 Comparing values

* `==`, `<`, `>`, `<=`, `>=` and `/=` are all Boolean comparison functions.
* `&&` is the infix operator for Boolean conjunction e.g. **and**.
* `||` is the infix operator for Boolean *disjunction* e.g. **or**.

## 4.6 Go on and bool me

### Exercises: Find the Mistakes

See [exercise02.md](https://github.com/rootbeersoup/haskellbook/blob/master/Chapter04/exercise02.md).

### Conditionals with if-then-else

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

## 4.7 Tuples

`Tuple` is a type that allows you to store and pass around multiple values with a single value. Tuples are referred to by the number of values in each tuple. Also known as the *arity*.
```haskell
Prelude> :info (,) 
data (,) a b = (,) a b

Prelude> (,) 1 2
(1,2)
it :: (Num a, Num b) => (a, b)

Prelude> (,,) 1 2 3
(1,2,3)
it :: (Num a, Num b, Num c) => (a, b, c)

Prelude> (,) 'a' 'b'
('a','b')
it :: (Char, Char)

Prelude> (,) "a" "b"
("a","b")
it :: ([Char], [Char])
```
Tuples can contain values of mixed types as well:
```haskell
Prelude> (,) 1 "Darryl"
(1,"Darryl")
it :: Num a => (a, [Char])

Prelude> (,,) "Darryl" 1 True
("Darryl",1,True)
it :: Num b => ([Char], b, Bool)
```
The "two-tuple" has default convenience functions for getting the first or second value, named `fst` and `snd`.
```haskell
Prelude> fst (1, 2)
1
it :: Num a => a

Prelude> 2 + snd (1, 2)
4
it :: Num a => a
```
It's generally encouraged to keep tuple size to a minimum. Most tuples will be 5-tuple or smaller.

## 4.8 Lists

Lists, like tuples, are also used for containing multiple values within a single value.

## 4.9 Chapter Exercises

See [exercise03.md](https://github.com/rootbeersoup/haskellbook/blob/master/Chapter04/exercise03.md).

## 4.10 Definitions
* Tuple - Ordered grouping of values. Must have >= 2 values. A zero tupes is called a unit.
* Typeclass - Defines the set of operations with respect to a polymorphic type.
* Data conctructors - A means of creating values that inhabit a given type.
* Type constructors - Precedes the `=` in type signatures.
* Data declarations - Define new datatypes. Data declarations always create new type constructors, but not necessarily data constructors. Data declarations are how we refer to an entire definition which begins with `data`.
* Type alias - Just as it sounds.
* Arity - Refers to the number of arguments accepted by a function.
* Polymorphism - Being able to write code in terms of values which may be more than one specific type. Polymorphic types can either be *parametric*:
  ```haskell
  id :: a -> a
  id x = x
  ```
  Or *constrained* or *bounded*:
  ```haskell
  isEqual :: Eq a => a -> a -> Bool
  isEqual xy = x == y
  ```

## 4.11 Names and variables

### Names

Seven categories of entities that have names:
* Functions
* Term-level variables
* Data constructors
* Type variables
* Type constructors
* Typeclasses
* Modules

*Term-level* is where your values live and is the code that executes when your program runs. These are **Term-level variables** and **Data constructors**.

*Type-level* (used for static analysis and verification of a program) contains **Type variables**, **Type constructors** and **Typeclasses**.

*Modules* exist for the purpose of organizing code into coherent groupings across files and directories.

### Conventions for variables

* **Type variables** generally start at `a` and go from there: `a`, `b`, `c`, etc. Numbers may sometimes be appended e.g. `a1`.
* **Functions** are often used as argumnets and are typically labeled starting at `f`. Functions can also have numbers appended to them (`f1`). Functions found decorated with the `′` character (pronounced "eff-prime") denote a function that is closely related to, or a helper function, to function `f`. Functions can be given variable names with relevant context e.g. `p` for a prime number function.

Single-letter names are often given to variables in smaller programs, but are generally discouraged in larger porgrams. Verbosity in such regard is encouraged, despite Haskell's affinity for concision and terseness.

**Arguments** are almost always given single-letter names starting at `x`, occasionally transliterated to `x1`. Argument names can follow a similar mnemonic protocol to variables e.g. `n` for numbers. A list of values named `x`, by convention, will be called `xs`, which is the plural form of `x` (`x` being the head and `xs` being the rest).
