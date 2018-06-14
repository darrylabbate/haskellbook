# Chapter 5: Types

## 5.2 What are types for?
* Haskell is essentially a syntactically sweet implementation of a pure lambda calculus.
* Type systems are designed to impose constraints which enfore correctness.
* Static typing means typechecking occurs at compile time.

## 5.3 How to read type signatures
When numeric values are typechecked, GHCi displays typeclass info instead of a concrete type. This is because the compiler doesn't know which numeric type a value is until it is declared or the compiler infers one. Declaring a concrete type for a numeric value beforehand will display the type properly.
```haskell
Prelude> :type 13
13:: Num p => p

-- Declare the concrete type:
Prelude> :type 13 :: Integer
13 :: Integer :: Integer

Prelude> let x = 13 :: Integer
Prelude> :type x
x :: Integer
```

### Understanding the function type

`->` is the type constructor for functions in Haskell. It's similar to other type constructors, except it takes arguments and has no data constructors. `->` contains an `infixr` prioirty of 0.

A function must have two arguments - one input and one result - in order to be valid function. *Functions are values*.

Reading the `fst` type signature:
```haskell
fst :: (a, b) ->   a
       [ 1  ] [2]  [3]
```

1. First parameter has the type `(a, b)` (tuple), which itself contains two arguments coinjoined by an infix operator.
1. The function type `->` has two parameters; `(a, b)` (input) and `a` (result).
1. The result; The same `a` from the input tuple, `(a, b)`.

We *know* `a` is the same type from `(a, b)` because the type signature of `fst` shows that nothing else happens between the input and output.

### Typeclass-constrained type variables

A typeclass-constrained variable is named for when we don't fully know the concrete type of a function e.g. `(+) :: Num a => a -> a -> a`. See: Polymorphic. Typeclass-constraints represent the maximum ambiguity a function could have. Put simply, `a` can be anything, as long as it has the subtype `Num`.

To turn the number 15 into a conrete type of `Int`:
```haskell
Prelude> let fifteen = 15
Prelude> let fifteenInt = fifteen :: Int
Prelude> :type fifteenInt
fifteenInt :: Int

-- or

Prelude> let fifteen = 15 :: Int
Prelude> :type fifteen
fifteen :: Int
```

### [Exercises: Type Matching](https://github.com/rootbeersoup/haskellbook/blob/master/Chapter05/exercise01.md)

## 5.4 Currying

Currying refers to the syntactic convenience of nesting multiple functions to create the illusion of multiple parameters in a single function.

`curry` is syntactic sugar for operating with tuples.

Because of a function's default right-associative behavior, types are implicitly parenthsized as such:
```haskell
f :: a -> a -> a
-- associates to
f :: a -> (a -> a)
```
And
```haskell
map :: (a -> b) -> [a] -> [b]
map :: (a -> b) -> ([a] -> [b])
```

The right-associations do not denote precedence or priority, but rather, serve to group the parameters into a single argument and result.

When a lambda expression appears to have more than one parameter, it's called a nested lambda. Lambdas can be nested more than twice, but it always reduces to one argument and one result.

Explicit parenthesization can be utilized to indicate order of evaluation, but does not necessarily mean the result type evaluates first.

### Currying

```haskell
Prelude> let curry f a b = f (a, b)
Prelude> :t curry
curry :: ((t1, t2) -> t) -> t1 -> t2 -> t
Prelude> :t fst
fst :: (a, b) -> a
Prelude> :t curry fst
curry fst :: t -> b -> t
Prelude> fst (1, 2)
1
Prelude> curry fst 1 2
1
```

### Uncurrying

```haskell
Prelude> let uncurry f (a, b) = f a b
Prelude> :t uncurry
uncurry :: (t1 -> t2 -> t) -> (t1, t2) -> t
Prelude> :t (+)
(+) :: Num a => a -> a -> a
Prelude> (+) 1 2
3
Prelude> uncurry (+) (1, 2)
3
```

### Sectioning

Sectioning refers to partial application of infix operators. The special syntax allows you to choose which argument to partially apply the operator to.

This does not apply to cumulative functions such as addition, since order does not matter. Here's an example using addition as an infix operator with Strings:
```haskell
Prelude> let celebrate = (++ " woot!")
Prelude> celebrate "naptime"
"naptime woot!"
Prelude> celebrate "dogs"
"dogs woot!"
```
We can use this sectioning syntax with prefix operators as well, enclosing them with backticks to make the infix:
```haskell
Prelude> elem 9 [1..10]
True
Prelude> 9 `elem` [1..10]
True
Prelude> let c = (`elem` [1..10])
Prelude> c 9
True
Prelude> c 25
False
```

### [Exercises: Type Arguments](https://github.com/rootbeersoup/haskellbook/blob/master/Chapter05/exercise02.md)

## 5.5 Polymorphism

Polymorphic - *made of many forms*

Polmorphic type variables allow the ability to return results different from the type of the argument.

In broad terms, tpye signatures may have three different types:
* Concrete
* Constrained polymorphic
* Parametrically polymorphic

Parametric polymorphism is broader, allowing the final, concrete type to be anything. Constrained polymorphism decreases the number of concrete types a final output could be, but increases what you can do with it.

To re-summarize:
* Anything with a lowercase name is a polymorphic type variable.
* A capitalized name is a concrete type.

### [Exercises: Parametricity](https://github.com/rootbeersoup/haskellbook/blob/master/Chapter05/exercise03.md)
