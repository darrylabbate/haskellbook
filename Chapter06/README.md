# Chapter 6: Typeclasses

## 6.2 What are typeclasses?

Typeclasses and types can be seen as opposites of each other.
* Type declarations define how a type is created.
* *Typeclass* declarations define how a type set is used in computations.

Typeclasses are generalizations of a set of types. They allow us to define and execute a standard set of features for those types.

As an example, let's say we wanted to test values for equality. We can use any data of a type which implements the `Eq` typeclass. Separate equality functions for each different type of data are unnecessary. So long as a datatype implements (instantiates) the `Eq` typeclass, we can use standard functions.

Another example would be numeric literals, whose various types implement the `Num` typeclass. `Num` defines a standard set of operators which can be used with any type of numbers.

## 6.3 Back to Bool

Using `Bool` as an example:
```haskell
Prelude> :info Bool      -- GHCi query
data Bool = False | True -- Data declaration for Bool
instance Bounded Bool    --
instance Enum Bool        |
instance Eq Bool          | Typeclasses which have instances of Bool
instance Ord Bool         |
instance Read Bool        |
instance Show Bool       --
```
* `Bounded` - For types which have upper and lower bounds
* `Enum` - For things than be enumerated
* `Eq` - For values that can be tested for equality
* `Ord` - For sequential ordering
* `Read`- Parses strings into "things." (The book says don't use it).
* `Show` - Renders "things" into strings.

## 6.4 Eq

Equality in Haskell is implemented with the `Eq` typeclass. Haskell, unlikesome programming languages, does not encode Equality into every type.

`Eq` has two basic functions: `==` and `/=`. Checking `Eq`'s info in the GHCi will list all instances of `Eq`:

```haskell
-- partial list
instance Eq a => Eq [a]
instance Eq Ordering
instance Eq Int
instance Eq Float
instance Eq Double
instance Eq Char
instance Eq Bool
instance (Eq a, Eq b) => Eq (a, b)
instance Eq ()
instance Eq a => Eq (Maybe a)
instance Eq Integer
```

We know from this list, whenever we use data of these types, we are implementing the typeclass `Eq` (as well as any other typeclasses associated with a type).

Types of `==` and `/=` in `Eq` reveal something important:

```haskell
(==) :: Eq a => a -> a -> Bool
(/=) :: Eq a => a -> a -> Bool
```

These types tell us they can be used on any type `a` which implements the `Eq` typeclass. It also tells us both arguments (`a -> a`) must be of the same type, while returning `Bool`.

The type signature tells us we must use two arguments. Both must be the same type. The type of the arguments must be able to instantiate the `Eq` typeclass. If these parameters are met, `==` and `/=` will be able to return a Boolean value.

### Successful `Eq` implementations

```haskell
Prelude> (==) 1 2
False
it :: Bool

Prelude> (==) "fizz" "buzz"
False
it :: Bool

Prelude> (==) (1,2) (1,2)
True
it :: Bool

Prelude> (==) (1, "hello") (2, "world")
False
it :: Bool

Prelude> (==) (1 / 3) (1 / 4)
False
it :: Bool

Prelude> (==) (1 `div` 2) (1 `div` 3)
True
it :: Bool
```

All of the preceding examples provded arguments of the same type, while also possessing the ability to instantiate `Eq`. Thus, we were able to successfully return Boolean values.

### Typeclass deriving

`Eq`, `Ord`, `Enum`, `Bounded`, `Read`, `Show` are the typeclass instances we can magically derive. Deriving means we don't have to manually write instances of these typeclasses when creating new datatypes. This will be addressed in-depth later.

## 6.5 Writing typeclass instances

### Eq instances

This is invalid:

```haskell
data Trivial =
  Trivial
```

...because Haskell does not provide universal equality (or stringification). So we must write our own instance of `Eq` in this scenario. A good convention is to package typeclass instances in the same file as the data types themselves.

Valid:

```haskell
data Trivial =
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True
  -- or
  (==) Trivial' Trivial' = True
```

* `instance` initiates a declaration of a typeclass instance. Typeclass instances tell the compiler hoe equality (`Eq`), stringification (`Show`), orderability (`Ord`), enumeration (`Enum`), etc should work for a particular datatype. Without the `instance` of `Eq` (or `deriving Eq`), we could not test for equality with the datatype `Trivial`.
* `Eq` is the typeclass the instance is providing to `Trivial`.
* `Trivial` is the name of the type we are providing the instance `Eq` for.
* `where` simply terminates the initial declaration, which is the beginning of the `instance`. Following this declaration will be functions/methods to be implemented.

### Partial functions

A partial function is one that doesn't handle all possible cases. These should generally be avoided, especially with types of multiple cases, such as `DayOfWeek`.

If we wrote the `DayOfWeek` instance without the line `(==) _ _ = False`, Haskell would not otherwise infer `False` for function calls such as `Mon == Tue`.

Partial functions are not only a concern with typeclass instances, but also with functions that don't handle all possible inputs, e.g.

```haskell
f :: Int -> Bool
f 2 = True
```

To make this valid, we'd need to handle all possible inputs, like:

```haskell
f :: Int -> Bool
f 2 = True
f _ = False
```

Another solution for the example functoin `f` is to define a datatype that isn't quite as large as `Int` i.e. only define a handful of inputs.

### Sometimes we need to ask for more

In instances where we require polymorphic parameters, we must constrain operations within the instance declaration. Otherwise, the compiler won't be able to make sense of the operation. 

Example:

```haskell
data Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'
```

We constrain `a` to `Eq` (`Eq a => ...`) so the expression `v == v'` makes sense to the compiler. Otherwise, it would not be a valid use of the function `==`. Haskell additionally makes sure we do not attempt to check equality of values which do not instantiate `Eq`.

### [Exercises: Eq Instances](https://github.com/rootbeersoup/haskellbook/blob/master/Chapter06/exercises)

## 6.6 Num
