# Exercises: Type Matching

* a -> c `not :: Bool -> Bool`
* b -> d `length :: [Char] -> Int`; Actual: `length :: Foldable t => t a -> Int`.
* c -> b Actual: `concat :: Foldable t => t [a] -> [a]`
* d -> a `head :: [a] -> a`
* e -> e `(<) :: Ord a => a -> a -> Bool`
