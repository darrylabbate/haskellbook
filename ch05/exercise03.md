>1. Given the type a -> a, which is the type for id, attempt to make a function that terminates successfully that does something other than returning the same value. This is impossible, but you should try it anyway.

```haskell
-- First thing that came in my head
impossiblefunc :: a -> a
impossibleFunc x = reverse x
```

>2. We can get a more comfortable appreciation of parametricity by looking at a -> a -> a. This hypothetical function a -> a -> a has two–and only two–implementations. Write both possible versions of a -> a -> a. After doing so, try to violate the constraints of parametrically polymorphic values we outlined above.

I'm not fucking with this right now.

>3. Implement a -> b -> b. How many implementations can it have? Does the behavior change when the types of 𝑎 and 𝑏 change?

Curried `snd` is pretty much `a -> b -> b`.
