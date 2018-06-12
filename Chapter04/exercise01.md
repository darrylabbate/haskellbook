1. Mood
2. Blah *or* Woot
3. `changeMood :: Mood -> Woot` is invalid because `Woot` is a data constructor. Type signatures may only contain type constructors, such as `Mood`. A valid type signature for this function would be `changeMood :: Mood -> Mood` 
4. The following works since the functions aims to change from one Boolean-type value (`Blah | Woot`) to the other:
```haskell
changeMood Blah = Woot
changeMood    _ = Woot
```
5. See `mood.hs` in this directory.
