data StringOrInt = TisAnInt Int
                 | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt y)     = x == y
  (==) (TisAString a) (TisAString b) = a == b
  (==) _ _                           = False
