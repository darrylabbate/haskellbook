data Which a = ThisOne a
             | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThatOne a') = a == a'
  (==) (ThatOne a) (ThisOne a') = a == a'
  (==) _ _                      = False
