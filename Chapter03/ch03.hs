exclaim x = concat [x, "!"]

selectFifth x = x !! 4

returnAwesome x = drop 9 x

thirdLetter :: String -> Char -- Works in REPL without this
thirdLetter x = x !! 2


letterIndex :: Int -> Char
letterIndex x      = testString !! x -- head $ drop x testString yields same result
  where testString = "Darryl learns Haskell"
