module Reverse where

rvrs :: String -> String
rvrs x = concat [awesomeString, " ", isString, " ", curryString]
  where awesomeString = drop 9 x
        isString      = take 2 $ drop 6 x
        curryString   = take 5 x

main :: IO ()
main = putStrLn $ rvrs "Curry is awesome"
