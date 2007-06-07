module Main where

clunky :: [(String,String)] -> String -> String -> String
clunky env var1 var2
  | Just val1 <- lookup var1 env
  , Just val2 <- lookup var2 env
  = val1 ++ val2
clunky env var1 var2
  = var1 ++ var2

table = [("foo","bar"),("fizz","buzz")]

main = do
  print (clunky table "foo"  "fizz")
  print (clunky table "foo"  "bar")
  print (clunky table "buzz" "fizz")
