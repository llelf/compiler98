module Prelude where

putStrLn        :: String -> IO ()
putStrLn s      =  do putStr s
                      putStr "\n"

