module Prelude where

putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = putChar x >> putStr xs
