module Prelude where

putStr :: String -> IO ()
putStr = mapM_ putChar

--putStr [] = return ()
--putStr (x:xs) = do putChar x
--                   putStr xs
