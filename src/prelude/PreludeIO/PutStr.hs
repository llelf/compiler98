module Prelude where

putStr :: String -> IO ()


#if !defined(TRACING)

putStr = mapM_ putChar

--putStr [] = return ()
--putStr (x:xs) = do putChar x
--                   putStr xs

#else

putStr []     = return ()
putStr (x:xs) = putChar x >> putStr xs

#endif
