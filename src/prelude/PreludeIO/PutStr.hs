module Prelude where

import IO (hPutStr, stdout)

putStr :: String -> IO ()


#if !defined(TRACING)

putStr = hPutStr stdout

--putStr [] = return ()
--putStr (x:xs) = do putChar x
--                   putStr xs

#else

putStr []     = return ()
putStr (x:xs) = putChar x >> putStr xs

#endif
