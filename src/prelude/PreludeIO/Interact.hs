module Prelude where

interact :: (String -> String) -> IO ()
-- #if !defined(TRACING)
interact f = do s <- getContents
	        putStr (f s)

-- #else
-- interact f = getContents >>= \s-> putStr (f s)
-- #endif
