module Prelude where

infix 4 `notElem`

notElem :: (Eq a) => a -> [a] -> Bool
#if !defined(TRACING)
notElem x = all (x/=)
#else
notElem x xs = all (x/=) xs
#endif

