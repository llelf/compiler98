module List where

genericTake             :: (Integral i) => i -> [a] -> [a]
#if !defined(TRACING)
genericTake 0 _         =  []
#else
genericTake n _ | n==0  =  []
#endif
genericTake _ []        =  []
genericTake n (x:xs) | n > 0  =  x : genericTake (n-1) xs
genericTake _  _        =  error "List.genericTake: negative argument"

