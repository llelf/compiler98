module List where

genericDrop             :: (Integral i) => i -> [a] -> [a]
#if 1
genericDrop 0 xs        =  xs
#else
genericDrop n xs | n==0 =  xs
#endif
genericDrop _ []        =  []
genericDrop n (_:xs) | n > 0  =  genericDrop (n-1) xs
genericDrop _ _         =  error "List.genericDrop: negative argument"
