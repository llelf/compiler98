module List where

genericSplitAt          :: (Integral i) => i -> [b] -> ([b],[b])
#if !defined(TRACING)
genericSplitAt 0 xs     =  ([],xs)
#else
genericSplitAt n xs | n==0  =  ([],xs)
#endif
genericSplitAt _ []     =  ([],[])
genericSplitAt n (x:xs) | n > 0  =  (x:xs',xs'') where
                               (xs',xs'') = genericSplitAt (n-1) xs
genericSplitAt _ _      =  error "List.genericSplitAt: negative argument"
