module List where

genericIndex             :: (Integral i) => [a] -> i -> a
#if 1
genericIndex (x:_) 0      = x
#else
genericIndex (x:_) n | n==0 = x
#endif
genericIndex (_:xs) n
             | n > 0      = genericIndex xs (n-1)
             | otherwise  = error "List.genericIndex: negative argument"
genericIndex _ _          = error "List.genericIndex: index too large"

