module List where

#if !defined(TRACING)
genericLength           :: (Integral i) => [b] -> i
#else
genericLength           :: (Num i) => [b] -> i
#endif

genericLength []        =  0
genericLength (_:l)     =  1 + genericLength l


