module Prelude where

take		      :: Int -> [a] -> [a]
--take 0 _	      = []
--take _ []	      = []
--take n (x:xs) | n>0 = x : take (n-1) xs
--take _ _            = error "PreludeList.take: negative argument"

#if !defined(TRACING)
take n xs
    | n<0  = error "PreludeList.take: negative argument"  -- []
    | n>=0 = take' n xs
  where take' 0 xs     = []
        take' n []     = []
        take' n (x:xs) = x : take' (n-1) xs

#else
take n xs
    | n<0  = error "PreludeList.take: negative argument"  -- []
    | n>=0 = take' n xs
  where take' n xs | n==0  = []
        take' n []         = []
        take' n (x:xs)     = x : take' (n-1) xs

#endif
