module Prelude where

drop			:: Int -> [a] -> [a]

--drop 0 xs	      = xs
--drop _ []	      = []
--drop n (x:xs) | n>0 = drop (n-1) xs
--drop _ _          = error "PreludeList.drop: negative argument"

#if !defined(TRACING)
drop n xs 
    | n<0  = error "PreludeList.drop: negative argument"  -- xs
    | n>=0 = drop' n xs
  where drop' 0 xs     = xs
        drop' n []     = []
        drop' n (x:xs) = drop' (n-1) xs

#else
drop n xs 
    | n<0  = error "PreludeList.drop: negative argument"  -- xs
    | n>=0 = drop' n xs
  where drop' n xs | n==0  = xs
        drop' n []         = []
        drop' n (x:xs)     = drop' (n-1) xs

#endif
