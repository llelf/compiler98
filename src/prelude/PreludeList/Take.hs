module Prelude where

take		      :: Int -> [a] -> [a]
take 0 _	      = []
take _ []	      = []
take n (x:xs) | n > 0 = x : take (n-1) xs
take _ _              = error "PreludeList.take: negative argument"
