module Prelude where

drop			:: Int -> [a] -> [a]
drop 0 xs	      = xs
drop _ []	      = []
drop n (x:xs) | n > 0 = drop (n-1) xs
drop _ _              = error "PreludeList.drop: negative argument"
