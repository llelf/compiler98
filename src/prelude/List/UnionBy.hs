module List where

import DeleteBy
import NubBy

unionBy           :: (a->a->Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys   =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs

