module List where

maximumBy               :: (a -> a -> a) -> [a] -> a
maximumBy max []        =  error "List.maximumBy: empty list"
maximumBy max xs        =  foldl1 max xs
