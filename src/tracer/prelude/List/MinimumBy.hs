module List where

minimumBy               :: (a -> a -> a) -> [a] -> a
minimumBy min []        =  error "List.minimumBy: empty list"
minimumBy min xs        =  foldl1 min xs
