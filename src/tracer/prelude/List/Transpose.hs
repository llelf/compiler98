module List where

transpose               :: [[a]] -> [[a]]
transpose               =  foldr
                             (\xs xss -> zipWith (:) xs (xss ++ repeat []))
                             []
