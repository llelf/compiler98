module Prelude where

{-
concat          :: MonadPlus m => [m a] -> m a
concat          =  foldr (++) zero
-}

concat :: [[a]] -> [a]
concat = foldr (++) []

