module Prelude(MonadPlus(..)) where

instance  MonadPlus []  where
    xs ++ ys            =  foldr (:) ys xs
