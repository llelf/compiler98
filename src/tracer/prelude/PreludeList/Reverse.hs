module Prelude where

reverse		:: [a] -> [a]
reverse xs	= foldl (flip (:)) [] xs
