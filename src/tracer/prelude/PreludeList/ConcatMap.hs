module Prelude where

concatMap  	:: (a->[b]) -> [a] -> [b]
concatMap f	= concat . map f
