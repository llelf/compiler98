module Prelude where

_tupleRange::(Ix a) => (a -> (a -> ([(a -> b)] -> [b])))
_tupleRange l u xs =
  let ys = range (l,u)
  in concatMap ( \ f -> map f ys ) xs 
