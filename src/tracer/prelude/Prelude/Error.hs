module Prelude(error) where

--import DPrelude
--import PrimError

_tprim_error primitive 2 :: Trace -> R String -> a

error :: String -> a
error s = case lookat s of
              () -> error' s

error' s = _prim _tprim_error s

lookat [] = ()
lookat (x:xs) = lookat xs

{-
mycError primitive 2 :: Trace -> R String -> a

error :: SR -> Trace -> R (Trace -> R String -> R a)
error sr t = fun1 NTDummy error' sr t
    where error' t s = case mycError t s of
                          v ->  R v t
-}

