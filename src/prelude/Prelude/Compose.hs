module Prelude where

infixr 9 .

(.)  :: (b -> c) -> (a -> b) -> a -> c
#if !defined(TRACING)
f . g = \ x -> f (g x)
#else
(.) f g x = f (g x)
#endif
