module Prelude (Floating(..)) where

import Ratio(Ratio,(%))

infixr 8  **

class  (Fractional a) => Floating a  where
    pi			:: a
    exp, log, sqrt	:: a -> a
    (**), logBase	:: a -> a -> a
    sin, cos, tan	:: a -> a
    asin, acos, atan	:: a -> a
    sinh, cosh, tanh	:: a -> a
    asinh, acosh, atanh :: a -> a

    x ** y		=  exp (log x * y)
    logBase x y		=  log y / log x
    sqrt x		=  x ** fromRational (1%2)	--0.5		-- fromRational (1 :% 2)
    tan  x		=  sin  x / cos  x
    tanh x		=  sinh x / cosh x

