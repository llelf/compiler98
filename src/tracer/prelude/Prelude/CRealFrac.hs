module Prelude where

class  (Real a, Fractional a) => RealFrac a  where
    properFraction	:: (Integral b) => a -> (b,a)
    truncate, round	:: (Integral b) => a -> b
    ceiling, floor	:: (Integral b) => a -> b
{-  
    truncate x		=  case properFraction x of (m,_) -> m
    
    round x		=  case properFraction x of
			    (n,r) ->
    			      case signum (abs r - 0.5) of
    				-1 -> n
    			 	0  -> if even n then n else if r < 0 then n - 1 else n + 1
    				1  ->                       if r < 0 then n - 1 else n + 1
    
    ceiling x		=  case properFraction x of (n,r) -> if r > 0 then n + 1 else n
    
    floor x		=  case properFraction x of (n,r) -> if r < 0 then n - 1 else n
-}
