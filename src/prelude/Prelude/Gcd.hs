module Prelude where

gcd		:: (Integral a) => a -> a -> a

#if !defined(TRACING)
gcd 0 0 = error "Prelude.gcd: gcd 0 0 is undefined."
gcd x y	= gcd' (abs x) (abs y)
          where gcd' :: (Integral a) => a -> a -> a
                gcd' x 0 = x
                gcd' x y = gcd' y (x `rem` y)

#else
gcd x y	| x == 0 && y == 0 = error "Prelude.gcd: gcd 0 0 is undefined."
        | otherwise = 
         gcd' (abs x) (abs y)
          where --gcd'     :: Int -> Int -> Int
	          gcd' x y  = if y == 0 then x else gcd' y (x `rem` y)

#endif
