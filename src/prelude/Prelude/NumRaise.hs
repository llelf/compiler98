module Prelude where

infixr 8  ^

(^)		:: (Num a, Integral b) => a -> b -> a

#if !defined(TRACING)
x ^ 0 		= 1
x ^ n | n > 0 	= f x (n-1) x
		  where f _ 0 y = y
			f x n y = g x n
                                  where  g x n | even n    = g (x*x) (n `quot` 2)
       			                       | otherwise = f x (n-1) (x*y)
_ ^ _		= error "Prelude.(^): negative exponent"

#else
x ^ n | n==fromInteger 0 	= fromInteger 1
      | n > fromInteger 0 	= f x (n - fromInteger 1) x
	  where f x n y | n==0      = y
		f x n y | otherwise = g x n
                    where  g x n | even n    = g (x*x) (n `quot` fromInteger 2)
       		                 | otherwise = f x (n - fromInteger 1) (x*y)
_ ^ _		= error "Prelude.(^): negative exponent"

#endif

