module Prelude where

lcm		:: (Integral a) => a -> a-> a

#if !defined(TRACING)
lcm _ 0		=  0
lcm 0 _		=  0
lcm x y		=  abs ((x `quot` (gcd x y)) * y)
#else
lcm x y = if x==0 || y==0 then 0 else abs ((x `quot` (gcd x y)) * y)
#endif
