module Prelude where

class  (RealFrac a, Floating a) => RealFloat a  where
    floatRadix		:: a -> Integer
    floatDigits		:: a -> Int
    floatRange		:: a -> (Int,Int)

    decodeFloat		:: a -> (Integer,Int)
    encodeFloat		:: Integer -> Int -> a
    exponent		:: a -> Int
    significand		:: a -> a
    scaleFloat		:: Int -> a -> a
    isNaN, isInfinite, isDenormalized, isNegativeZero
			:: a -> Bool
    atan2               :: a -> a -> a

    exponent x		= case decodeFloat x of (m,n) -> if m == 0 then 0 else n + floatDigits x

    significand x	= case decodeFloat x of (m,_) -> encodeFloat m (negate (floatDigits x))

    scaleFloat k x	= case decodeFloat x of (m,n) -> encodeFloat m (n+k)

--  atan2 y x = case (signum y, signum x) of
--                  ( 0, 1) -> 0
--                  ( 1, 0) -> pi/2
--                  ( 0,-1) -> negate pi
--                  (-1, 0) -> negate pi/2
--                  ( _, 1) -> atan (y/x)
--                  ( _,-1) -> atan (y/x) + pi
--                  ( 0, 0) -> error "Prelude.atan2: atan2 of origin"
    atan2 y x
      | x>0          = atan (y/x)
      | x==0 && y>0  = pi / 2
      | x<0  && y>0  = pi + atan (y/x)
      |(x<=0 && isNegativeZero y) ||
       (isNegativeZero x && isNegativeZero y)
                     = negate (atan2 (negate y) x)
      | y==0 && (x<0 || isNegativeZero x)
                     = pi	-- must be after the previous test on zero y
      | x==0 && y==0 = y	-- must be after the other double zero tests
      | otherwise    = x + y	-- x or y is a NaN, return a NaN (via +)

