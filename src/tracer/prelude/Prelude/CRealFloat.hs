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
    isNaN, isInfinity, isDenormalized, isNegativeZero
			:: a -> Bool

    exponent x		= case decodeFloat x of (m,n) -> if m == 0 then 0 else n + floatDigits x

    significand x	= case decodeFloat x of (m,_) -> encodeFloat m (- (floatDigits x))

    scaleFloat k x	= case decodeFloat x of (m,n) -> encodeFloat m (n+k)

