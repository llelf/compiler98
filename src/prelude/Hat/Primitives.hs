module Primitives where

-- all of these need an implementation outside Haskell itself.

_seq :: a -> b -> b
primError :: String -> a

primToEnumChar :: Int -> Char
primFromEnumChar :: Char -> Int

primIntEq :: Int -> Int -> Bool
primIntNe :: Int -> Int -> Bool
primIntLt :: Int -> Int -> Bool
primIntLe :: Int -> Int -> Bool
primIntGt :: Int -> Int -> Bool
primIntGe :: Int -> Int -> Bool
primIntQuot   :: Int -> Int -> Int
primIntRem    :: Int -> Int -> Int
primIntPlus   :: Int -> Int -> Int
primIntMinus  :: Int -> Int -> Int
primIntTimes  :: Int -> Int -> Int
primIntNegate :: Int -> Int
primIntAbs    :: Int -> Int
primIntSignum :: Int -> Int

primIntegerFromInt :: Int -> Integer
primIntFromInteger :: Integer -> Int

primIntegerEq :: Integer -> Integer -> Bool
primIntegerNe :: Integer -> Integer -> Bool
primIntegerLt :: Integer -> Integer -> Bool
primIntegerLe :: Integer -> Integer -> Bool
primIntegerGt :: Integer -> Integer -> Bool
primIntegerGe :: Integer -> Integer -> Bool
primIntegerQuot    :: Integer -> Integer -> Integer
primIntegerRem     :: Integer -> Integer -> Integer
primIntegerQuotRem :: Integer -> Integer -> (Integer,Integer)
primIntegerAdd     :: Integer -> Integer -> Integer
primIntegerSub     :: Integer -> Integer -> Integer
primIntegerMul     :: Integer -> Integer -> Integer
primIntegerNeg     :: Integer -> Integer

primFloatFromInteger  :: Integer -> Float
primDecodeFloat       :: Float -> (Integer,Int)
primEncodeFloat       :: Integer -> Int -> Float

primFloatEq :: Float -> Float -> Bool
primFloatNe :: Float -> Float -> Bool
primFloatLt :: Float -> Float -> Bool
primFloatLe :: Float -> Float -> Bool
primFloatGt :: Float -> Float -> Bool
primFloatGe :: Float -> Float -> Bool
primFloatExp  :: Float -> Float
primFloatLog  :: Float -> Float
primFloatSqrt :: Float -> Float
primFloatSin  :: Float -> Float
primFloatCos  :: Float -> Float
primFloatTan  :: Float -> Float
primFloatAsin :: Float -> Float
primFloatAcos :: Float -> Float
primFloatAtan :: Float -> Float
primFloatDiv  :: Float -> Float -> Float
primFloatAdd  :: Float -> Float -> Float
primFloatSub  :: Float -> Float -> Float
primFloatMul  :: Float -> Float -> Float
primFloatMul  :: Float -> Float -> Float
primFloatAbs    :: Float -> Float
primFloatSignum :: Float -> Float

primDoubleFromInteger :: Integer -> Double
primDecodeDouble :: Double -> (Integer,Int)
primEncodeDouble :: Integer -> Int -> Double

primDoubleEq :: Double -> Double -> Bool
primDoubleNe :: Double -> Double -> Bool
primDoubleLt :: Double -> Double -> Bool
primDoubleLe :: Double -> Double -> Bool
primDoubleGt :: Double -> Double -> Bool
primDoubleGe :: Double -> Double -> Bool
primDoubleExp  :: Double -> Double
primDoubleLog  :: Double -> Double
primDoubleSqrt :: Double -> Double
primDoubleSin  :: Double -> Double
primDoubleCos  :: Double -> Double
primDoubleTan  :: Double -> Double
primDoubleAsin :: Double -> Double
primDoubleAcos :: Double -> Double
primDoubleAtan :: Double -> Double
primDoubleDiv  :: Double -> Double -> Double
primDoubleAdd  :: Double -> Double -> Double
primDoubleSub  :: Double -> Double -> Double
primDoubleMul  :: Double -> Double -> Double
primDoubleAbs    :: Double -> Double
primDoubleSignum :: Double -> Double

