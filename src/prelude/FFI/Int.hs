module FFI
  -- all types are abstract and instances of:
  -- Num, Bounded, Real, Integral, Ix, Enum, Read, Show
  ( Int8
  , Int16
  , Int32
  , Int64
  ) where

import FFIBuiltin (Int8, Int16, Int32, Int64)

-- Int8
foreign import primEqInt8     :: Int8 -> Int8 -> Int8
foreign import primLeInt8     :: Int8 -> Int8 -> Bool
foreign import primAddInt8    :: Int8 -> Int8 -> Int8
foreign import primMulInt8    :: Int8 -> Int8 -> Int8
foreign import primAbsInt8    :: Int8 ->         Int8
foreign import primSignumInt8 :: Int8 ->         Int8
foreign import primQuotInt8   :: Int8 -> Int8 -> Int8
foreign import primRemInt8    :: Int8 -> Int8 -> Int8

foreign cast   primToEnumInt8 :: Int  ->         Int8
foreign cast primFromEnumInt8 :: Int8 ->         Int

instance Eq Int8 where
  (==) = primEqInt8

instance Ord Int8 where
  (<=) = primLeInt8

instance Num Int8 where
  (+) = primAddInt8
  (*) = primMulInt8
  abs = primAbsInt8
  signum = primSignumInt8
  fromInteger = toEnum . fromInteger

instance Bounded Int8 where
  minBound = toEnum (-128)
  maxBound = toEnum 127

instance Real Int8 where
  toRational i = fromEnum i % 1

instance Integral Int8 where
  quot = primQuotInt8
  rem  = primRemInt8

instance Ix Int8 where
  range (m,n) = [m..n]
  index b@(m,n) i
        | inRange b i = i - m
        | True         = error "Ix.Int.index: Index out of range."
  inRange (m,n) i    = m <= i && i <= n

instance Enum Int8 where
  toEnum = primToEnumInt8
  fromEnum = primFromEnumInt8
#if !defined(TRACING)
  enumFrom =  numericEnumFrom
  enumFromThen = numericEnumFromThen
#else
  enumFrom n = n : enumFrom (n+1)
  enumFromThen m n = m : enumFromThen n (2*n-m)
#endif

instance Read Int8 where
  readsPrec p = map (\(a,b)->(toEnum a,b)) . readSigned readDec

instance Show Int8 where
  showsPrec p = showsPrec p . fromEnum
