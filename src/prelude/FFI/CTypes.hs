module FFI
	( -- Integral types, instances of: Eq, Ord, Num, Read, Show, Enum,
	  -- Typeable, Storable, Bounded, Real, Integral, Bits
	  CChar(..),    CSChar(..),  CUChar(..)
	, CShort(..),   CUShort(..), CInt(..),    CUInt(..)
	, CLong(..),    CULong(..),  CLLong(..),  CULLong(..)

	  -- Floating types, instances of: Eq, Ord, Num, Read, Show, Enum,
	  -- Typeable, Storable, Real, Fractional, Floating, RealFrac, RealFloat
	, CFloat(..),   CDouble(..), CLDouble(..)
	) where

import NonStdUnsafeCoerce
import Int	( Int8,  Int16,  Int32,  Int64  )
import Word	( Word8, Word16, Word32, Word64 )
import Storable	( Storable(..) )
import Monad	( liftM )
import Ptr

{---
Efficiency hack: We don't really map a newtype over a list,
but do a coercion instead.
-}
fakeMap :: (a -> b) -> [a] -> [b]
fakeMap _f xs = unsafeCoerce xs


-- As long as there is no automatic derivation of classes for newtypes,
-- we resort to extremely dirty cpp-hackery.   :-P
-- Some care has to be taken when the macros below are modified,
-- otherwise the layout rule will bite you.

#define BASIC_TYPE(T,B) \
newtype T = T B deriving (Eq, Ord) ; \
INSTANCE_READ(T) ; \
INSTANCE_SHOW(T) ; \
INSTANCE_ENUM(T) ; \
INSTANCE_STORABLE(T) ;

#define NUMERIC_TYPE(T,B) \
BASIC_TYPE(T,B) ; \
INSTANCE_NUM(T) ;

#define INTEGRAL_TYPE(T,B) \
NUMERIC_TYPE(T,B) ; \
INSTANCE_BOUNDED(T) ; \
INSTANCE_REAL(T) ; \
INSTANCE_INTEGRAL(T) ;

#define FLOATING_TYPE(T,B) \
NUMERIC_TYPE(T,B) ; \
INSTANCE_REAL(T) ; \
INSTANCE_FRACTIONAL(T) ; \
INSTANCE_FLOATING(T) ; \
INSTANCE_REALFRAC(T) ; \
INSTANCE_REALFLOAT(T)

#define INSTANCE_READ(T) \
instance Read T where { \
   readsPrec p s = fakeMap (\(x, t) -> (T x, t)) (readsPrec p s) }

#define INSTANCE_SHOW(T) \
instance Show T where { \
   showsPrec p (T x) = showsPrec p x }

#define INSTANCE_NUM(T) \
instance Num T where { \
   (T i) + (T j) = T (i + j) ; \
   (T i) - (T j) = T (i - j) ; \
   (T i) * (T j) = T (i * j) ; \
   negate  (T i) = T (negate i) ; \
   abs     (T i) = T (abs    i) ; \
   signum  (T i) = T (signum i) ; \
   fromInteger x = T (fromInteger x) }

#define INSTANCE_STORABLE(T) \
instance Storable T where { \
   sizeOf    (T x)       = sizeOf x ; \
   alignment (T x)       = alignment x ; \
   peekElemOff (Ptr a) i       = liftM T (peekElemOff (Ptr a) i) ; \
   pokeElemOff (Ptr a) i (T x) = pokeElemOff (Ptr a) i x }

#define INSTANCE_BOUNDED(T) \
instance Bounded T where { \
   minBound = T minBound ; \
   maxBound = T maxBound }

#define INSTANCE_ENUM(T) \
instance Enum T where { \
   succ           (T i)             = T (succ i) ; \
   pred           (T i)             = T (pred i) ; \
   toEnum               x           = T (toEnum x) ; \
   fromEnum       (T i)             = fromEnum i ; \
   enumFrom       (T i)             = fakeMap T (enumFrom i) ; \
   enumFromThen   (T i) (T j)       = fakeMap T (enumFromThen i j) ; \
   enumFromTo     (T i) (T j)       = fakeMap T (enumFromTo i j) ; \
   enumFromThenTo (T i) (T j) (T k) = fakeMap T (enumFromThenTo i j k) }

#define INSTANCE_REAL(T) \
instance Real T where { \
   toRational (T i) = toRational i }

#define INSTANCE_INTEGRAL(T) \
instance Integral T where { \
   (T i) `quot`    (T j) = T (i `quot` j) ; \
   (T i) `rem`     (T j) = T (i `rem`  j) ; \
   (T i) `div`     (T j) = T (i `div`  j) ; \
   (T i) `mod`     (T j) = T (i `mod`  j) ; \
   (T i) `quotRem` (T j) = let (q,r) = i `quotRem` j in (T q, T r) ; \
   (T i) `divMod`  (T j) = let (d,m) = i `divMod`  j in (T d, T m) ; \
   toInteger (T i)       = toInteger i }

#define INSTANCE_FRACTIONAL(T) \
instance Fractional T where { \
   (T x) / (T y)  = T (x / y) ; \
   recip   (T x)  = T (recip x) ; \
   fromRational	r = T (fromRational r) }

#define INSTANCE_FLOATING(T) \
instance Floating T where { \
   pi                    = pi ; \
   exp   (T x)           = T (exp   x) ; \
   log   (T x)           = T (log   x) ; \
   sqrt  (T x)           = T (sqrt  x) ; \
   (T x) **        (T y) = T (x ** y) ; \
   (T x) `logBase` (T y) = T (x `logBase` y) ; \
   sin   (T x)           = T (sin   x) ; \
   cos   (T x)           = T (cos   x) ; \
   tan   (T x)           = T (tan   x) ; \
   asin  (T x)           = T (asin  x) ; \
   acos  (T x)           = T (acos  x) ; \
   atan  (T x)           = T (atan  x) ; \
   sinh  (T x)           = T (sinh  x) ; \
   cosh  (T x)           = T (cosh  x) ; \
   tanh  (T x)           = T (tanh  x) ; \
   asinh (T x)           = T (asinh x) ; \
   acosh (T x)           = T (acosh x) ; \
   atanh (T x)           = T (atanh x) }

#define INSTANCE_REALFRAC(T) \
instance RealFrac T where { \
   properFraction (T x) = let my = properFraction x in (fst my, T (snd my)) ; \
   truncate (T x) = truncate x ; \
   round    (T x) = round x ; \
   ceiling  (T x) = ceiling x ; \
   floor    (T x) = floor x }

#define INSTANCE_REALFLOAT(T) \
instance RealFloat T where { \
   floatRadix     (T x) = floatRadix x ; \
   floatDigits    (T x) = floatDigits x ; \
   floatRange     (T x) = floatRange x ; \
   decodeFloat    (T x) = decodeFloat x ; \
   encodeFloat m n      = T (encodeFloat m n) ; \
   exponent       (T x) = exponent x ; \
   significand    (T x) = T (significand  x) ; \
   scaleFloat n   (T x) = T (scaleFloat n x) ; \
   isNaN          (T x) = isNaN x ; \
   isInfinite     (T x) = isInfinite x ; \
   isDenormalized (T x) = isDenormalized x ; \
   isNegativeZero (T x) = isNegativeZero x ; \
   {- isIEEE         (T x) = isIEEE x ; -} \
   (T x) `atan2`  (T y) = T (x `atan2` y) }





BASIC_TYPE(CChar,Char)
INTEGRAL_TYPE(CSChar,Int8)
INTEGRAL_TYPE(CUChar,Word8)
INTEGRAL_TYPE(CShort,Int16)
INTEGRAL_TYPE(CUShort,Word16)
INTEGRAL_TYPE(CInt,Int)
INTEGRAL_TYPE(CUInt,Word32)
INTEGRAL_TYPE(CLong,Int32)
INTEGRAL_TYPE(CULong,Word32)
INTEGRAL_TYPE(CLLong,Int64)
INTEGRAL_TYPE(CULLong,Word64)

FLOATING_TYPE(CFloat,Float)
FLOATING_TYPE(CDouble,Double)
-- HACK: Currently no long double in the FFI, so we simply re-use double
FLOATING_TYPE(CLDouble,Double)
