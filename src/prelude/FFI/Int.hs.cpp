module FFI
  -- all types are abstract and instances of:
  -- Num, Bounded, Real, Integral, Ix, Enum, Read, Show
  ( Int8
  , Int16
  , Int32
  , Int64
  ) where

{- Note explicit braces and semicolons here - layout is corrupted by cpp. -}

{import FFIBuiltin (Int8, Int16, Int32, Int64)
;import Numeric    (readSigned,readDec,showSigned,showInt)
;import Ix


#define INT_TYPE(T,LB,UB)	\
; FOREIGNS(T)			\
; INSTANCE_EQ(T)		\
; INSTANCE_ORD(T)		\
; INSTANCE_NUM(T)		\
; INSTANCE_BOUNDED(T,LB,UB)	\
; INSTANCE_REAL(T)		\
; INSTANCE_INTEGRAL(T)		\
; INSTANCE_IX(T)		\
; INSTANCE_ENUM(T)		\
; INSTANCE_READ(T)		\
; INSTANCE_SHOW(T)

#define FOREIGNS(T)	\
; foreign import primEq##T		:: T -> T  -> Bool	\
; foreign import primLt##T		:: T -> T  -> Bool	\
; foreign import primLe##T		:: T -> T  -> Bool	\
; foreign import primGt##T		:: T -> T  -> Bool	\
; foreign import primGe##T		:: T -> T  -> Bool	\
; foreign import primAdd##T		:: T -> T  -> T		\
; foreign import primSub##T		:: T -> T  -> T		\
; foreign import primMul##T		:: T -> T  -> T		\
; foreign import primAbs##T		:: T       -> T		\
; foreign import primSignum##T		:: T       -> T		\
; foreign import primQuot##T		:: T -> T  -> T		\
; foreign import primRem##T		:: T -> T  -> T		\
; foreign import primToEnum##T		:: Int     -> T		\
; foreign import primFromEnum##T	:: T       -> Int	\
; foreign import prim##T##FromInteger	:: Integer -> T		\
; foreign import prim##T##ToInteger	:: T       -> Integer


#define INSTANCE_EQ(T)	\
; instance Eq T where	\
    { (==) = primEq##T	\
    }

#define INSTANCE_ORD(T)		\
; instance Ord T where		\
    { (<)  = primLt##T		\
    ; (<=) = primLe##T		\
    ; (>)  = primGt##T		\
    ; (>=) = primGe##T		\
    }

#define INSTANCE_NUM(T)			\
; instance Num T where			\
    { (+) = primAdd##T			\
    ; (-) = primSub##T			\
    ; (*) = primMul##T			\
    ; abs = primAbs##T			\
    ; signum = primSignum##T		\
    ; fromInteger = prim##T##FromInteger\
    }

#define INSTANCE_BOUNDED(T,LB,UB)	\
; instance Bounded T where		\
    { minBound = fromInteger LB		\
    ; maxBound = fromInteger UB		\
    }

#define INSTANCE_REAL(T)		\
; instance Real T where			\
    { toRational i = toInteger i % 1	\
    }

#define INSTANCE_INTEGRAL(T)		\
; instance Integral T where		\
    { quot = primQuot##T		\
    ; rem  = primRem##T			\
    ; toInteger = prim##T##ToInteger	\
    }

#define INSTANCE_IX(T)			\
; instance Ix T where			\
    { range (m,n) = [m..n]		\
    ; index b@(m,n) i			\
      | inRange b i = fromIntegral (i-m)			\
      | True        = error "Ix.index: Index out of range."	\
    ; inRange (m,n) i    = m <= i && i <= n			\
    }

#define INSTANCE_ENUM(T)		\
; instance Enum T where			\
    { toEnum = primToEnum##T		\
    ; fromEnum = primFromEnum##T	\
    ; enumFrom n = n : enumFrom (n+1)	\
    ; enumFromThen m n = m : enumFromThen n (2*n-m)	\
    }

#define INSTANCE_READ(T)		\
; instance Read T where			\
    { readsPrec p = readSigned readDec	\
    }

#define INSTANCE_SHOW(T)		\
; instance Show T where			\
    { showsPrec p = showSigned showInt p	\
    }


INT_TYPE(Int8,(-128),127)
INT_TYPE(Int16,(-32768),32767)
INT_TYPE(Int32,(-2147483648),2147483647)
INT_TYPE(Int64,(-9223372036854775808),9223372036854775807)

}
