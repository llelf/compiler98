module FFI
  ( Ptr(..)	-- abstract, instance of: Eq, Ord, Enum, Show
  , nullPtr	-- :: Ptr a
  , castPtr	-- :: Ptr a -> Ptr b
  , plusPtr	-- :: Ptr a -> Int -> Ptr b
  , alignPtr    -- :: Ptr a -> Int -> Ptr a
  , minusPtr    -- :: Ptr a -> Ptr b -> Int

  ) where

import Addr

newtype Ptr a = Ptr Addr deriving (Eq,Ord,Show)

instance Enum (Ptr a) where
  toEnum a         = Ptr (toEnum a)
  fromEnum (Ptr a) = fromEnum a

nullPtr :: Ptr a
nullPtr  = Ptr nullAddr

castPtr :: Ptr a -> Ptr b
castPtr (Ptr a) = Ptr a

plusPtr :: Ptr a -> Int -> Ptr b
plusPtr (Ptr a) i = Ptr (plusAddr a i)

alignPtr :: Ptr a -> Int -> Ptr a
alignPtr (Ptr a) i = Ptr (intToAddr (let j = addrToInt a in j + (j`rem`i)))

minusPtr :: Ptr a -> Ptr b -> Int
minusPtr (Ptr a) (Ptr b) = addrToInt a - addrToInt b

