module FFI
  ( FunPtr		-- abstract, instance of: Eq, Ord, Enum, Show
  , nullFunPtr		-- :: FunPtr a
  , castFunPtr		-- :: FunPtr a -> FunPtr b
  , castFunPtrToPtr	-- :: FunPtr a -> Ptr b
  , castPtrToFunPtr	-- :: Ptr a -> FunPtr b
  ) where

import Addr
import Ptr

newtype FunPtr a = FunPtr Addr deriving (Eq,Ord,Show)

instance Enum (FunPtr a) where
  fromEnum (FunPtr x) = fromEnum x
  toEnum x            = FunPtr (toEnum x)

nullFunPtr :: FunPtr a
nullFunPtr  = FunPtr nullAddr

castFunPtr :: FunPtr a -> FunPtr b
castFunPtr (FunPtr a) = FunPtr a

castFunPtrToPtr :: FunPtr a -> Ptr b
castFunPtrToPtr (FunPtr a) = Ptr a

castPtrToFunPtr :: Ptr a -> FunPtr b
castPtrToFunPtr (Ptr a) = FunPtr a

