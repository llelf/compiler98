module FFI
  ( FunPtr		-- abstract, instance of: Eq, Ord, Show
  , nullFunPtr		-- :: FunPtr a
  , castFunPtr		-- :: FunPtr a -> FunPtr b
  , freeHaskellFunPtr	-- :: FunPtr a -> IO ()
  , castFunPtrToPtr	-- :: FunPtr a -> Ptr b
  , castPtrToFunPtr	-- :: Ptr a -> FunPtr b
  ) where

{-
-- old implementation in terms of Addr
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
-}

import Ptr     (Ptr,nullPtr)
import Numeric (showHex)
import NonStdUnsafeCoerce (unsafeCoerce)

data FunPtr a;		-- primitive type known to the compiler internals

foreign import cast funPtrToInt :: FunPtr a -> Int
instance Eq (FunPtr a) where
  a == b        = (funPtrToInt a) == (funPtrToInt b)
instance Ord (FunPtr a) where
  compare a b   = compare (funPtrToInt a) (funPtrToInt b)
instance Show (FunPtr a) where
  showsPrec _ p = showString "0x" . showHex (funPtrToInt p)

nullFunPtr :: FunPtr a
nullFunPtr = castPtrToFunPtr nullPtr

castFunPtr :: FunPtr a -> FunPtr b
castFunPtr p = unsafeCoerce p

freeHaskellFunPtr :: FunPtr a -> IO ()
freeHaskellFunPtr p = return ()		-- not implemented

foreign import cast castFunPtrToPtr :: FunPtr a -> Ptr b
foreign import cast castPtrToFunPtr :: Ptr a -> FunPtr b
