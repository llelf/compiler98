module NHC.FFI
    ( ForeignPtr		-- abstract, instance of: Eq,Ord,Show
 -- , newForeignPtr		-- :: Ptr a -> IO () -> IO (ForeignPtr a)
 -- , addForeignPtrFinalizer	-- :: ForeignPtr a -> IO () -> IO ()
    , newForeignPtr		-- :: Ptr a -> FunPtr (Ptr a -> IO ())
				--		 -> IO (ForeignPtr a)
    , addForeignPtrFinalizer	-- :: ForeignPtr a -> FunPtr (Ptr a -> IO ())
				--		 -> IO ()
    , withForeignPtr		-- :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
    , touchForeignPtr		-- :: ForeignPtr a -> IO ()
    , foreignPtrToPtr		-- :: ForeignPtr a -> Ptr a
    , castForeignPtr		-- :: ForeignPtr a -> ForeignPtr b
    ) 
    where

{-
-- old implementation in terms of ForeignObj
import Ptr
import ForeignObj

newtype ForeignPtr a = ForeignPtr ForeignObj

newForeignPtr :: Ptr a -> IO () -> IO (ForeignPtr a)
newForeignPtr (Ptr p) finalizer = do
  fo <- newForeignObj p finalizer
  return (ForeignPtr fo)

touchForeignPtr :: ForeignPtr a -> IO ()
touchForeignPtr (ForeignPtr fo) = touchForeignObj fo

withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr fo io
  = do r <- io (foreignPtrToPtr fo)
       touchForeignPtr fo
       return r

foreignPtrToPtr :: ForeignPtr a -> Ptr a
foreignPtrToPtr (ForeignPtr fo) = Ptr (foreignObjToAddr fo)

castForeignPtr :: ForeignPtr a -> ForeignPtr b
castForeignPtr (ForeignPtr a) = ForeignPtr a
-}

import Ptr
import FunPtr
import NonStdUnsafeCoerce
import Numeric (showHex)

data ForeignPtr a;	-- primitive type known to the compiler internals

foreign import cast foreignPtrToInt :: ForeignPtr a -> Int
instance Eq (ForeignPtr a) where
  a == b        =  (foreignPtrToInt a) == (foreignPtrToInt b)
instance Ord (ForeignPtr a) where
  compare a b   =  compare (foreignPtrToInt a) (foreignPtrToInt b)
instance Show (ForeignPtr a) where
  showsPrec _ p = showString "0x" . showHex (foreignPtrToInt p)

-- Note that `newForeignPtr' is not a strictly legal FFI function.
-- It is not usually possible to return a ForeignPtr as the result of
-- a foreign import.  However, in order to implement ForeignPtrs, we
-- need one single instance of returning a ForeignPtr, and this is it.
--   *** Do not do it elsewhere!
foreign import ccall "primForeignPtrC"
    newForeignPtr :: Ptr a -> FunPtr (Ptr a -> IO ()) -> IO (ForeignPtr a)

-- addForeignPtrFinalizer is not implemented in nhc98.
addForeignPtrFinalizer :: ForeignPtr a -> FunPtr (Ptr a -> IO ()) -> IO ()
addForeignPtrFinalizer p free = return ()

-- `withForeignObj' is a safer way to use `foreignPtrToPtr'.
withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr p k = k (foreignPtrToPtr p)
{- GHC implementation:
  do x <- k (foreignPtrToPtr p)
     touchForeignPtr p
     return x
-}

-- `foreignPtrToPtr' is a highly dangerous operation.  If the last
-- reference to the ForeignPtr disappears before the Ptr that has
-- been extracted from it is used, then the finaliser could run
-- rendering the Ptr invalid.
foreign import cast foreignPtrToPtr :: ForeignPtr a -> Ptr a

-- `Touching' a foreignObj is just intended to keep it alive across
-- calls which might otherwise allow it to be GC'ed.  Only really
-- an issue in GHC - for nhc98 a null-op is sufficient.
touchForeignPtr :: ForeignPtr a -> IO ()
touchForeignPtr p = return ()

castForeignPtr :: ForeignPtr a -> ForeignPtr b
castForeignPtr a = unsafeCoerce a

{- GHC extensions
mallocForeignPtr :: Storable a => IO (ForeignPtr a)
mallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
-}

