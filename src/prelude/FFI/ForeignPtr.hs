module FFI
    ( ForeignPtr		-- abstract, instance of: Eq
    , newForeignPtr		-- :: Ptr a -> IO () -> IO (ForeignPtr a)
 -- , addForeignPtrFinalizer	-- :: ForeignPtr a -> IO () -> IO ()
    , touchForeignPtr		-- :: ForeignPtr a -> IO ()
    , withForeignPtr		-- :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
    , foreignPtrToPtr		-- :: ForeignPtr a -> Ptr a
    , castForeignPtr		-- :: ForeignPtr a -> ForeignPtr b
    ) 
    where

import ForeignObj
import Ptr

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
