module FFI
  ( malloc       -- :: Storable a =>        IO (Ptr a)
  , mallocBytes  -- ::               Int -> IO (Ptr a)
  , alloca       -- :: Storable a =>        (Ptr a -> IO b) -> IO b
  , allocaBytes  -- ::               Int -> (Ptr a -> IO b) -> IO b
  , reallocBytes -- :: Ptr a -> Int -> IO (Ptr a)
  , free         -- :: Ptr a -> IO ()
  ) where

import Ptr
import Storable
import CError
import CTypes
import CTypesExtra (CSize)
import DErrNo

import IO (bracket)
import Monad (when)

-- allocate space for storable type
--
malloc :: Storable a => IO (Ptr a)
malloc  = doMalloc undefined
  where
    doMalloc       :: Storable a => a -> IO (Ptr a)
    doMalloc dummy  = mallocBytes (sizeOf dummy)

-- allocate given number of bytes of storage
--
mallocBytes      :: Int -> IO (Ptr a)
mallocBytes size  = failWhenNULL "malloc" (_malloc (fromIntegral size))

-- temporarily allocate space for a storable type
--
-- * the pointer passed as an argument to the function must *not* escape from
--   this function; in other words, in `alloca f' the allocated storage must
--   not be used after `f' returns
--
alloca :: Storable a => (Ptr a -> IO b) -> IO b
alloca  = doAlloca undefined
  where
    doAlloca       :: Storable a => a -> (Ptr a -> IO b) -> IO b
    doAlloca dummy  = allocaBytes (sizeOf dummy)

-- temporarily allocate the given number of bytes of storage
--
-- * the pointer passed as an argument to the function must *not* escape from
--   this function; in other words, in `allocaBytes n f' the allocated storage
--   must not be used after `f' returns
--
allocaBytes      :: Int -> (Ptr a -> IO b) -> IO b
allocaBytes size  = bracket (mallocBytes size) free

-- adjust a malloc'ed storage area to the given size
--
reallocBytes          :: Ptr a -> Int -> IO (Ptr a)
reallocBytes ptr size  = 
  failWhenNULL "realloc" (_realloc ptr (fromIntegral size))

-- free malloc'ed storage
--
free :: Ptr a -> IO ()
free  = _free


---------------------------------------------------------------------------
-- utility functions, not exported

failWhenNULL :: String -> IO (Ptr a) -> IO (Ptr a)
failWhenNULL nm f = do
   addr <- f
   when (addr == nullPtr)
        (throwIOError (nm++" out of memory") Nothing Nothing (fromEnum ENOMEM))
   return addr

-- Hmmm, Int is a little bit strange here, C uses size_t
foreign import ccall unsafe "malloc"  _malloc  :: CSize -> IO (Ptr a)
foreign import ccall unsafe "realloc" _realloc :: Ptr a -> CSize -> IO (Ptr a)
foreign import ccall unsafe "free"    _free    :: Ptr a -> IO ()


