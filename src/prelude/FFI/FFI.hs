{- Hey Emacs, this is -*- haskell -*- !
   @configure_input@
   $Id: FFI.hs,v 1.8 2001/02/25 16:17:16 malcolm Exp $
-}

module FFI
   -------------------------------------------------------------------
   -- {Int,Word}{8,16,32,64} are abstract and instance of Eq, Ord,
   -- Num, Bounded, Real, Integral, Ix, Enum, Read, Show
   -- NOTE 1: GHC has additional instances CCallable, CReturnable, Bits
   -- NOTE 2: No plain Word
  ( Int8,  Int16,  Int32,  Int64
  , Word8, Word16, Word32, Word64

   -------------------------------------------------------------------
  , Addr                  -- abstract, instance of: Eq, Ord, Enum, Show
  , nullAddr              -- :: Addr
  , plusAddr              -- :: Addr -> Int -> Addr
  , Storable
      ( sizeOf            -- :: a -> Int
      , alignment         -- :: a -> Int
      , peekElemOff       -- :: Addr -> Int      -> IO a
      , pokeElemOff       -- :: Addr -> Int -> a -> IO ()
      , peekByteOff       -- :: Addr -> Int      -> IO a
      , pokeByteOff       -- :: Addr -> Int -> a -> IO ()
      , peek              -- :: Addr             -> IO a
      , poke)             -- :: Addr        -> a -> IO ()

   -------------------------------------------------------------------
  , malloc                -- ::               Int      -> IO Addr
  , mallocElem            -- :: Storable a => a        -> IO Addr
  , mallocElems           -- :: Storable a => a -> Int -> IO Addr
  , realloc               -- :: Addr -> Int -> IO Addr
  , free                  -- :: Addr -> IO ()

   -------------------------------------------------------------------
  , ForeignObj            -- abstract, instance of: Eq
  , makeForeignObj        -- :: Addr ->        IO()  -> IO ForeignObj
-- ,makeForeignObj        -- :: Addr -> (Addr->IO()) -> IO ForeignObj
-- ,makeForeignObj        -- :: Addr ->  Addr        -> IO ForeignObj
-- ,writeForeignAddr      -- :: ForeignObj -> Addr         -> IO ()
-- ,writeForeignFinalizer -- :: ForeignObj -> (Addr->IO()) -> IO ()
-- ,writeForeignFinalizer -- :: ForeignObj ->  Addr        -> IO ()
  , foreignObjToAddr      -- :: ForeignObj -> Addr
-- ,addrToForeignObj      -- :: Addr       -> IO ForeignObj 
#if !defined(TRACING)
   ,freeForeignObj        -- :: ForeignObj -> IO ()
#endif
  , withForeignObj        -- :: ForeignObj -> (Addr -> IO a) -> IO a
  , touchForeignObj       -- :: ForeignObj -> IO ()

   -------------------------------------------------------------------
#if !defined(TRACING)
  , StablePtr             -- abstract
  , makeStablePtr         -- :: a -> IO (StablePtr a)
  , deRefStablePtr        -- :: StablePtr a -> IO a
  , freeStablePtr         -- :: StablePtr a -> IO ()
  , stablePtrToAddr       -- :: StablePtr a -> Addr
  , addrToStablePtr       -- :: Addr -> StablePtr a
#endif
   -------------------------------------------------------------------
  , CString		-- abstract
  , toCString		-- :: String  -> CString
  , fromCString		-- :: CString -> String

   -------------------------------------------------------------------
  , getErrNo		-- :: IO Int
  , mkIOError		-- :: String -> Maybe FilePath -> Maybe Handle
			--      -> Int -> IO a
   -------------------------------------------------------------------
  ) where

import Int		-- believed complete now
import Word		-- 
import Addr		--
import ForeignObj	--
#if !defined(TRACING)
import StablePtr	-- only works for non-tracing so far
#endif
import CString		-- nhc98-only
import CError		-- nhc98-only
import FixIO (fixIO)	-- part of IOExtras, but IOExtras depends on FFI.
import Monad (when)

----------------------------------------------------------------------
-- primitive marshaling

-- Minimal complete definition: sizeOf, alignment and one definition
-- in each of the deref/peek/poke families.
class Storable a where
   -- sizeOf/alignment *never* use their first argument
   sizeOf          :: a -> Int
   alignment       :: a -> Int
   -- replacement for index-/read-/write???OffAddr
   peekElemOff     :: Addr -> Int      -> IO a
   pokeElemOff     :: Addr -> Int -> a -> IO ()
   -- the same with *byte* offsets
   peekByteOff     :: Addr -> Int      -> IO a
   pokeByteOff     :: Addr -> Int -> a -> IO ()
   -- ... and with no offsets at all
   peek            :: Addr             -> IO a
   poke            :: Addr        -> a -> IO ()

   -- circular default instances
   peekElemOff addr off =
      fixIO (\val -> peekByteOff addr (off * sizeOf val))
   pokeElemOff addr off val =
      pokeByteOff addr (off * sizeOf val) val

   peekByteOff addr off = peek  (addr `plusAddr` off)
   pokeByteOff addr off = poke  (addr `plusAddr` off)

   peek  addr = peekElemOff addr 0
   poke  addr = pokeElemOff addr 0

---------------------------------------------------------------------------
-- system-dependent, but rather obvious instances

foreign import readCharAtAddr  :: Addr -> IO Char
foreign import writeCharAtAddr :: Addr -> Char -> IO ()

instance Storable Char where
   sizeOf        = const 1
   alignment     = const 1
   peek          = readCharAtAddr
   poke          = writeCharAtAddr

foreign import readIntAtAddr  :: Addr -> IO Int
foreign import writeIntAtAddr :: Addr -> Int -> IO ()

instance Storable Int where
   sizeOf        = const 4
   alignment     = const 4
   peek          = readIntAtAddr
   poke          = writeIntAtAddr

foreign import readAddrAtAddr  :: Addr -> IO Addr
foreign import writeAddrAtAddr :: Addr -> Addr -> IO ()

instance Storable Addr where
   sizeOf        = const 4
   alignment     = const 4
   peek          = readAddrAtAddr
   poke          = writeAddrAtAddr

foreign import readFloatAtAddr  :: Addr -> IO Float
foreign import writeFloatAtAddr :: Addr -> Float -> IO ()

instance Storable Float where
   sizeOf        = const 4
   alignment     = const 4
   peek          = readFloatAtAddr
   poke          = writeFloatAtAddr

foreign import readDoubleAtAddr  :: Addr -> IO Double
foreign import writeDoubleAtAddr :: Addr -> Double -> IO ()

instance Storable Double where
   sizeOf        = const 8
   alignment     = const 8
   peek          = readDoubleAtAddr
   poke          = writeDoubleAtAddr

foreign import readWord8AtAddr  :: Addr -> IO Word8
foreign import writeWord8AtAddr :: Addr -> Word8 -> IO ()

instance Storable Word8 where
   sizeOf        = const 1
   alignment     = sizeOf   -- not sure about this
   peek          = readWord8AtAddr
   poke          = writeWord8AtAddr

foreign import readWord16AtAddr  :: Addr -> IO Word16
foreign import writeWord16AtAddr :: Addr -> Word16 -> IO ()

instance Storable Word16 where
   sizeOf        = const 2
   alignment     = sizeOf   -- not sure about this
   peek          = readWord16AtAddr
   poke          = writeWord16AtAddr

foreign import readWord32AtAddr  :: Addr -> IO Word32
foreign import writeWord32AtAddr :: Addr -> Word32 -> IO ()

instance Storable Word32 where
   sizeOf        = const 4
   alignment     = sizeOf   -- not sure about this
   peek          = readWord32AtAddr
   poke          = writeWord32AtAddr

foreign import readWord64AtAddr  :: Addr -> IO Word64
foreign import writeWord64AtAddr :: Addr -> Word64 -> IO ()

instance Storable Word64 where
   sizeOf        = const 8
   alignment     = sizeOf   -- not sure about this
   peek          = readWord64AtAddr
   poke          = writeWord64AtAddr

foreign import readInt8AtAddr  :: Addr -> IO Int8
foreign import writeInt8AtAddr :: Addr -> Int8 -> IO ()

instance Storable Int8 where
   sizeOf        = const 1
   alignment     = sizeOf   -- not sure about this
   peek          = readInt8AtAddr
   poke          = writeInt8AtAddr

foreign import readInt16AtAddr  :: Addr -> IO Int16
foreign import writeInt16AtAddr :: Addr -> Int16 -> IO ()

instance Storable Int16 where
   sizeOf        = const 2
   alignment     = sizeOf   -- not sure about this
   peek          = readInt16AtAddr
   poke          = writeInt16AtAddr

foreign import readInt32AtAddr  :: Addr -> IO Int32
foreign import writeInt32AtAddr :: Addr -> Int32 -> IO ()

instance Storable Int32 where
   sizeOf        = const 4
   alignment     = sizeOf   -- not sure about this
   peek          = readInt32AtAddr
   poke          = writeInt32AtAddr

foreign import readInt64AtAddr  :: Addr -> IO Int64
foreign import writeInt64AtAddr :: Addr -> Int64 -> IO ()

instance Storable Int64 where
   sizeOf        = const 8
   alignment     = sizeOf   -- not sure about this
   peek          = readInt64AtAddr
   poke          = writeInt64AtAddr

---------------------------------------------------------------------------
-- (de-)allocation of raw bytes

malloc :: Int -> IO Addr
malloc numBytes = failWhenNULL "malloc" (malloc_ numBytes)

mallocElem :: Storable a => a -> IO Addr
mallocElem unused = malloc (sizeOf unused)

mallocElems :: Storable a => a -> Int -> IO Addr
mallocElems unused numElems = malloc (numElems * sizeOf unused)

realloc :: Addr -> Int -> IO Addr
realloc oldAddr numBytes =
   failWhenNULL "realloc" (realloc_ oldAddr numBytes)

foreign import ccall unsafe free :: Addr -> IO ()

---------------------------------------------------------------------------
-- utility functions, not exported

failWhenNULL :: String -> IO Addr -> IO Addr
failWhenNULL fun act = do
   addr <- act
   when (addr == nullAddr)
        (ioError (userError (fun ++ ": out of memory")))
   return addr

-- Hmmm, Int is a little bit strange here, C uses size_t
foreign import ccall "malloc"  unsafe malloc_  :: Int  -> IO Addr
foreign import ccall "realloc" unsafe realloc_ :: Addr -> Int -> IO Addr


