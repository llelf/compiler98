{- Hey Emacs, this is -*- haskell -*- !
   @configure_input@
   $Id: FFI.hs,v 1.1 1999/12/16 11:01:45 malcolm Exp $
-}

module FFI
   -------------------------------------------------------------------
   -- {Int,Word}{8,16,32,64} are abstract and instance of Eq, Ord,
   -- Num, Bounded, Real, Integral, Ix, Enum, Read, Show
   -- NOTE 1: GHC has additional instances CCallable, CReturnable, Bits
   -- NOTE 2: No plain Word
-- Int8,  Int16,  Int32,  Int64,
-- Word8, Word16, Word32, Word64,

   -------------------------------------------------------------------
  ( Addr                  -- abstract, instance of: Eq, Ord, Enum, Show
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
  , foreignObjToAddr      -- :: ForeignObj -> IO Addr
-- ,addrToForeignObj      -- :: Addr       -> IO ForeignObj 
       
   -------------------------------------------------------------------
  , StablePtr             -- abstract
  , makeStablePtr         -- :: a -> IO (StablePtr a)
  , deRefStablePtr        -- :: StablePtr a -> IO a
  , freeStablePtr         -- :: StablePtr a -> IO ()
  , stablePtrToAddr       -- :: StablePtr a -> Addr
  , addrToStablePtr       -- :: Addr -> StablePtr a
  ) where

--import Int		-- not complete yet
--import Word		-- not complete yet
import Addr		-- new
import ForeignObj	-- new, note: type of finalizers is still wrong.
import StablePtr	-- new
import FixIO (fixIO)	-- eventually to be part of IOExts
import Monad(when)

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

{-
instance Storable Word8 where
   sizeOf        = const 1
   alignment     = sizeOf   -- not sure about this
   peekElemOffset  = readWord8OffAddr
   pokeElemOffset  = writeWord8OffAddr

instance Storable Word16 where
   sizeOf        = const 2
   alignment     = sizeOf   -- not sure about this
   peekElemOffset  = readWord16OffAddr
   pokeElemOffset  = writeWord16OffAddr

instance Storable Word32 where
   sizeOf        = const 4
   alignment     = sizeOf   -- not sure about this
   peekElemOffset  = readWord32OffAddr
   pokeElemOffset  = writeWord32OffAddr

instance Storable Word64 where
   sizeOf        = const 8
   alignment     = sizeOf   -- not sure about this
   peekElemOffset  = readWord64OffAddr
   pokeElemOffset  = writeWord64OffAddr

instance Storable Int8 where
   sizeOf        = const 1
   alignment     = sizeOf   -- not sure about this
   peekElemOffset  = readInt8OffAddr
   pokeElemOffset  = writeInt8OffAddr

instance Storable Int16 where
   sizeOf        = const 2
   alignment     = sizeOf   -- not sure about this
   peekElemOffset  = readInt16OffAddr
   pokeElemOffset  = writeInt16OffAddr

instance Storable Int32 where
   sizeOf        = const 4
   alignment     = sizeOf   -- not sure about this
   peekElemOffset  = readInt32OffAddr
   pokeElemOffset  = writeInt32OffAddr

instance Storable Int64 where
   sizeOf        = const 8
   alignment     = sizeOf   -- not sure about this
   peekElemOffset  = readInt64OffAddr
   pokeElemOffset  = writeInt64OffAddr
-}

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


