module FFI
  ( Storable
      ( sizeOf		-- :: a -> Int
      , alignment	-- :: a -> Int
      , peekElemOff	-- :: Ptr a -> Int      -> IO a
      , pokeElemOff	-- :: Ptr a -> Int -> a -> IO ()
      , peekByteOff	-- :: Ptr b -> Int      -> IO a
      , pokeByteOff	-- :: Ptr b -> Int -> a -> IO ()
      , peek		-- :: Ptr a             -> IO a
      , poke		-- :: Ptr a        -> a -> IO ()
      , destruct	-- :: Ptr a             -> IO ()
      )
  ) where

import Int        (Int8, Int16, Int32, Int64)
import Word       (Word8, Word16, Word32, Word64)
import Ptr        (Ptr, plusPtr)
--import StablePtr  (StablePtr)
--import CTypes
--import CTypesISO

class Storable a where

   -- Yields the storage requirements (in bytes) of the argument.
   -- * Never uses its argument.
   sizeOf      :: a -> Int

   -- Yields the alignment constraint of the argument.
   -- * An alignment constraint x is fulfilled by any address divisible by x.
   -- * Never uses its argument.
   alignment   :: a -> Int

   -- Read/write elements from an array of elements of the given type.
   peekElemOff :: Ptr a -> Int      -> IO a
   pokeElemOff :: Ptr a -> Int -> a -> IO ()

   -- The same with *byte* offsets.
   peekByteOff :: Ptr b -> Int      -> IO a
   pokeByteOff :: Ptr b -> Int -> a -> IO ()

   -- ... and with no offsets at all.
   peek        :: Ptr a      -> IO a
   poke        :: Ptr a -> a -> IO ()

   -- Free memory associated with the object
   -- (except the object pointer itself).
   destruct    :: Ptr a -> IO ()

   -- circular default instances
   peekElemOff = peekElemOff_ undefined
      where peekElemOff_ :: Storable a => a -> Ptr a -> Int -> IO a
            peekElemOff_ undef ptr off = peekByteOff ptr (off * sizeOf undef)
   pokeElemOff ptr off val = pokeByteOff ptr (off * sizeOf val) val

   peekByteOff ptr off = peek (ptr `plusPtr` off)
   pokeByteOff ptr off = poke (ptr `plusPtr` off)

   peek ptr = peekElemOff ptr 0
   poke ptr = pokeElemOff ptr 0

-- Note that the various `peek' and `poke' functions might require properly
-- aligned addresses to function correctly. This is architecture dependent;
-- thus, portable code should ensure that when peeking or poking values of
-- some type `a', the alignment constraint for `a', as given by the function
-- alignment is fulfilled.

   destruct _ = return ()

----------------------------------------------------------------------
-- system-dependent instances

foreign import readCharAtAddr  :: Ptr Char -> IO Char
foreign import writeCharAtAddr :: Ptr Char -> Char -> IO ()

instance Storable Char where
   sizeOf        = const 1
   alignment     = const 1
   peek          = readCharAtAddr
   poke          = writeCharAtAddr

foreign import readIntAtAddr  :: Ptr Int -> IO Int
foreign import writeIntAtAddr :: Ptr Int -> Int -> IO ()

instance Storable Int where
   sizeOf        = const 4
   alignment     = const 4
   peek          = readIntAtAddr
   poke          = writeIntAtAddr

foreign import readAddrAtAddr  :: Ptr (Ptr a) -> IO (Ptr a)
foreign import writeAddrAtAddr :: Ptr (Ptr a) -> Ptr a -> IO ()

instance Storable (Ptr a) where
   sizeOf        = const 4
   alignment     = const 4
   peek          = readAddrAtAddr
   poke          = writeAddrAtAddr

foreign import readFloatAtAddr  :: Ptr Float -> IO Float
foreign import writeFloatAtAddr :: Ptr Float -> Float -> IO ()

instance Storable Float where
   sizeOf        = const 4
   alignment     = const 4
   peek          = readFloatAtAddr
   poke          = writeFloatAtAddr

foreign import readDoubleAtAddr  :: Ptr Double -> IO Double
foreign import writeDoubleAtAddr :: Ptr Double -> Double -> IO ()

instance Storable Double where
   sizeOf        = const 8
   alignment     = const 8
   peek          = readDoubleAtAddr
   poke          = writeDoubleAtAddr

foreign import readWord8AtAddr  :: Ptr Word8 -> IO Word8
foreign import writeWord8AtAddr :: Ptr Word8 -> Word8 -> IO ()

instance Storable Word8 where
   sizeOf        = const 1
   alignment     = sizeOf   -- not sure about this
   peek          = readWord8AtAddr
   poke          = writeWord8AtAddr

foreign import readWord16AtAddr  :: Ptr Word16 -> IO Word16
foreign import writeWord16AtAddr :: Ptr Word16 -> Word16 -> IO ()

instance Storable Word16 where
   sizeOf        = const 2
   alignment     = sizeOf   -- not sure about this
   peek          = readWord16AtAddr
   poke          = writeWord16AtAddr

foreign import readWord32AtAddr  :: Ptr Word32 -> IO Word32
foreign import writeWord32AtAddr :: Ptr Word32 -> Word32 -> IO ()

instance Storable Word32 where
   sizeOf        = const 4
   alignment     = sizeOf   -- not sure about this
   peek          = readWord32AtAddr
   poke          = writeWord32AtAddr

foreign import readWord64AtAddr  :: Ptr Word64 -> IO Word64
foreign import writeWord64AtAddr :: Ptr Word64 -> Word64 -> IO ()

instance Storable Word64 where
   sizeOf        = const 8
   alignment     = sizeOf   -- not sure about this
   peek          = readWord64AtAddr
   poke          = writeWord64AtAddr

foreign import readInt8AtAddr  :: Ptr Int8 -> IO Int8
foreign import writeInt8AtAddr :: Ptr Int8 -> Int8 -> IO ()

instance Storable Int8 where
   sizeOf        = const 1
   alignment     = sizeOf   -- not sure about this
   peek          = readInt8AtAddr
   poke          = writeInt8AtAddr

foreign import readInt16AtAddr  :: Ptr Int16 -> IO Int16
foreign import writeInt16AtAddr :: Ptr Int16 -> Int16 -> IO ()

instance Storable Int16 where
   sizeOf        = const 2
   alignment     = sizeOf   -- not sure about this
   peek          = readInt16AtAddr
   poke          = writeInt16AtAddr

foreign import readInt32AtAddr  :: Ptr Int32 -> IO Int32
foreign import writeInt32AtAddr :: Ptr Int32 -> Int32 -> IO ()

instance Storable Int32 where
   sizeOf        = const 4
   alignment     = sizeOf   -- not sure about this
   peek          = readInt32AtAddr
   poke          = writeInt32AtAddr

foreign import readInt64AtAddr  :: Ptr Int64 -> IO Int64
foreign import writeInt64AtAddr :: Ptr Int64 -> Int64 -> IO ()

instance Storable Int64 where
   sizeOf        = const 8
   alignment     = sizeOf   -- not sure about this
   peek          = readInt64AtAddr
   poke          = writeInt64AtAddr

---------------------------------------------------------------------------

