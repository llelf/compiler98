module FFI
  ( Addr	-- abstract, instance of: Eq, Ord, Enum, Show
  , nullAddr	-- :: Addr
  , plusAddr	-- :: Addr -> Int -> Addr
  , addrToInt	-- :: Addr -> Int
  , intToAddr	-- :: Int -> Addr
  ) where

import FFIBuiltin (Addr)
--import ShowsIntBase
import Numeric (showIntBase)

foreign cast addrToInt :: Addr -> Int
foreign cast intToAddr :: Int  -> Addr


instance Eq   Addr where
  x == y   =  addrToInt x == addrToInt y
instance Ord  Addr where
  compare x y   =  compare (addrToInt x) (addrToInt y)
instance Show Addr where
  showsPrec p a = showString "0x" . showIntBase 16 (addrToInt a)
instance Enum Addr where
  toEnum   = intToAddr
  fromEnum = addrToInt


nullAddr :: Addr
plusAddr :: Addr -> Int -> Addr

nullAddr     = intToAddr 0
plusAddr a i = intToAddr (addrToInt a + i)
