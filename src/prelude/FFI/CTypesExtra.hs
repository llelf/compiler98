module NHC.FFI 
  -- Integral types, instances of: Eq, Ord, Num, Read, Show, Enum,
  -- Storable, Bounded, Real, Integral
  ( CPtrdiff(..), CSize(..), CWchar(..), CSigAtomic(..)

  -- Numeric types, instances of: Eq, Ord, Num, Read, Show, Enum, Storable
  , CClock(..),   CTime(..)
  , CFile,        CFpos,     CJmpBuf

  -- C99 types which are still missing include:
  -- intptr_t, uintptr_t, intmax_t, uintmax_t, wint_t, wctrans_t, wctype_t

  ) where

import Int
import Word
import CTypes

-- So far, we just make a best guess at these types for most 32-bit machines.
-- Should really be auto-configured.

newtype CPtrdiff   = CPtrdiff CInt	-- ??
newtype CWchar     = CWchar CInt	-- ??
newtype CSigAtomic = CSigAtomic CInt	-- ??

-- Should be newtypes perhaps?
type CSize      = CInt		-- size_t
type CClock     = CUInt		-- clock_t
type CTime      = CUInt		-- time_t

-- Not sure what these are for??
data CFile   = CFile		-- ??
data CFpos   = CFpos		-- ??
data CJmpBuf = CJmpBuf		-- ??
