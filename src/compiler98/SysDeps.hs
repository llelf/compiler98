module SysDeps (
   module PS, trace
) where

#if __GLASGOW_HASKELL__ >= 502
import Data.PackedString as PS
#else
import PackedString as PS
#endif

#if defined(__NHC__) || defined(__HBC__)
import NonStdTrace (trace)
#elif __GLASGOW_HASKELL__ >= 502
import Debug.Trace (trace)
#else
import IOExts      (trace)
#endif
