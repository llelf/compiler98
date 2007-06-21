module SysDeps (
   module PackedString, trace, openBinaryFileWrite, osName
) where

#if __GLASGOW_HASKELL__ >= 502
import Data.PackedString as PackedString
#else
import PackedString
#endif

#if defined(__NHC__) || defined(__HBC__)
import NonStdTrace (trace)
#elif __GLASGOW_HASKELL__ >= 502
import Debug.Trace (trace)
#else
import IOExts      (trace)
#endif

import System.IO
import System.Info

openBinaryFileWrite :: FilePath -> IO Handle
openBinaryFileWrite f = openBinaryFile f WriteMode

osName :: String
osName = if compilerName == "yhc" || os /= "mingw32"
         then os
         else "windows"
