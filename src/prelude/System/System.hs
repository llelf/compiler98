module System ( 
      ExitCode(..)
#if !defined(TRACING)
     ,getArgs
     ,getProgName
     ,getEnv
     ,system
#endif
     ,exitWith
     ,exitFailure
#if !defined(TRACING)
     ,system
#endif
     ) where

import DExitCode
import Ord_ExitCode
import Show_ExitCode
import Eq_ExitCode
import Read_ExitCode
#if !defined(TRACING)
import GetArgs
import GetProgName
import GetEnv
#endif
import ExitWith
import ExitFailure
#if !defined(TRACING)
import SystemFun
#endif

