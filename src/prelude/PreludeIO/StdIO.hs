module Prelude(stdin,stdout,stderr) where

#if defined(TRACING)

{- Only used when tracing. -}

import PreludeBuiltin
import IO

stdin :: SR -> Trace -> R Handle
stdin sr t = _stdin

stdout :: SR -> Trace -> R Handle
stdout sr t = _stdout

stderr :: SR -> Trace -> R Handle
stderr sr t = _stderr

#endif