module IO (stdin,stdout,stderr) where


#if !defined(TRACING)
import PreludeBuiltin (stdin,stdout,stderr)

#else

import PreludeBuiltin
import DHandle

stdin :: SR -> Trace -> R Handle
stdin sr t = R (Handle _stdin) t

stdout :: SR -> Trace -> R Handle
stdout sr t = R (Handle _stdout) t

stderr :: SR -> Trace -> R Handle
stderr sr t = R (Handle _stderr) t

#endif
