module Prelude where


#if !defined(TRACING)
_ioReturn :: a -> IO a
_ioReturn = return
#else

import DIO

_ioReturn :: a -> IO a
_ioReturn x = IOPrim (R x Pruned)

-- Earlier implementation, now incorporated into class NmCoerce.
--   _ioReturn x = IO (R (\t rw-> R (Right (R x t)) t) Root)

#endif
