module System where

import DIO
import System(ExitCode)

primExitWith code =
  IO ( \ world -> _cExitWith code)

_cExitWith code = _prim _tprim_cExitWith code

_tprim_cExitWith primitive 2 :: Trace -> R ExitCode -> R (Either IOError a)
