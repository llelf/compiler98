module System where

import System
import DIO
import CExitWith

primExitWith code =
  IO ( \ world -> cExitWith code)
