module GcodeOpt2(gcodeOpt2) where

import Gcode
import AssocTree
import Extra(sndOf)

gcodeOpt2 state gcode = (gcode, state)
