module Prelude where

import DIO

fail :: IOError -> IO a
fail e = IO ( \ world -> Left e )
