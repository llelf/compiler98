module Prelude (ioError) where

import DIO

ioError :: IOError -> IO a
ioError e = IO ( \ world -> Left e )
