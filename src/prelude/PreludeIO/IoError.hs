module Prelude (ioError) where

import DIO

ioError :: IOError -> IO a
ioError e = IO ( const (Left e) )
        --  IO ( \ world -> Left e )	-- const is better for nice tracing
