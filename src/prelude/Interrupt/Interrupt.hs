module Interrupt ( setUserInterrupt ) where

setUserInterrupt   :: Maybe (IO ()) -> IO (Maybe (IO ()))
setUserInterrupt   handle = error "Not implemented: setUserInterrupt"
