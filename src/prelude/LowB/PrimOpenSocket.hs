module Prelude(cOpenSocket,primOpenSocket,SocketType) where

import IO
import CString
import DIO
import DSocket

cOpenSocket primitive 3 :: CString -> Int -> SocketType -> (Either IOError Handle)

primOpenSocket :: String -> Int -> SocketType -> IO Handle
primOpenSocket host port stype = 
  IO ( \ world -> cOpenSocket (toCString host) port stype )
