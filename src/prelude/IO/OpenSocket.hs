module IO where

import IO
import LowIO(primOpenSocket)

openSocket                 :: String -> Int -> SocketType -> IO Handle
openSocket host port stype = primOpenSocket host port stype

