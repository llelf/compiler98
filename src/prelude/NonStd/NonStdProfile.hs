module NonStdProfile where

import CString

cProfile primitive 1 :: CString -> ()

profile :: String -> a -> a
profile msg a =
  seq (cProfile (toCString msg)) a


