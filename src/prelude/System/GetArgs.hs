module System where

import FFI

foreign import cGetArg       :: IO Addr
foreign cast   addrToCString :: Addr -> IO PackedString

getArgs  :: IO [String]
getArgs =
-- The use of unsafePerformIO followed by return is a slightly bizarre
-- way of ensuring that the stateful computation gets executed once
-- and once only.
  let args = unsafePerformIO (getThem ())
  in return args
 where
  getThem () = do
    a <- cGetArg
    if (a==nullAddr) then return []
      else do arg <- addrToCString a
              args <- getThem ()
              return (fromCString arg:args)
