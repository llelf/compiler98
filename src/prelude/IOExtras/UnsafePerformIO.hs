module IOExtras (unsafePerformIO) where

import DIO
import DIOError
import Show_IOError

unsafePerformIO :: IO a -> a
unsafePerformIO (IO f) =
  case f World of
    Left err -> error ("unsafePerformIO: "++show err)
    Right a  -> a
