module Prelude(putChar) where

import IO
#if defined(TRACING)
--import DIO
--import DPrelude(SR, R(..), Trace, fun1, rseq, NmType(..))
#endif

putChar :: Char -> IO ()
putChar c = hPutChar stdout c


#if defined(TRACING)
{-
mycPutChar primitive 1 :: R Char -> ()
--mycPutChar primitive 1 :: Char -> a

putChar :: SR -> Trace -> R (Trace -> R Char -> R (IO ()))
putChar sr t = fun1 NTDummy pc sr t
    where pc t cr = rseq cr (case mycPutChar cr of
                               () -> R (IO (R (cont t) t)) t)
          cont t x y = R (Right (R () t)) t


--ap1 sr t (ioOK sr t) (con sr t ())
-}
#endif
