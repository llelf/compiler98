module Directory where

import IO
import DIO
import CString

doesFileExist              :: FilePath -> IO Bool
doesFileExist fp =
  IO (\world -> cDoesFileExist (toCString fp))

cDoesFileExist primitive 1 :: PackedString -> Either IOError Bool

{-
import GreenCard

%-#include <sys/stat.h>
%-#include <unistd.h>

%fun doesFileExist :: FilePath -> IO Bool
%call (filePath fp)
%code
%  struct stat st;
%  int err;
%  err = stat(fp,&st);
%  if ((err==0) && S_ISREG(st.st_mode)) return mkTrue();
%  else return mkFalse();
%result (bool b)

-}
