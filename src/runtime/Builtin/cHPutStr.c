#include "haskell2c.h"
#include <stdio.h>

/* foreign import hPutStrC :: IO.Handle -> PackedString -> IO () */
void hPutStrC (FileDesc *f, char *s)
{
    int err;
    err = fputs(s,f->fp);
 /* if (err==EOF) return mkLeft(mkInt(errno)); */
 /* else return mkRight(mkUnit); */
}
