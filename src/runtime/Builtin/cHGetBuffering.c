#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "haskell2c.h"

#if 0
/* cHGetBuffering primitive 1 :: Handle -> Either IOError BufferMode */
C_HEADER(cHGetBuffering)
{
  FileDesc *a;
  NodePtr nodeptr;
  int bm,err;
   
  C_CHECK(nhc_sizeRight + nhc_sizeNoBuffering+nhc_sizeBlockBuffering+nhc_sizeJust+nhc_sizeInt + nhc_sizeLeft+nhc_sizeIOErrorHIsEOF);
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  a = derefForeignObj((ForeignObj*)(GET_INT_VALUE(nodeptr)));

  switch(a->bm) {
  case _IONBF: nodeptr = nhc_mkRight(nhc_mkNoBuffering()); break;
  case _IOLBF: nodeptr = nhc_mkRight(nhc_mkLineBuffering()); break;
  case _IOFBF:
    if(a->size>=0)
      nodeptr = nhc_mkRight(nhc_mkBlockBuffering(nhc_mkJust(nhc_mkInt(a->size))));
    else
      nodeptr = nhc_mkRight(nhc_mkBlockBuffering(nhc_mkNothing()));
    break;
  default: nodeptr = nhc_mkLeft(nhc_mkIOErrorHGetBuffering(C_GETARG1(1),nhc_mkInt(a->bm)));
  }
  C_RETURN(nodeptr);
}
#endif

/* foreign import hGetBufferingC :: Handle -> IO BufferMode */
NodePtr hGetBufferingC (FileDesc *f)
{
  switch(f->bm) {
    case _IONBF: return nhc_mkNoBuffering(); break;
    case _IOLBF: return nhc_mkLineBuffering(); break;
    case _IOFBF:
        if(f->size>=0) return nhc_mkBlockBuffering(nhc_mkJust(nhc_mkInt(f->size)));
        else           return nhc_mkBlockBuffering(nhc_mkNothing());
        break;
    default: break;
  }
}
