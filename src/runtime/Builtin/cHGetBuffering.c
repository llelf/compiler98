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
   
  C_CHECK(sizeRight + sizeNoBuffering+sizeBlockBuffering+sizeJust+sizeInt + sizeLeft+sizeIOErrorHIsEOF);
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  a = derefForeignObj((ForeignObj*)(GET_INT_VALUE(nodeptr)));

  switch(a->bm) {
  case _IONBF: nodeptr = mkRight(mkNoBuffering()); break;
  case _IOLBF: nodeptr = mkRight(mkLineBuffering()); break;
  case _IOFBF:
    if(a->size>=0)
      nodeptr = mkRight(mkBlockBuffering(mkJust(mkInt(a->size))));
    else
      nodeptr = mkRight(mkBlockBuffering(mkNothing()));
    break;
  default: nodeptr = mkLeft(mkIOErrorHGetBuffering(C_GETARG1(1),mkInt(a->bm)));
  }
  C_RETURN(nodeptr);
}
#endif

/* foreign import hGetBufferingC :: Handle -> IO BufferMode */
NodePtr hGetBufferingC (FileDesc *f)
{
  switch(f->bm) {
    case _IONBF: return mkNoBuffering(); break;
    case _IOLBF: return mkLineBuffering(); break;
    case _IOFBF:
        if(f->size>=0) return mkBlockBuffering(mkJust(mkInt(f->size)));
        else           return mkBlockBuffering(mkNothing());
        break;
    default: break;
  }
}
