#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>

#include "haskell2c.h"

/* cHGetBuffering primitive 1 :: Handle -> Either IOError BufferMode */

C_HEADER(cHGetBuffering)
{
  Arg *a;
  NodePtr nodeptr;
  int bm,err;
   
  C_CHECK(sizeRight + sizeNoBuffering+sizeBlockBuffering+sizeJust+sizeInt + sizeLeft+sizeIOErrorHIsEOF);
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  a = cdataArg((CData *)(GET_INT_VALUE(nodeptr)));

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
