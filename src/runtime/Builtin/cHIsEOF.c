#include <errno.h>
#include "haskell2c.h"

/* hIsEOF primitive 1 :: Handle -> (Either IOError Bool) */

C_HEADER(cHIsEOF)
{
  FileDesc *a;
  NodePtr nodeptr;
  int eof;
  
  C_CHECK(    sizeRight+(sizeTrue + sizeFalse));
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  a = derefForeignObj((ForeignObj*)(GET_INT_VALUE(nodeptr)));
#ifdef PROFILE
  if(replay) {
    REPLAY_BOOL(eof);
  } else
#endif
    eof = feof(a->fp);
#ifdef PROFILE
  if(record) {
    RECORD_BOOL(eof);
  }
#endif

  nodeptr = mkRight(mkBool(eof));

  C_RETURN(nodeptr);
}	
