#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "haskell2c.h"

/* cHGetPosn primitive 1 :: Handle -> HandlePosn -> Either IOError () */

C_HEADER(cHSetPosn)
{
  FileDesc *a;
  NodePtr nodeptr;
  int err;
  int posn;
  fpos_t *fpos;
  
  C_CHECK(sizeRight+sizeUnit + sizeLeft+sizeIOErrorHSetPosn);
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  a = derefForeignObj((ForeignObj*)(GET_INT_VALUE(nodeptr)));

  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  fpos = getHandlePosn(nodeptr);

#ifdef PROFILE
  if(replay) {
    REPLAY_BOOL(err);
    if(err)
      REPLAY(errno);
  } else
#endif
    err = fsetpos(a->fp,fpos);
#ifdef PROFILE
  if(record) {
    RECORD_BOOL(err);
    if(err)
      RECORD(errno);
  }
#endif

  if(err)
    nodeptr = mkLeft(mkIOErrorHSetPosn(C_GETARG1(1),mkInt(errno)));
  else
    nodeptr = mkRight(mkUnit());
  
  C_RETURN(nodeptr);
}	
