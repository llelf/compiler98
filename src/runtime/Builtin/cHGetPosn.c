#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>

#include "haskell2c.h"

/* cHGetPosn primitive 1 :: Handle -> Either IOError HandlePosn */

C_HEADER(cHGetPosn)
{
  Arg *a;
  NodePtr nodeptr;
  int err;
  int posn;
  fpos_t fpos;
  
  C_CHECK(sizeRight+sizeHandlePosn + sizeLeft+sizeIOErrorHGetPosn);
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  a = cdataArg((CData *)(GET_INT_VALUE(nodeptr)));

#ifdef PROFILE
  if(replay) {
    REPLAY_BOOL(err);
    if(err)
      REPLAY(errno);
    else
      REPLAY(fpos);
  } else
#endif
    err = fgetpos(a->fp,&fpos);
#ifdef PROFILE
  if(record) {
    RECORD_BOOL(err);
    if(err)
      RECORD(errno);
    else
      RECORD(fpos);
  }
#endif

  if(!err)
    nodeptr = mkRight(mkHandlePosn(&fpos));
  else
    nodeptr = mkLeft(mkIOErrorHGetPosn(C_GETARG1(1),mkInt(errno)));
  
  C_RETURN(nodeptr);
}	
