#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>

#include "haskell2c.h"

/* cHFlush 1 :: Handle -> (Either IOError ()) */

C_HEADER(cHFlush)
{
  Arg *a;
  NodePtr nodeptr;
  int err;
  
  C_CHECK(sizeRight+sizeUnit + sizeLeft+sizeIOErrorHFlush);
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  a = cdataArg((CData *)(GET_INT_VALUE(nodeptr)));

#ifdef PROFILE
  if(replay) {
    REPLAY_BOOL(err);
    if(err) 
      REPLAY(errno);
  } else
#endif
    err = fflush(a->fp);
#ifdef PROFILE
  if(record) {
    RECORD_BOOL(err);
    if(err)
      RECORD(errno);
  }
#endif

  if(err)
    nodeptr = mkLeft(mkIOErrorHFlush(C_GETARG1(1),mkInt(errno)));
  else
    nodeptr = mkRight(mkUnit());
  
  C_RETURN(nodeptr);
}	
