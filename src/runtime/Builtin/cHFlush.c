#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "haskell2c.h"
#if TRACE
#include "../../hat/runtime/getconstr.h"
#endif

#if 0
/* cHFlush 1 :: Handle -> (Either IOError ()) */
C_HEADER(cHFlush)
{
  FileDesc *a;
  NodePtr nodeptr;
  int err;
  
  C_CHECK(nhc_sizeRight+nhc_sizeUnit + nhc_sizeLeft+nhc_sizeIOErrorHFlush);
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  a = derefForeignObj((ForeignObj*)(GET_INT_VALUE(nodeptr)));

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
    nodeptr = nhc_mkLeft(nhc_mkIOErrorHFlush(C_GETARG1(1),nhc_mkInt(errno)));
  else
    nodeptr = nhc_mkRight(nhc_mkUnit());
  
  C_RETURN(nodeptr);
}
#endif

/* foreign import hFlushC :: Handle -> Int */
int hFlushC (FileDesc* a)
{
  return fflush(a->fp);
}
