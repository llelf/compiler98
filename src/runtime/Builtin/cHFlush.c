#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "haskell2c.h"
#if TRACE
#include "../../tracer/runtime/getconstr.h"
#endif

#if 0
/* cHFlush 1 :: Handle -> (Either IOError ()) */
C_HEADER(cHFlush)
{
  FileDesc *a;
  NodePtr nodeptr;
  int err;
  
  C_CHECK(sizeRight+sizeUnit + sizeLeft+sizeIOErrorHFlush);
  
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
    nodeptr = mkLeft(mkIOErrorHFlush(C_GETARG1(1),mkInt(errno)));
  else
    nodeptr = mkRight(mkUnit());
  
  C_RETURN(nodeptr);
}
#endif

/* foreign import hFlushC :: Handle -> Either Int () */
NodePtr hFlushC (FileDesc* a)
{
  int err;
  err = fflush(a->fp);
#if !TRACE
  if (err)
    return mkLeft(mkInt(errno));
  else
    return mkRight(mkUnit());
#else
  if (err)
    return mkLeft(mkR(mkInt(errno),mkTNm(0,mkNmInt(mkInt(errno)),mkSR())));
  else
    return mkRight(mkR(mkUnit(),mkTNm(0,mkNmUnit(),mkSR())));
#endif
}
