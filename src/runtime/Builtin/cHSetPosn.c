#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "haskell2c.h"
#if TRACE
#include "../../hat/runtime/getconstr.h"
#endif

#if 0
/* cHSetPosn primitive 1 :: Handle -> HandlePosn -> Either IOError () */
C_HEADER(cHSetPosn)
{
  FileDesc *a;
  NodePtr nodeptr;
  int err;
  int posn;
  fpos_t *fpos;
  
  C_CHECK(nhc_sizeRight+nhc_sizeUnit + nhc_sizeLeft+nhc_sizeIOErrorHSetPosn);
  
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
    nodeptr = nhc_mkLeft(nhc_mkIOErrorHSetPosn(C_GETARG1(1),nhc_mkInt(errno)));
  else
    nodeptr = nhc_mkRight(nhc_mkUnit());
  
  C_RETURN(nodeptr);
}
#endif

/* foreign import hSetPosnC :: Handle -> HandlePosn -> IO Int */
int hSetPosnC (FileDesc *f, fpos_t *p)
{
  return fsetpos(f->fp,p);
}
