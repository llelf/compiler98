#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include "haskell2c.h"
#if TRACE
#include "../../tracer/runtime/getconstr.h"
#endif

#if 0
/* cHGetPosn primitive 1 :: Handle -> Either IOError HandlePosn */
C_HEADER(cHGetPosn)
{
  FileDesc *a;
  NodePtr nodeptr;
  int err;
  int posn;
  fpos_t fpos;
  
  C_CHECK(sizeRight+sizeHandlePosn + sizeLeft+sizeIOErrorHGetPosn);
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  a = derefForeignObj((ForeignObj *)(GET_INT_VALUE(nodeptr)));

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
#endif

/* foreign import hGetPosnC :: Handle -> Either Int Addr */
NodePtr hGetPosnC(FileDesc* f)
{
  fpos_t *p;
  int err;
  p = (fpos_t*)malloc(sizeof(fpos_t));
  err = fgetpos(f->fp,p);
#if !TRACE
  if (err)
    return mkLeft(mkInt(errno));
  else
    return mkRight(mkCInt((int)p));
#else
  if (err)
    return mkLeft(mkR(mkInt(errno),mkTNm(0,mkNmInt(mkInt(errno)),mkSR())));
  else
    return mkRight(mkR(mkCInt((int)p),mkTNm(0,mkNmVector(),mkSR())));
#endif
}
