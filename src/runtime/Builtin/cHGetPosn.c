#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include "haskell2c.h"
#if TRACE
#include "../../hat/runtime/getconstr.h"
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
  
  C_CHECK(nhc_sizeRight+nhc_sizeHandlePosn + nhc_sizeLeft+nhc_sizeIOErrorHGetPosn);
  
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
    nodeptr = nhc_mkRight(nhc_mkHandlePosn(&fpos));
  else
    nodeptr = nhc_mkLeft(nhc_mkIOErrorHGetPosn(C_GETARG1(1),nhc_mkInt(errno)));
  
  C_RETURN(nodeptr);
}
#endif

/* foreign import hGetPosnC :: Handle -> IO Addr */
void* hGetPosnC(FileDesc* f)
{
  fpos_t *p;
  int err;
  p = (fpos_t*)malloc(sizeof(fpos_t));
  err = fgetpos(f->fp,p);
  if (err) {
    free(p);
    return (void*)0;
  } else {
    return p;
  }
}
