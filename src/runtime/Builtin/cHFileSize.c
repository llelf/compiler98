#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include "haskell2c.h"
#if TRACE
#include "../../tracer/runtime/getconstr.h"
#endif


/* hFileSize 1 :: Handle -> (Either IOError Integer) */

#if 0
C_HEADER(cHFileSize)
{
  FileDesc *a;
  NodePtr nodeptr;
  struct stat buf;
  int err;
  
  C_CHECK(sizeRight+sizeSmallIntegerU + sizeLeft+sizeInt);
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  a = derefForeignObj((ForeignObj*)(GET_INT_VALUE(nodeptr)));

#ifdef PROFILE
  if(replay) {
    REPLAY_BOOL(err);
    if(err)
      RECORD(errno);
    else
      RECORD(buf);
  } else
#endif
    err = fstat(fileno(a->fp),&buf);
#ifdef PROFILE
  if(record) {
    RECORD_BOOL(err);
    if(err)
      RECORD(errno);
    else
      RECORD(buf);
  }
#endif

  if(!err)
    nodeptr = mkRight(mkSmallIntegerU(buf.st_size));
  else
    nodeptr = mkLeft(mkInt(errno));
  
  C_RETURN(nodeptr);
}
#endif

NodePtr primHFileSizeC (FileDesc* f)
{
  struct stat buf;
  int err;
  err = fstat(fileno(f->fp),&buf);
#if !TRACE
  if (err)
    return mkLeft(mkInt(errno));
  else
    return mkRight(mkSmallIntegerU(buf.st_size));
#else
  if (err)
    return mkLeft(mkR(mkInt(errno),mkTNm(0,mkNmInt(mkInt(errno)),mkSR())));
  else
    return mkRight(mkR(mkSmallIntegerU(buf.st_size),mkTNm(0,mkNmInt(mkInt(buf.st_size)),mkSR())));
#endif
}
