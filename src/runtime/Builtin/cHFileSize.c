#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include "haskell2c.h"
#if TRACE
#include "../../hat/runtime/getconstr.h"
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

/* foreign import primHFileSizeC :: Handle -> IO Integer */
NodePtr primHFileSizeC (FileDesc* f)
{
  struct stat buf;
  int err;
  err = fstat(fileno(f->fp),&buf);
  if (err)
    return mkSmallIntegerU(-1);
  else
    return mkSmallIntegerU(buf.st_size);
}
