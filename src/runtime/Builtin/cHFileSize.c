#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>

#include "haskell2c.h"

/* hFileSize 1 :: Handle -> (Either IOError Integer) */

C_HEADER(cHFileSize)
{
  Arg *a;
  NodePtr nodeptr;
  struct stat buf;
  int err;
  
  C_CHECK(sizeRight+sizeSmallIntegerU + sizeLeft+sizeInt);
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  a = cdataArg((CData *)(GET_INT_VALUE(nodeptr)));

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
