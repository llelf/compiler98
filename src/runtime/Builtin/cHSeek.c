#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>

#include "haskell2c.h"

/* cHSeek primitive 3 :: Handle -> SeekMode -> Int -> Either IOError () */

C_HEADER(cHSeek)
{
  FileDesc *a;
  NodePtr nodeptr;
  int err;
  int sm;
  long offset;

  C_CHECK(sizeRight+sizeUnit + sizeLeft+sizeIOErrorHSeek);
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  a = derefForeignObj((ForeignObj*)(GET_INT_VALUE(nodeptr)));

  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  switch(GET_CONSTR(nodeptr)) {
  case AbsoluteSeek: sm = SEEK_SET; break;
  case RelativeSeek: sm = SEEK_CUR; break;
  case SeekFromEnd:  sm = SEEK_END; break;
  }

  nodeptr = C_GETARG1(3);
  IND_REMOVE(nodeptr);
  offset = GET_INT_VALUE(nodeptr);
  
  
#ifdef PROFILE
  if(replay) {
    REPLAY_BOOL(err);
    if(err)
      REPLAY(errno);
  } else
#endif
  err = fseek(a->fp,offset,sm);
#ifdef PROFILE
  if(record) {
    RECORD_BOOL(err);
    if(err)
      RECORD(errno);
  }
#endif

  if(err)
    nodeptr = mkLeft(mkIOErrorHSeek(C_GETARG1(1),mkInt(errno)));
  else
    nodeptr = mkRight(mkUnit());
  
  C_RETURN(nodeptr);
}	
