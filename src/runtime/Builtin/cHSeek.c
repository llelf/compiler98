#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include "haskell2c.h"
#if TRACE
#include "../../hat/runtime/getconstr.h"
#endif

#if 0
/* cHSeek primitive 3 :: Handle -> SeekMode -> Int -> Either IOError () */
C_HEADER(cHSeek)
{
  FileDesc *a;
  NodePtr nodeptr;
  int err;
  int sm;
  long offset;

  C_CHECK(nhc_sizeRight+nhc_sizeUnit + nhc_sizeLeft+nhc_sizeIOErrorHSeek);
  
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
    nodeptr = nhc_mkLeft(nhc_mkIOErrorHSeek(C_GETARG1(1),nhc_mkInt(errno)));
  else
    nodeptr = nhc_mkRight(nhc_mkUnit());
  
  C_RETURN(nodeptr);
}
#endif

/* foreign import hSeekC :: Handle -> Int -> Integer -> IO Int */
int hSeekC (FileDesc* f, int seekmode, NodePtr i)
{
  int sm;
  long offset;
  switch (seekmode) {
    case AbsoluteSeek: sm = SEEK_SET; break;
    case RelativeSeek: sm = SEEK_CUR; break;
    case SeekFromEnd:  sm = SEEK_END; break;
  }
  offset = GET_INT_VALUE(i);	/* naughty! */
  return fseek(f->fp,offset,sm);
}
