#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include "haskell2c.h"
#if TRACE
#include "../../tracer/runtime/getconstr.h"
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
#endif

/* foreign import hSeekC :: Handle -> Int -> Integer -> Either Int () */
NodePtr hSeekC (FileDesc* f, int seekmode, NodePtr i)
{
  int err;
  int sm;
  long offset;
  switch (seekmode) {
    case AbsoluteSeek: sm = SEEK_SET; break;
    case RelativeSeek: sm = SEEK_CUR; break;
    case SeekFromEnd:  sm = SEEK_END; break;
  }
  offset = GET_INT_VALUE(i);	/* naughty! */
  err = fseek(f->fp,offset,sm);
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
