#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "haskell2c.h"
#if TRACE
#include "../../hat/runtime/getconstr.h"
#endif

#if 0
/* cHSetBuffering primitive 2 :: Handle -> BufferMode -> Either IOError () */
C_HEADER(cHSetBuffering)
{
  FileDesc *a;
  NodePtr nodeptr;
  int bm,size = BUFSIZ;
  int err;
   
  C_CHECK(nhc_sizeRight+nhc_sizeUnit + nhc_sizeLeft+nhc_sizeIOErrorHIsEOF);
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  a = derefForeignObj((ForeignObj*)(GET_INT_VALUE(nodeptr)));

  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  switch(GET_CONSTR(nodeptr)) {
  case NoBuffering:    bm = _IONBF; break;
  case LineBuffering:  bm = _IOLBF; break;
  case BlockBuffering:
    { NodePtr maybe = GET_POINTER_ARG1(nodeptr,1);
      bm = _IOFBF;
      IND_REMOVE(maybe);
      switch(GET_CONSTR(maybe)) {
      case Just:
	maybe = GET_POINTER_ARG1(maybe,1);
	IND_REMOVE(maybe);
	size = GET_INT_VALUE(maybe);
      }
    }
 break;
  }

#ifdef PROFILE
  if(replay) {
    REPLAY_BOOL(err);
    if(err)
      REPLAY(errno);
  } else
#endif
  err = setvbuf(a->fp,0,bm,size);
#ifdef PROFILE
  if(record) {
    RECORD_BOOL(err);
    if(err)
      RECORD(errno);
  }
#endif
  
  if(err) {
    nodeptr = nhc_mkLeft(nhc_mkIOErrorHSetBuffering(C_GETARG1(1),nhc_mkInt(errno)));
  } else {
    a->bm = bm;
    a->size = size;
    nodeptr = nhc_mkRight(nhc_mkUnit());
  }
  
  C_RETURN(nodeptr);
}
#endif

/* foreign import hSetBufferingC :: Handle -> BufferMode -> IO Int */
int hSetBufferingC (FileDesc*f, NodePtr bufm)
{
  int bm,size = BUFSIZ;
  int err;

  switch(GET_CONSTR(bufm)) {
    case NoBuffering:    bm = _IONBF; break;
    case LineBuffering:  bm = _IOLBF; break;
    case BlockBuffering:
      { NodePtr maybe = GET_POINTER_ARG1(bufm,1);
        bm = _IOFBF;
        IND_REMOVE(maybe);
        switch(GET_CONSTR(maybe)) {
          case Just: maybe = GET_POINTER_ARG1(maybe,1);
                     IND_REMOVE(maybe);
                     size = GET_INT_VALUE(maybe);
        }
      } break;
  }
  err = setvbuf(f->fp,0,bm,size);
  if (err) {
    return err;
  } else {
    f->bm = bm;
    f->size = size;
    return 0;
  }
}
