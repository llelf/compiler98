#include <errno.h>
#include "haskell2c.h"

#if 0
/* hIsEOF primitive 1 :: Handle -> (Either IOError Bool) */
C_HEADER(cHIsEOF)
{
  FileDesc *a;
  NodePtr nodeptr;
  int eof;
  
  C_CHECK(nhc_sizeRight+(nhc_sizeTrue + nhc_sizeFalse));
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  a = derefForeignObj((ForeignObj*)(GET_INT_VALUE(nodeptr)));
#ifdef PROFILE
  if(replay) {
    REPLAY_BOOL(eof);
  } else
#endif
    eof = feof(a->fp);
#ifdef PROFILE
  if(record) {
    RECORD_BOOL(eof);
  }
#endif

  nodeptr = nhc_mkRight(nhc_mkBool(eof));

  C_RETURN(nodeptr);
}
#endif

/* foreign import hIsEOF :: Handle -> IO Bool */
int hIsEOF (FileDesc* f)
{
  int err, c;
  c = fgetc(f->fp);	/* Horrible but true: feof does not truly test for */
  err = feof(f->fp);	/* end of file!  The EOF flag must be set by a     */
  ungetc(c,f->fp);	/* read operation, hence the getc -> ungetc pair.  */
  return err;
}
