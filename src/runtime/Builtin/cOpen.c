#include <errno.h>
#include "haskell2c.h"

#ifdef CDBGTRANS
/* cOpen primitive 2 :: Trace -> R CString -> R IOMode -> R (Either IOError Handle) */
#else
/* cOpen primitive 2 :: CString -> IOMode -> (Either IOError Handle) */
#endif

C_HEADER(cOpen)
{
  int length;
  NodePtr fileptr,typeptr,nodeptr;
  Coninfo cinfo;
  char *filename;
  char *type;
  FILE *fp;

#ifdef CDBGTRANS
  C_CHECK(512); /* wrong */
#else
  C_CHECK(sizeLeft+sizeIOErrorOpen + sizeRight+sizeInt);
#endif

#ifdef CDBGTRANS
  np1 = C_GETARG1(2); /* R v t */
  IND_REMOVE(np1);
  fileptr = GET_POINTER_ARG1(np1, 1);
#else
  fileptr = C_GETARG1(1);
#endif
  IND_REMOVE(fileptr);
  filename = (char*)&fileptr[1+EXTRA];

#ifdef CDBGTRANS
  np1 = C_GETARG1(3); /* R v t */
  IND_REMOVE(np1);
  typeptr = GET_POINTER_ARG1(np1, 1);
#else
  typeptr = C_GETARG1(2);
#endif
  IND_REMOVE(typeptr);
  switch(GET_CONSTR(typeptr)) {
  case ReadMode:      type = "r";  break;
  case WriteMode:     type = "w";  break;
  case AppendMode:    type = "a";  break;
  case ReadWriteMode: type = "rw"; break;
  }

#ifdef PROFILE
  if(replay) {
    REPLAY(fp);
    if(!fp)
      REPLAY(errno);
  } else
#endif
    fp = fopen(filename,type);
#ifdef PROFILE
  if(record) {
    RECORD(fp);
    if(!fp)
      RECORD(errno);
  }
#endif
  if(fp) {
    FileDesc *a;
    ForeignObj *fo;
    a = (FileDesc *)malloc(sizeof(FileDesc));
    a->fp = fp;
    a->bm = _IOFBF;
    a->size = -1;
    fo = allocForeignObj(a,gcFile,gcNow);
    nodeptr = mkRight(mkCInt((Int)fo));
  } else {
    nodeptr = mkLeft(mkIOErrorOpen(fileptr,typeptr,mkInt(errno)));
  }
  C_RETURN(nodeptr);
}

