#include <errno.h>
#include <string.h>
#include <stdio.h>
#include "haskell2c.h"
#if TRACE
#include "../../tracer/runtime/getconstr.h"
#endif

#if 0

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
    a->path = strdup(filename);
    fo = allocForeignObj(a,gcFile,gcNow);
    nodeptr = mkRight(mkCInt((Int)fo));
  } else {
    nodeptr = mkLeft(mkIOErrorOpen(fileptr,typeptr,mkInt(errno)));
  }
  C_RETURN(nodeptr);
}

#endif

/* foreign import openFileC :: CString -> IOMode -> Either Int ForeignObj */
NodePtr openFileC (char* filename, int iom)
{
  char *type;
  FILE *fp;

  switch (iom) {
    case ReadMode:      type = "r";  break;
    case WriteMode:     type = "w";  break;
    case AppendMode:    type = "a";  break;
    case ReadWriteMode: type = "rw"; break;
  }
/*fprintf(stderr,"fopen: attempting to open file %s for %s\n",filename,type);*/

  fp = fopen(filename,type);
  if(fp) {
    FileDesc *a;
    ForeignObj *fo;
    /*fprintf(stderr,"fopen: succeeded\n");*/
    a = (FileDesc *)malloc(sizeof(FileDesc));
    a->fp = fp;
    a->bm = _IOFBF;
    a->size = -1;
    a->path = strdup(filename);
    fo = allocForeignObj(a,gcFile,gcNow);
    /*fprintf(stderr,"[openFileC: succeeded %x %x]\n",a,a->fp);*/
#if !TRACE
    return mkRight(mkCInt((int)fo));
#else
    return mkRight(mkR(mkCInt((int)fo),mkTNm(0,mkNmVector(),mkSR())));
#endif
  } else {
    /*fprintf(stderr,"fopen: failed to open file %s for %s\n",filename,type);*/
#if !TRACE
    return mkLeft(mkInt(errno));
#else
    return mkLeft(mkR(mkInt(errno),mkTNm(0,mkNmInt(mkInt(errno)),mkSR())));
#endif
  }
}
