#include "haskell2c.h"

#if 0
/* cHClose primitive 1 :: Handle -> () */
C_HEADER(cHClose)
{
  ForeignObj *f;
  FileDesc *a;
  NodePtr nodeptr;

  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  UPDATE_PROFINFO(nodeptr)
  f = (ForeignObj*)GET_INT_VALUE(nodeptr);
  a = derefForeignObj(f);

#if 0
#ifdef PROFILE
  if(!replay)
#endif
    fclose(a->fp);
  a->bm = -1;
  f->gc  = NULL;
  f->gcf = NULL;
#else
  freeForeignObj(f);
#endif

  C_RETURN(nhc_mkUnit());
}
#endif

/* foreign import hCloseC :: Addr -> IO () */
void hCloseC (FileDesc *f)
{
  if (f->fp)
    fclose(f->fp);
  f->fp = 0;
  f->bm = -1;
}
