
#include <errno.h>

#include "haskell2c.h"

/* cDoesFileExist primitive 1 :: CString -> (Either IOError Bool) */

C_HEADER(cDoesFileExist)
{
  int length;
  NodePtr fileptr,typeptr,nodeptr;
  Coninfo cinfo;
  char *filename;

  C_CHECK(sizeLeft+sizeIOErrorOpen + sizeRight+sizeBool);

  fileptr = C_GETARG1(1);
  IND_REMOVE(fileptr);
  filename = (char*)&fileptr[1+EXTRA];

  if (access(filename,0)==0) {
    nodeptr = mkRight(mkTrue());
  } else {
    nodeptr = mkRight(mkFalse());
  }
  C_RETURN(nodeptr);
}

