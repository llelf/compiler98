
#include "haskell2c.h"

/* primComparePS :: Prelude.PackedString -> Prelude.PackedString -> Ordering */

C_HEADER(primComparePS)
{
  int length1,length2;
  char *srcptr1,*srcptr2;
  NodePtr nodeptr;
  Coninfo cinfo;
  int cmp;

  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  cinfo =  GET_CONINFO(nodeptr);
  length1 = CONINFO_LARGESIZES(cinfo) * sizeof(Node) - CONINFO_LARGEEXTRA(cinfo);
  srcptr1 = (char*)&nodeptr[1+EXTRA];

  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  cinfo =  GET_CONINFO(nodeptr);
  length2 = CONINFO_LARGESIZES(cinfo) * sizeof(Node) - CONINFO_LARGEEXTRA(cinfo);
  srcptr2 = (char*)&nodeptr[1+EXTRA];

  if(cmp = strncmp(srcptr1,srcptr2,(length1<length2?length1:length2))) {
    if(cmp<0)
      nodeptr = mkLT();
    else
      nodeptr = mkGT();
  } else {
    if(length1 < length2) 
      nodeptr = mkLT();
    else if (length1 == length2)
      nodeptr = mkEQ();
    else
      nodeptr = mkGT();
  }
  C_RETURN(nodeptr);
}	
