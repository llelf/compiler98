#include <haskell2c.h>
#include "cLowBinary.h"

C_HEADER (gr_freezeBin)
{ NodePtr nodeptr;
  BinHandle bh;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  bh = (BinHandle)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  { 
    /* User code starts here */
    
    closecache(bh);
    if (bh->file) {
    if (bh->mode != RO) {
    char c; int i;
    c = (char)(bh->highwater%8);
    i = (c ? 1+(bh->highwater/8) : (bh->highwater/8));
    c = (c ? 16-c : 8);
    lseek(bh->loc.fd,i,SEEK_SET);
    write(bh->loc.fd,&c,1);
    }
    }
    bh->mode = RO;
    opencache(bh);
    /* User code ends here */
    
    nodeptr = mkUnit();
    C_RETURN(nodeptr);
  }
  ;
}
