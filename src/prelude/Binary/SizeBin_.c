#include <haskell2c.h>
#include "cLowBinary.h"

C_HEADER (gr_sizeBin)
{ NodePtr nodeptr;
  BinHandle bh;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  bh = (BinHandle)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  { int n;
    
    /* User code starts here */
    
    unsigned curpos;
    closecache(bh);
    if (bh->file) {
    curpos = vtell(bh);		/* store current position */
    n = lseek(bh->loc.fd,0,SEEK_END);	/* jump to end of file */
    } else {
    curpos = mtell(bh);		/* store current position */
    n = bh->attrib.size;
    }
    opencache(bh);			/* then skip back again */
    bh->cptr = curpos - forceCacheTo(bh,curpos);
    /* User code ends here */
    
    nodeptr = mkInt(n);
    C_RETURN(nodeptr);
  }
  ;
}
