#include <haskell2c.h>
#include "cLowBinary.h"

C_HEADER (gr_stdmem)
{ NodePtr nodeptr;
  
  { BinHandle bh;
    
    /* User code starts here */
    
    NodePtr n;
    bh = (BinHandle)malloc(sizeof(BinState));       /* assume unfailing */
    bh->mode = RW;
    bh->file = 0;
    n    = C_ALLOC(1+EXTRA);
    n[0] = CONSTRW(0,EXTRA);
    bh->loc.sp = stableInsert(n);
    bh->attrib.size = 0;
    bh->adjust.here = 0;
    opencache(bh);
    /* User code ends here */
    
    nodeptr = mkForeign((void*)bh,(gccval)finaliseBH);
    C_RETURN(nodeptr);
  }
  ;
}
