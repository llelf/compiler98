#include <haskell2c.h>
#include "cLowBinary.h"

C_HEADER (gr_sameBH)
{ NodePtr nodeptr;
  BinHandle sbh;
  BinHandle dbh;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  sbh = (BinHandle)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  nodeptr = C_GETARG1(2);
  IND_REMOVE(nodeptr);
  dbh = (BinHandle)((cdataArg((CData*)GET_INT_VALUE(nodeptr)))->cval);
  
  { int b;
    
    /* User code starts here */
    
    b = (sbh==dbh) ||
    ((sbh->file == dbh->file) && (sbh->loc.fd==dbh->loc.fd));
    /* User code ends here */
    
    nodeptr = mkBool(b);
    C_RETURN(nodeptr);
  }
  ;
}
