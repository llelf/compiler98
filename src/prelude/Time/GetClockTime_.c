#include <haskell2c.h>
#include <time.h>

C_HEADER (gr_getClockTime)
{ NodePtr nodeptr;
  
  { time_t c;
    
    /* User code starts here */
    c = time(0);
    /* User code ends here */
    
    nodeptr = mkInt(c);
    C_RETURN(nodeptr);
  }
  ;
}
