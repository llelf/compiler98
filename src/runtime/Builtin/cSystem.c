#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include "haskell2c.h"

/* cSystem :: CString -> Either Int ExitCode */

C_HEADER(cSystem)
{
  NodePtr nodeptr;
  char *src;
  int i;

#ifdef TPROF
  timerStop(&runTime);	/*PH*/
  stoptimer();		/*PH*/
#endif

  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
#ifdef PROFILE
  if(replay) {
    REPLAY(i);
    if(i == -1)
      REPLAY(errno);
  } else
#endif
    i = system(nhc_getPackedString(nodeptr));
#ifdef PROFILE
  if(record) {
    RECORD(i);
    if(i == -1)
      RECORD(errno);
  }
#endif

  if(i == -1) { /* Failed fork or exec */
    nodeptr = nhc_mkLeft(nhc_mkInt(errno));
  } else {
    if(i) {	
      nodeptr = nhc_mkRight(nhc_mkExitFailure(nhc_mkInt(i)));
    } else {
      nodeptr = nhc_mkRight(nhc_mkExitSuccess());
    }
  }

#ifdef TPROF
  setuptimer();		/*PH*/
  timerStart(&runTime);	/*PH*/
#endif

  C_RETURN(nodeptr);
}
