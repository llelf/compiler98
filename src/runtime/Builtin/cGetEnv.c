#include <string.h>
#include <stdlib.h>
#include "haskell2c.h"
#include <errno.h>

/* cGetEnv :: CString -> Either Int PackedString */

C_HEADER(cGetEnv)
{
  NodePtr nodeptr;
  char *src;

  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);

#ifdef PROFILE
  if(replay) {
    int length;
    REPLAY(length);
    nodeptr = allocPackedString(length);
    REPLAY_STR(length,getPackedString(nodeptr));
  } else
#endif
    { src = getenv(getPackedString(nodeptr));
      if(!src) { /*src = "";*/
             nodeptr = mkLeft(mkInt(ENOENT));
      }else{ nodeptr = mkRight(mkPackedString(strlen(src),src)); }
    }
#ifdef PROFILE
  if(record) {
    int length = strlen(src);
    RECORD(length);
    RECORD_STR(length,src);
  }
#endif
  C_RETURN(nodeptr);
}	

char *
primGetEnv (char* sym)
{
  char* val;
  val = getenv(sym);
  if (!val) errno=ENOENT;
  return val;
}
