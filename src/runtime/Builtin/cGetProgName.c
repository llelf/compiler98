#include <string.h>
#include "haskell2c.h"

#if 0

/* cGetProgName :: PackedString */
C_HEADER(cGetProgName)
{
  int totalsize = 0;
  int i;
  NodePtr res,*tmp;

#ifdef PROFILE
  if(replay) {
    int length;
    REPLAY(length);
    res = nhc_allocPackedString(length);
    REPLAY_STR(length,nhc_getPackedString(res));
  } else
#endif
    res = nhc_mkPackedString(strlen(Argv[0]),Argv[0]);
#ifdef PROFILE
  if(record) {
    int length = strlen(Argv[0]);
    RECORD(length);
    RECORD_STR(length,Argv[0]);
  }
#endif
  C_RETURN(res);
}	
#endif

char *
primGetProgName (void)
{
  return Argv[0];
}
