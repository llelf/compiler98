#include <string.h>
#include "haskell2c.h"

#if 0

/* cGetArgs :: [PackedString] */
C_HEADER(cGetArgs)
{
  int totalsize = 0;
  int i;
  NodePtr res,*tmp;

#ifdef PROFILE
  if(replay) {
    REPLAY(i);
  } else
#endif
  for(i=1; i<Argc; i++) 
    totalsize += sizePackedString(strlen(Argv[i]));
  C_CHECK(Argc*sizeCons + totalsize);
#ifdef PROFILE
  if(record) {
    RECORD(i);
  }
#endif

  tmp = &res;

  for(i=1; i<Argc; i++) {
#ifdef PROFILE
    if(replay) {
      int length;
      NodePtr n;
      REPLAY(length);
      n = allocPackedString(length);
      REPLAY_STR(length,getPackedString(n));
      *tmp = mkCons(n,0);
      tmp = (NodePtr *)&((*tmp)[EXTRA+2]);
    } else
#endif
      {
	*tmp = mkCons(mkPackedString(strlen(Argv[i]),Argv[i]),0);
	tmp = (NodePtr *)&((*tmp)[EXTRA+2]);
      }	
#ifdef PROFILE
    if(record) {
      int length = strlen(Argv[i]);
      RECORD(length);
      RECORD_STR(length,Argv[i]);
    }
#endif
  }

  *tmp = mkNil();
  C_RETURN(res);
}	
#endif

char *
cGetArg (void)
{
  static int i=1;
  if (i<Argc) return Argv[i++];
  else return (char*)0;
}
