#include <haskell2c.h>
#include <sys/stat.h>
#include <unistd.h>
#define TRUE   1
#define FALSE  0

C_HEADER (gr_doesDirectoryExist)
{ NodePtr nodeptr;
  char* fp;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  fp = (char*)getPackedString(nodeptr);
  
  { int b;
    
    /* User code starts here */
    
    struct stat st;
    int err;
    err = stat(fp,&st);
    if ((err==0) && S_ISDIR(st.st_mode)) b=TRUE;
    else b=FALSE;
    /* User code ends here */
    
    nodeptr = mkBool(b);
    C_RETURN(nodeptr);
  }
  ;
}
