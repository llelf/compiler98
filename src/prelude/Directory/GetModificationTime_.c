#include <haskell2c.h>
#include <sys/stat.h>
#include <unistd.h>

C_HEADER (gr_getModificationTime)
{ NodePtr nodeptr;
  char* fp;
  
  nodeptr = C_GETARG1(1);
  IND_REMOVE(nodeptr);
  fp = (char*)getPackedString(nodeptr);
  
  { 
    /* User code starts here */
    
    struct stat st;
    stat(fp,&st);
    /* User code ends here */
    
    nodeptr = mkInt(st.st_mtime);
    C_RETURN(nodeptr);
  }
  ;
}
