#ifdef BIG_ENDIAN
#undef BIG_ENDIAN
#endif

#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
/*#include "haskell2c.h"*/
#include "runtime.h"
#include "mark.h"

extern void mark(NodePtr*);
extern void flip(NodePtr*);

/* Whenever we get a stable ptr, it is given a reference number and placed in 
 * a table, the StableTable, using stableInsert(NodePtr np).  Nodes are
 * looked up in the table using stableRef(int ref). If it returns zero,
 * the node is not in the table.
 */

#define STABLE_ALLOCSIZE 1024

NodePtr *StableTable;

int StableFree;
int StableRefNr;

NodePtr stableRef(int ref)
{
    return StableTable[ref];
}

void stableMap(void (*f)(NodePtr*))
{
  int i;
  for(i=0; i < STABLE_ALLOCSIZE; i++) {
    if (StableTable[i]) {
      f(&StableTable[i]);
    }
  }    
}

int stableInsert(NodePtr np) 
{
    if (--StableFree) {
        while (StableTable[StableRefNr]) {
            if (++StableRefNr == STABLE_ALLOCSIZE)
                StableRefNr = 0;
        }
        StableTable[StableRefNr] = np;
        return StableRefNr;
    } else {
	fprintf(stderr, "Couldn't allocate stable pointer - limit exceeded.\n");
	exit(1);
    }
}

void stableRelease(int i)
{
    StableTable[i] = (NodePtr)0;
    StableFree++;
}

/* Called by the garbage collector */
void stableMark()
{
  stableMap(mark);
}

void stableFlip()
{
  stableMap(flip);
}

int stableInit() {
    int i;

    if (!(StableTable = (NodePtr*)malloc(STABLE_ALLOCSIZE*sizeof(NodePtr*)))) {
	fprintf(stderr, "Couldn't allocate stable pointer table.\n");
	exit(1);
    }
    for (i=0; i<STABLE_ALLOCSIZE; i++)
        StableTable[i] = (NodePtr)0;
    StableFree = STABLE_ALLOCSIZE;
    StableRefNr = 0;
    add_user_gc(stableMark,stableFlip);
}
