#ifdef BIG_ENDIAN
#undef BIG_ENDIAN
#endif

#define TRUE	1
#define FALSE	0

#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
/* #include <netinet/in.h> */
/* #include <netdb.h> */
#include "haskell2c.h"
#include "bytecode.h"

/* Whenever we see a node, it is given a reference number and placed in 
 * a table, the node cache, using ncInsert(NodePtr np, int hist).
 * If hist is true, it means that trace information for the node has been
 * (or is being) sent to the interface. Nodes are looked up in the cache 
 * using ncFind(NodePtr np). If it returns zero, the node is not in the 
 * cache. If it returns a positive number, it is the reference number of 
 * the node in the cache, and the node has its trace sent to the interface.
 * If the number is negative, the node is in the cache, but its trace has 
 * not been sent to the interface.
 */

#define NC_ALLOCSIZE 1024

NodePtr *nodeCache = NULL;

int nodeCacheSize;
int nodeCacheRefNr;

int ncFind(NodePtr np) 
{
    int p = 0;
    while (p < nodeCacheRefNr)
	if ((int)np == ((int)nodeCache[p] & -2))
	    if ((int)nodeCache[p++] & 1)
		return p;
	    else
		return -p;
	else
	    p++;
    return 0;
}

NodePtr ncRef(int ref)
{
    return (NodePtr)((int)nodeCache[ref-1] & -2);    
}

void ncHist(NodePtr np, int ref)
{
    nodeCache[ref-1] = (NodePtr)((int)np | 1);
}

int ncInsert(NodePtr np, int hist) 
{
    if (nodeCacheRefNr == nodeCacheSize) {
	nodeCacheSize += NC_ALLOCSIZE;
	nodeCache = (NodePtr*)realloc((void *)nodeCache, 
				      nodeCacheSize * sizeof(NodePtr*));
	if (!nodeCache) {
	    fprintf(stderr, "Couldn't reallocate node cache.\n");
	    exit(1);
	}
    }
    nodeCache[nodeCacheRefNr++] = (NodePtr)((int)np | (hist ? 1 : 0));
    return nodeCacheRefNr;
}

int ncInit()
{
    if (nodeCache != NULL)
	free(nodeCache);
    if (!(nodeCache = (NodePtr*)malloc(NC_ALLOCSIZE * sizeof(NodePtr*)))) {
	fprintf(stderr, "Couldn't allocate node cache.\n");
	exit(1);
    }
    nodeCacheSize = NC_ALLOCSIZE;
    nodeCacheRefNr = 0;
}

void ncDump()
{
    int p;
    if (nodeCache != NULL) {
	p = 0;
	while (p < nodeCacheRefNr) {
	    fprintf(stderr, "%-4d 0x%-8x\n", p, (int)nodeCache[p]);
	    p++;
	}
    }
}

#if 0
int ncFind2(NodePtr np)
{
    int p, lo = 0, hi = nodeCacheRefNr;

    while (lo < hi) {
	p = (lo+hi)/2;
	if (np > nodeCache[p]) {
	    lo = p;
	} else if (np < nodeCache[p]) {
	    hi = p;
	} else {
	    return p;
	}	    
    }
    return -p;
}
#endif
