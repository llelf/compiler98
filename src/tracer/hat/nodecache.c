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
#include "art.h"

/* Whenever we see a node, it is given a reference number and placed in 
 * a table, the node cache, using ncInsert(FileOffset np, int hist).
 * If hist is true, it means that trace information for the node has been
 * (or is being) sent to the interface. Nodes are looked up in the cache 
 * using ncFind(FileOffset np). If it returns zero, the node is not in the 
 * cache. If it returns a positive number, it is the reference number of 
 * the node in the cache, and the node has its trace sent to the interface.
 * If the number is negative, the node is in the cache, but its trace has 
 * not been sent to the interface.
 */

#define NC_ALLOCSIZE 1024

typedef struct {
  FileOffset	fo;
  char		sent;
} CacheEntry;

CacheEntry *nodeCache = NULL;

int nodeCacheSize;
int nodeCacheRefNr;

int ncFind(FileOffset np) 
{
    int p = 0;
    while (p < nodeCacheRefNr)
	if (np == nodeCache[p].fo)
	    if (nodeCache[p++].sent)
		return p;
	    else
		return -p;
	else
	    p++;
    return 0;
}

FileOffset ncRef(int ref)
{
    if (!ref) return 0;
    return (nodeCache[abs(ref)-1].fo);    
}

void ncHist(int ref)
{
    nodeCache[ref-1].sent = TRUE;
}

int ncInsert(FileOffset np, int hist) 
{
    if (nodeCacheRefNr == nodeCacheSize) {
	nodeCacheSize += NC_ALLOCSIZE;
	nodeCache = (CacheEntry*)realloc((void *)nodeCache, 
				      nodeCacheSize * sizeof(CacheEntry));
	if (!nodeCache) {
	    fprintf(stderr, "Couldn't reallocate node cache.\n");
	    exit(1);
	}
    }
    nodeCache[nodeCacheRefNr].fo = np;
    nodeCache[nodeCacheRefNr].sent  = (char)(hist ? 1 : 0);
    return ++nodeCacheRefNr;
}

int ncInit()
{
    if (nodeCache != NULL) {
	fprintf(stderr,"Cannot re-initialise NodeCache\n");
	exit(1);
	/*free(nodeCache);*/	/* might we have a nodecache already? */
    }
    if (!(nodeCache = (CacheEntry*)malloc(NC_ALLOCSIZE * sizeof(CacheEntry)))) {
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
	    fprintf(stderr, "%-4d 0x%-8x %s\n", p, nodeCache[p].fo
                          , (nodeCache[p].sent?"sent":""));
	    p++;
	}
    }
}

