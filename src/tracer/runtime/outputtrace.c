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
#include "runtime.h"
#include "mark.h"
#include "outputtrace.h"

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

#define OT_ALLOCSIZE 1024

otElement *outputTrace;

int outputTraceSize;
int outputTraceRefNr;

int 
otSize()
{
    return outputTraceRefNr;
}

otElement *
otRef(int ref)
{
    return &outputTrace[ref-1];
}

void
otMap(void (*f)(NodePtr*))
{
  int i;
  for(i = 0; i < outputTraceRefNr; i++) {
      f(&outputTrace[i].trace);
  }    
}

void 
otInsert(int ch, NodePtr trace) 
{
    if (outputTraceRefNr == outputTraceSize) {
	outputTraceSize += OT_ALLOCSIZE;
	outputTrace = (otElement*)realloc((void *)outputTrace, 
				      outputTraceSize * sizeof(otElement));
	if (!outputTrace) {
	    fprintf(stderr, "Couldn't reallocate output trace table.\n");
	    exit(1);
	}
    }
    outputTrace[outputTraceRefNr].ch = ch;
    outputTrace[outputTraceRefNr++].trace = trace;
}

int 
otInit()
{
    if (!(outputTrace = (otElement*)malloc(OT_ALLOCSIZE * sizeof(otElement)))) {
	fprintf(stderr, "Couldn't allocate node cache.\n");
	exit(1);
    }
    outputTraceSize = OT_ALLOCSIZE;
    outputTraceRefNr = 0;
}

extern NodePtr outputContext;

/* Called by the garbage collector */
void
otMark()
{
  int i;
  if (outputContext != NULL)
	  mark(&outputContext);
  for(i = 0; i < outputTraceRefNr; i++) {
      mark(&outputTrace[i].trace);
  }
}

void 
otFlip()
{
  int i;
  if (outputContext != NULL)
	  flip(&outputContext);
  for(i = 0; i < outputTraceRefNr; i++) {
      flip(&outputTrace[i].trace);
  }
}

