#ifndef _UI_H
#define _UI_H

#ifdef BIG_ENDIAN
#undef BIG_ENDIAN
#endif

#ifndef TRUE
#define TRUE    1
#define FALSE   0
#endif

#include <sys/types.h>
#include <sys/socket.h>
/*#include <sys/un.h>*/
#include <netinet/in.h>
/* #include <netdb.h> */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <time.h>
#include <signal.h>
#include <assert.h>
#include "haskell2c.h"
#include "bytecode.h"
#include "getconstr.h"
#include "nodecache.h"
#include "fileformat.h"
/*#include "outputtrace.h"*/

#define DEFAULT_DEPTH 7
#define DBGPORT 6710
#define DBGMAXTRIES 32

extern int terminated, interrupted, reductions;
extern int redTT, redTS, redST, redSS;
extern int trace_enter, trace_bpregexp, trace_singlestep;

extern NodePtr dbg_last_trace;
extern FileOffset outputContext;

NodePtr	shortCircuitSelectors	(NodePtr node);
void	showSymbol	(NodePtr t, char **pmodule, char **pname
			, int *pdefpos, int *ppri);
int	checkEvaluation	(NodePtr nodeptr, NodePtr *bot);
void	stackTrace	(NodePtr t);
/*int	startDbg	(NodePtr trace, int exitok);*/

#define MASK_K                  (0x00003f0)
#define CONINFO_DIST(p)         (((p)>>8)&0x3f)
#define INF_AGE                 63

#endif
