#ifndef _BROWSERCOMMS_H
#define _BROWSERCOMMS_H

#ifdef BIG_ENDIAN
#undef BIG_ENDIAN
#endif

#ifndef TRUE
#define TRUE    1
#define FALSE   0
#endif

#include <sys/types.h>
#include <sys/socket.h>
/*#include <sys/param.h>*/
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <time.h>
#include <signal.h>
#include <assert.h>
#include "hat.h"
#include "utils.h"
#include "nodecache.h"

#define DEFAULT_DEPTH 7
#define DBGPORT 6710
#define DBGMAXTRIES 32


int		isRoot			(FileOffset fo);
int		checkForCaseIfGuard	(FileOffset t);
int		storeRef		(FileOffset fo);
void		charListP		(FileOffset fo);
FileOffset	followTrace		(FileOffset fo
					,FileOffset *pbot, int *pind);
FileOffset	peekTrace		(FileOffset fo);
FILE*		waitForBrowserConnection(void);
void		ToBrowser		(FILE *sock, char *s);
void		dumpSRToBrowser		(FILE *sock, FileOffset t);
void		dumpRefToBrowser	(FILE *sock, FileOffset fo);
void		dumpOutputRefToBrowser	(FILE *sock, FileOffset fo);
void		dumpNodeToBrowser	(FILE *sock, int level, FileOffset fo);
void		dumpOutputToBrowser	(FILE *sock);
void		dumpTraceToBrowser	(FILE *sock, FileOffset fo);
void		sendFile		(FILE *sock, char *filename);
int		getline			(FILE *sock, char *s);
void		loop			(FILE *sock, FileOffset fo);


#endif
