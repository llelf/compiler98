#ifdef BIG_ENDIAN
#undef BIG_ENDIAN
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <string.h>
/* #include <netinet/in.h> */
/* #include <netdb.h> */
#include "haskell2c.h"
#include "bytecode.h"

#define DBGPORT 6705

CData *sockcdata = NULL;

/* cGetDbgSocket :: () -> Socket */
C_HEADER(cGetDbgSocket)
{
	if (sockcdata == NULL) { /* Socket not yet opened */
		int fdesc;
		FILE *fp;

		fdesc = sockopen("localhost", DBGPORT, SOCK_STREAM);
		if (fdesc < 0)
			fp = 0;
		else
			fp = fdopen(fdesc, "rb+");
		if (fp) {
			Arg a;
			
			a.fp = fp;
			a.bm = _IOFBF;
			a.size = -1;
			a.gc = gcFile;

			sockcdata = allocCData(a);
			/* fprintf(stderr, "Debug socket successfully opened!\n");*/
		} else {
			fprintf(stderr, "Couldn't fdopen dbg socket!\n");
			exit(-1);
		}
	}
	C_RETURN(mkCInt((int)sockcdata));
}


