#ifndef _UTILS_H
#define _UTILS_H

#include "hat.h"
#if defined(sparc)
#include <sys/byteorder.h>
#endif

extern FILE *HatFile, *OutputFile, *BridgeFile;
extern FileOffset errorRoot, errorMsg,remoteStartNode;
extern char* progname;
extern unsigned filesize, outputsize;
extern int ignoreErrors;

typedef struct {
  char* srcname;
  int   line;
  int   column;
} SrcRef;

typedef struct {
  char* idname;
  char* modname;
  char* srcname;
  char  priority;
  int   defnline;
  int   defncolumn;
} Ident;

char*		basename	(char* path, char* ext); /* ext can be NULL */
char*		dirname		(char* path);

void		initialise	(int argc, char **argv, int *browserport);
FILE*		openFile	(char* base, char* ext);
int		sizeFile	(char* base, char* ext);

FileOffset	readFO		(void);
int		freadAt		(FileOffset fo, void* ptr
				,int size, int nmemb, FILE* stream);
char*		readString	(void);
void		readModuleAt	(FileOffset fo, char** modname, char** srcname);
Ident*		readIdentifierAt(FileOffset fo);
Ident*		readNmTypeAt	(FileOffset fo);
SrcRef*		readSRAt	(FileOffset fo);

FileOffset	readTraceAt	(FileOffset fo, char** expr, SrcRef** sr
				,int* infix,int followHidden,int depth);
char*		infixPrint	( char* str1, int arg1
				, char* strfn, int fn
				, char* str2, int arg2);

#define isInfix(fix)  ((fix%4)!=3)
#define isInfixL(fix) ((fix%4)==2)
#define isInfixR(fix) ((fix%4)==1)
#define isInfixN(fix) ((fix%4)==0)
#define priority(fix) (fix/4)

#endif
