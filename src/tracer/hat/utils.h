#ifndef _UTILS_H
#define _UTILS_H

#include "hat.h"

extern FILE *HatFile, *OutputFile, *BridgeFile;
extern FileOffset errorRoot, errorMsg;
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

void		initialise	(int argc, char **argv);
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

FileOffset	readTraceAt	(FileOffset fo, char** expr, SrcRef** sr);

#endif
