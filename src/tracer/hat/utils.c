#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include "hat.h"
#include "utils.h"

#define DEBUG 0

#if DEBUG
#define HIDE(x)	x
#else
#define HIDE(x)
#endif

FILE *HatFile, *OutputFile, *BridgeFile;
FileOffset errorRoot, errorMsg;
int ignoreErrors=False;
unsigned filesize=0, outputsize=0;
char* progname, *dir;

char*
rmext (char* word, char* ext)
{
  if (ext==(char*)0) return word;
  else {
    char *e = ext;
    char *i = strdup(word);
    char *c = i;
    while (*c) c++;
    while (*e) e++;
    while ((*--c==*--e) && e!=ext) ;
    if (e==ext) *c='\0'; else *++c='\0';
    return i;
  }
}

char*
basename (char* path, char* ext)
{
  char *c = path;
  while (*c) c++;
  while (*c!='/' && c!=path) c--;
  if (c==path) return rmext(path,ext);
  else {
    c++;
    return rmext(strdup(c),ext);
  }
}

char*
dirname (char* path)
{
  char *start, *c;
  start = strdup(path);
  c = start;
  while (*c) c++;
  while (*c!='/' && c!=start) c--;
  if (c==start) {
    return ".";
  } else {
    *c='\0';
    return start;
  }
}


/* The initialise() routine ensures that all files are available,
 * and opens them ready for further action.  It also fills in the
 * errorRoot and errorMsg globals.
 */
void
initialise (int argc, char **argv)
{
  int err;
  char header[8];
  char *arg;

  if ((argc!=2)&&(argc!=3)) {
    fprintf(stderr,"Usage: %s [-o] program\n",argv[0]);
    exit(1);
  }
  if ((argc==3)&&!strcmp(argv[1],"-o")) {
    ignoreErrors=True;
    arg=argv[2];
  } else {
    arg=argv[1];
  }
  dir = dirname(arg);
  arg = basename(arg,".hat");
  chdir(dir);
  progname   = basename(argv[0],0);
			/* for error messages - /not/ the prog being debugged */
  filesize   = sizeFile(arg,".hat");
  outputsize = sizeFile(arg,".hat.output");
  HatFile    = openFile(arg,".hat");
  OutputFile = openFile(arg,".hat.output");
  BridgeFile = openFile(arg,".hat.bridge");

  err = fread(header,sizeof(char),8,HatFile);
  if (err!=8) {
    fprintf(stderr,"%s: file %s/%s is too short\n",progname,dir,arg);
    exit(1);
  }
  if (strncmp(header,"Hat v01",7)) {
    fprintf(stderr,"%s: file %s in directory %s\n",progname,arg,dir);
    fprintf(stderr,"   does not appear to be a Hat archive in format v01\n");
    exit(1);
  }
  errorRoot = readFO();
  errorMsg  = readFO();
}

void
finalise (void)
{
  fclose(HatFile);
  fclose(OutputFile);
  fclose(BridgeFile);
}



/* Open a file for reading, given:
 *    the base name of the file
 *    the file extension
 */
FILE*
openFile (char* base, char* ext)
{
  char filename[256];
  FILE* file;
  struct stat buf;
  strcpy(filename,base);
  strcat(filename,ext);
  if (file = fopen(filename,"r")) {
    return file;
  } else {
    fprintf(stderr,"%s: cannot open %s/%s\n",progname,dir,filename);
    exit(1);
  }
}



/* Determine the size of a file, given:
 *    the base name of the file
 *    the file extension
 */
int
sizeFile (char* base, char* ext)
{
  char filename[256];
  struct stat buf;
  strcpy(filename,base);
  strcat(filename,ext);
  stat(filename,&buf);
  return buf.st_size;
}



/* readFO() reads a single FileOffset from the file and ensures it is
 * in host-endian order.
 */
FileOffset
readFO (void)
{
  FileOffset fo;
  fread(&fo,sizeof(FileOffset),1,HatFile);
  HIDE(fprintf(stderr,"readFO -> 0x%x\n",ntohl(fo));)
  return ntohl(fo);
}



/* freadAt() is just like fread(), except it seeks to a specific
 * file location first.
 */
int
freadAt (FileOffset fo, void* ptr, int size, int nmemb, FILE* stream)
{
  int err;
  if (fo > filesize) {
    fprintf(stderr,"%s: attempt to read beyond end of file\n",progname);
    fprintf(stderr,"%s: offset = 0x%x, filesize = 0x%x\n",progname,fo,filesize);
    fprintf(stderr,"%s: errno = %d (%s)\n",progname,errno,strerror(errno));
    exit(1);
  }
  if (fseek(stream, fo, SEEK_SET)) {
    fprintf(stderr,"%s: seek error on file\n",progname);
    fprintf(stderr,"%s: errno = %d (%s)\n",progname,errno,strerror(errno));
    exit(1);
  }
  return fread(ptr,size,nmemb,stream);
}



/* readString() reads a null-terminated string from the current position
 * in the file.
 */
char*
readString (void)
{
  char c, buf[256];
  int i;

  i=0; while (c=fgetc(HatFile)) {
    buf[i++] = c;
  }
  buf[i] = '\0';
  HIDE(fprintf(stderr,"readString -> %s\n",buf);)
  return strdup(buf);
}



/* readModuleAt() fills in the name of the module and its source file,
 * given the location of the module descriptor in the file.
 */
void
readModuleAt (FileOffset fo, char** modname, char** srcname)
{
  char c;

  freadAt(fo,&c,sizeof(char),1,HatFile);
  if ((c!=0x20) && (c!=0x21)) {
    fprintf(stderr,"%s: expected a Module descriptor at position 0x%x\n"
                  ,progname,fo);
    exit(1);
  }
  *modname = readString();
  *srcname = readString();
  HIDE(fprintf(stderr,"readModuleAt 0x%x -> %s %s\n",fo,*modname,*srcname);)
}



/* readIdentifierAt() fills in the name of the variable or constructor,
 * as well as its module, source file, priority, and definition position,
 * given the location of the NTId/NTConstr descriptor in the file.
 */
Ident*
readIdentifierAt (FileOffset fo)
{
  char c;
  FileOffset modpos;
  Ident* id;
  int defnpos;

  freadAt(fo,&c,sizeof(char),1,HatFile);
  if ((c!=0x46) && (c!=0x47)) {
    fprintf(stderr,"%s: expected an Identifier descriptor at position 0x%x\n"
                  ,progname,fo);
    exit(1);
  }
  id = (Ident*)malloc(sizeof(Ident));
  id->idname = readString();
  modpos = readFO();
  fread(&(id->priority),sizeof(char),1,HatFile);
  fread(&defnpos,sizeof(int),1,HatFile);
  id->defnline   = ntohl(defnpos)/10000;
  id->defncolumn = ntohl(defnpos)%10000;
  readModuleAt(modpos,&(id->modname),&(id->srcname));
  HIDE(fprintf(stderr,"readIdentifierAt 0x%x -> %s %s %s %d %d %d\n",fo,id->idname,id->modname,id->srcname,id->defnline,id->defncolumn,id->priority);)
  return id;
}



/* readSRAt() fills in a struct containing the filename and usage
 * position of a source reference, given the location of the
 * SR descriptor in the file.
 */
SrcRef *
readSRAt (FileOffset fo)
{
  FileOffset modpos;
  char *modname, *srcname;
  int usepos;
  char c;
  SrcRef *sr;

  HIDE(fprintf(stderr,"readSRAt 0x%x\n",fo);)
  if (fo) {
    freadAt(fo,&c,sizeof(char),1,HatFile);
    if (c!=0x60) {
      fprintf(stderr,"%s: expected a SrcRef descriptor at position 0x%x\n"
                    ,progname,fo);
      fprintf(stderr,"%s: got a 0x%x\n",progname,c);
      exit(1);
    }
    sr = (SrcRef*)malloc(sizeof(SrcRef));
    modpos = readFO();
    fread(&usepos,sizeof(int),1,HatFile);
    usepos = ntohl(usepos);
    readModuleAt(modpos, &modname, &(sr->srcname));
    sr->line    = usepos/10000;
    sr->column  = usepos%10000;
    return sr;
  } else {
    return (SrcRef*)0;
  }
}


/* readNmTypeAt() returns a struct containing a readable notation of the
 * NmType stored at the given location in the file.
 */
Ident*
readNmTypeAt (FileOffset fo)
{
  char c, buf[256];
  Ident *id = (Ident*)malloc(sizeof(Ident));

  /* defaults */
  id->idname   = (char*)0;
  id->modname  = strdup("Prelude");
  id->srcname  = strdup("Prelude.hs");
  id->priority = (char)3;
  id->defnline = 0;
  id->defncolumn = 0;

  freadAt(fo,&c,sizeof(char),1,HatFile);
  if ((c<0x40) || (c>0x50)) {
    fprintf(stderr,"%s: expected a NmType descriptor at position 0x%x\n"
                  ,progname,fo);
    exit(1);
  }
  HIDE(fprintf(stderr,"readNmTypeAt 0x%x -> tag 0x%x\n",fo,c);)
  switch (c&0x1f) {
    case NTInt:
		{ int i;
		  fread(&i,sizeof(int),1,HatFile);
		  sprintf(buf,"%d",ntohl(i));
		  id->idname = strdup(buf);
		} break;
    case NTChar:
		{ fread(&c,sizeof(char),1,HatFile);
		  if ((c>31) && (c!='\''))
		    sprintf(buf,"'%c'",c);
		  else switch(c) {
		    case '\n': sprintf(buf,"'\\n'"); break;
		    case '\t': sprintf(buf,"'\\t'"); break;
		    case '\255' : sprintf(buf,"'\\e'"); break;
		    default  : sprintf(buf,"'\\0%X'",c); break;
		  }
		  id->idname = strdup(buf);
		} break;
    case NTInteger:
		{ char size; int n;
		  fread(&size,sizeof(char),1,HatFile);
		  if (size==0) sprintf(buf,"0");
		  else if (size==1) {
		    int n;
		    fread(&n,sizeof(int),1,HatFile);
		    sprintf(buf,"%d",ntohl(n));
		  } else sprintf(buf,"<integer>");
		  id->idname = strdup(buf);
		} break;
    case NTRational:
		{ sprintf(buf,"<rational>");
		  id->idname = strdup(buf);
		} break;
    case NTFloat:
		{ float f;
		  fread(&f,sizeof(float),1,HatFile);
		  sprintf(buf,"%.6f",f);
		  id->idname = strdup(buf);
		} break;
    case NTDouble:
		{ double d;
		  fread(&d,sizeof(double),1,HatFile);
		  sprintf(buf,"%.15f",d);
		  id->idname = strdup(buf);
		} break;
    case NTId:
    case NTConstr:
    case NTTrusted:
		{ free(id->modname);
		  free(id->srcname);
		  free(id);
		  id = readIdentifierAt(fo);
		} break;
    case NTTuple:
		{ sprintf(buf,",");
		  id->idname = strdup(buf);
		} break;
    case NTFun:
		{ sprintf(buf,"<fun>");
		  id->idname = strdup(buf);
		} break;
    case NTCase:
		{ sprintf(buf,"case");
		  id->idname = strdup(buf);
		} break;
    case NTLambda:
		{ sprintf(buf,"\\");
		  id->idname = strdup(buf);
		} break;
    case NTDummy:
		{ sprintf(buf,"<dummy>");
		  id->idname = strdup(buf);
		  sprintf(buf,"Unknown");
		  id->modname = strdup(buf);
		} break;
    case NTCString:
		{ id->idname = readString();
		  sprintf(buf,"\"%s\"",id->idname);
		  free(id->idname);
		  id->idname = strdup(buf);
		} break;
    case NTIf:
		{ sprintf(buf,"if");
		  id->idname = strdup(buf);
		} break;
    case NTGuard:
		{ sprintf(buf,"guard");
		  id->idname = strdup(buf);
		} break;
    case NTContainer:
		{ sprintf(buf,"?");
		  id->idname = strdup(buf);
		  sprintf(buf,"Unknown");
		  id->modname = strdup(buf);
		} break;
    default: break;
  }
  HIDE(fprintf(stderr,"readNmTypeAt 0x%x -> %s %s %s %d %d %d\n",fo,id->idname,id->modname,id->srcname,id->defnline,id->defncolumn,id->priority);)
  if (!id->idname) id->idname = strdup("Problem");
  return id;
}


/* readTraceAt() fills in a string containing a readable notation of the
 * Trace stored at the given location in the file.  It returns the
 * parent trace.  This routine is only currently used by the "virtual
 * stack trace" program.
 */
FileOffset
readTraceAt (FileOffset fo, char** expr, SrcRef** sr, int* infix)
{
  char c, buf[256];
  FileOffset parent;

  *infix = 3;	/* default */

  if (fo) {
    freadAt(fo,&c,sizeof(char),1,HatFile);
    if ((c<0x00) || (c>0x07)) {
      fprintf(stderr,"%s: expected a Trace descriptor at position 0x%x\n"
                    ,progname,fo);
      exit(1);
    }
    HIDE(fprintf(stderr,"readTraceAt 0x%x -> tag 0x%x\n",fo,c);)
    switch (c&0x1f) {
      case TAp:
		{ int i, dummy;
		  FileOffset foExprs[20], foSR;
		  char* exprs[20];
		  int  fixexp[20];
		  fread(&c,sizeof(char),1,HatFile);
		  parent = readFO();
                  HIDE(fprintf(stderr,"enter parent of 0x%x -> 0x%x\n",fo,parent);)
		  for (i=0; i<=c; i++) {
		    foExprs[i] = readFO();
                  }
		  foSR = readFO();
		  for (i=0; i<=c; i++) {
		    (void)readTraceAt(foExprs[i],&(exprs[i]),sr,&(fixexp[i]));
                  }
		  *infix = fixexp[0];
		  if (isInfix(fixexp[0])) {
		    sprintf(buf,"%s"
			,infixPrint(exprs[1],fixexp[1],exprs[0],fixexp[0]
					,exprs[2],fixexp[2]));
		    for (i=3; i<=c; i++) {
		      strcat(buf," ");
		      strcat(buf,exprs[i]);
                    }
		  } else {	/* no fixity */
		    sprintf(buf,"(%s",exprs[0]);
		    for (i=1; i<=c; i++) {
		      strcat(buf," ");
		      if (isInfix(fixexp[i])) {
		        strcat(buf,"(");
		        strcat(buf,exprs[i]);
		        strcat(buf,")");
		      } else
		        strcat(buf,exprs[i]);
                    }
		    strcat(buf,")");
		  }
		  *expr = strdup(buf);
                  *sr   = readSRAt(foSR);
                  HIDE(fprintf(stderr,"return parent of 0x%x -> 0x%x\n",fo,parent);)
		  return parent;
		} break;
      case TNm:
		{ FileOffset foExpr, foSR;
		  Ident *id;
		  parent = readFO();
                  foExpr = readFO();
                  foSR   = readFO();
		  id     = readNmTypeAt(foExpr);
		  *infix = id->priority;
		  sprintf(buf,"%s",id->idname);
		  *expr = strdup(buf);
                  *sr   = readSRAt(foSR);
		  return parent;
		} break;
      case TInd:
		{ parent = readFO();	/* throw first away */
		  parent = readFO();
		  return readTraceAt(parent, expr, sr, infix);
		} break;
      case THidden:
		{ parent = readFO();
		  sprintf(buf,"·");
		  *expr = strdup(buf);
		  return parent;
		} break;
      case TSatA:
		{ parent = readFO();
		  return readTraceAt(parent, expr, sr, infix);
		} break;
      case TSatB:
		{ parent = readFO();
		  sprintf(buf,"_L");
		  *expr = strdup(buf);
		  return parent;
		} break;
      case TSatC:
		{ parent = readFO();
		  return readTraceAt(parent, expr, sr, infix);
		} break;
      default: break;
    }
  } else {
    sprintf(buf,"<Root>");
    *expr = strdup(buf);
    return fo;
  }
}



/* NOT USED */
/* checkSATkind() returns 0==error, 1==A, 2==B, 3==C, for the kind of
 * SAT pointed to by fo.  The trace must already be a SAT.
 */
int
checkSATkind (FileOffset fo)
{
  char c;
  freadAt(fo,&c,sizeof(char),1,HatFile);
  switch (c) {
    case 0x04: return 1; break;
    case 0x05: return 2; break;
    case 0x06: return 3; break;
    default:
        fprintf(stderr,"%s: expected a SAT at position 0x%x\n"
                      ,progname,fo);
        return 0; break;
  }
}


/* print an infix expression correctly according to the given priorities. */
char*
infixPrint (char* str1, int arg1, char* strfn, int fn, char* str2, int arg2)
{
  char buf[256];

  if (!isInfix(arg1))
      sprintf(buf,"%s",str1);
  else if (priority(arg1) > priority(fn))
      sprintf(buf,"%s",str1);
  else if (priority(arg1) < priority(fn))
      sprintf(buf,"(%s)",str1);
  else if (isInfixN(fn))
      sprintf(buf,"(%s)",str1);
  else
      sprintf(buf,"%s",str1);

  strcat(buf,strfn);

  if (!isInfix(arg2)) {
      strcat(buf,str2);
  } else if (priority(arg2) > priority(fn)) {
      strcat(buf,str2);
  } else if (priority(arg2) < priority(fn)) {
      strcat(buf,"(");
      strcat(buf,str2);
      strcat(buf,")");
  } else if (isInfixN(fn)) {
      strcat(buf,"(");
      strcat(buf,str2);
      strcat(buf,")");
  } else {
      strcat(buf,str2);
  }

  return strdup(buf);
}
