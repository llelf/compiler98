/**************************************************************************/
/* hatinterface.c: general operations on hat files                        */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include "hatinterface.h"
#include "hatgeneral.h"

/* routines providing interface to the hat archive file. */
#define MAXBUFSIZE 16384

int f;                  // file descriptor for archive

int bufsize=MAXBUFSIZE; // buffersize 
char* buf;              // input buffer
unsigned int buf_n;     // buf[0..buf_n-1] filled
unsigned int boff;      // if buf_n>0, boff in 0..buf_n-1 and buf[boff] is current
unsigned long foff;     // if buf_n>0, this is offset in f of buf[0]


typedef struct {
  int f;                  /* file descriptor for archive */
  int bufsize;
  char *buf;              /* input buffer */
  unsigned int buf_n;     /* buf[0..buf_n-1] filled */
  unsigned int boff;      /* if buf_n>0, boff in 0..buf_n-1 and buf[boff] is current */
  unsigned long foff;     /* if buf_n>0, this is offset in f of buf[0] */
  unsigned long filesize;
  struct stat statBuf;
} filehandler;

int hatHandleCount = 0,currentHandle=-1;
filehandler* hatHandle;

typedef union {char byte[4];
               unsigned long ptrval;
	       long intval;
	       float floatval;} fourbytes;

void _saveHandle(HatFile h) {
  if (h<0) return;
  hatHandle[h].f=f;
  hatHandle[h].bufsize=bufsize;
  hatHandle[h].buf = buf;
  hatHandle[h].buf_n = buf_n;
  hatHandle[h].boff = boff;
  hatHandle[h].foff = foff;
}

void _loadHandle(HatFile h) {
  if (h<0) return;
  f= hatHandle[h].f;
  bufsize = hatHandle[h].bufsize;
  buf = hatHandle[h].buf;
  buf_n = hatHandle[h].buf_n;
  boff = hatHandle[h].boff;
  foff = hatHandle[h].foff;
}

void hatSwitchToHandle(HatFile h) {
  if (h==currentHandle) return;
  _saveHandle(currentHandle);
  currentHandle = h;
  _loadHandle(currentHandle);
}

HatFile hatCurrentHandle() {return currentHandle;}

HatFile hatNewHandle() {
  filehandler* newHandles = (filehandler*) calloc(++hatHandleCount,sizeof(filehandler));
  if (hatHandleCount>1)
    memcpy(newHandles,hatHandle,sizeof(filehandler)*(hatHandleCount-1));
  free(hatHandle);
  hatHandle = newHandles;
  hatHandle[hatHandleCount-1].bufsize = MAXBUFSIZE;
  hatHandle[hatHandleCount-1].buf = (char*) calloc(MAXBUFSIZE,1);
  return hatHandleCount-1;
}

int checkParameters(char* str,char* allowed) {
  if (*str!='-') return 1; // bad parameter syntax
  str++;
  if (*str=='\0') return 1; // nothing specified!
  while (*str!='\0') {
    if (strchr(allowed,*str)==NULL)
      return 2; // unsupported parameter
    str++;
  }
  return 0; // string is ok
}

/* make proper file extension, if missing */
char* hatFilename(char* name) {
  if (strstr(name,".hat")!=NULL) return newStr(name);
  else {
    char* newstr=(char*) malloc(strlen(name)+5,sizeof(char));
    strcpy(newstr,name);
    strcat(newstr,".hat");
    return newstr;
  }
}

/* open file for reading, save in internal file descriptor */
HatFile hatOpenFile(char* name) {
  int f;
  name = hatFilename(name);
  f = open(name, 0);
  if (f!=-1) {
    int handle = hatNewHandle();
    stat(name, &(hatHandle[handle].statBuf));
    hatHandle[handle].filesize = hatHandle[handle].statBuf.st_size;
    freeStr(name);
    hatHandle[handle].buf_n = 0; // set buffer pointers appropriately. buf_n=0 buffer currently empty
    hatHandle[handle].boff = 0;  // at position 0 in buffer
    hatHandle[handle].foff = 0;  // at position 0 in file
    hatHandle[handle].f = f;
    currentHandle = handle;
    _loadHandle(handle);
    return handle;
  } else
    return -1;
}

/* close file in internal file descriptor */
void hatCloseFile(HatFile h) {
  //printf("Closing file %u\n",handle);
  close(hatHandle[h].f);
  free(hatHandle[h].buf);
  if (h==currentHandle) {
    currentHandle = -1;
  }
}

unsigned long hatFileSize() {return hatHandle[currentHandle].filesize;}

/* set new file position */
void hatSeekNode(unsigned long ofs) {
  if ((ofs>=foff)&&(ofs<foff+buf_n)) { // new position is within the buffer!
    boff=ofs-foff;   // only reset the buffer position!
  } else {
    boff=0;   // set internal pointers correctly
    foff=ofs;
    buf_n=0;      // buf_n=0, nothing in memory. read from file on next read!
    lseek(f,ofs,0); // do seek in file
  }
}

/* check for EOF */
int hatSeqEOF() {
  if (boff<buf_n) {
    return 0; /* more data in buffer! */
  } else {
    foff+=buf_n; // read next block
    boff-=buf_n;
    buf_n=read(f,buf,bufsize);
    return (buf_n<=boff);  // more data?
  }
}

/* return position in file */
unsigned long hatNodeNumber() {
  return foff + boff;
}

/* Routines to extract values encoded as one or more bytes. */

/* read one byte from file */
char nextbyte() {
  char c;
  if (boff>=buf_n) {  // need to read a new block
    foff += buf_n;
    boff -= buf_n;    // reduce boff appropriately
    buf_n = read(f,buf,bufsize);
    //if (buf_n==0) { fprintf(stderr, "unexpected end of trace file\n"); exit(1); }
    
  }
  c =  buf[boff++];
  return c;
}

char seenextbyte() {
  char c = nextbyte();
  boff--;    // backup by one byte
  return c;
}

/* just skip one byte in file - don't care to read it */
void skipbyte() { boff++; }

#define STRINGMAX 255
char stringbuf[STRINGMAX+1];

char *readstring() {
  int n = 0;
  char c;
  do {
    c = nextbyte();
    stringbuf[n++] = c;
  } while (c!='\0' && n<STRINGMAX); // read until buffer full or string's end
  stringbuf[n] = '\0';
  while (c!='\0') {c = nextbyte();} // oops, string is even longer. find its end
  return stringbuf;
}

/* skip one string in file */
void skipstring() {
  do {
    if (boff>=buf_n) {
      foff +=buf_n;
      boff -=buf_n;
      buf_n = read(f,buf,bufsize);
    }
  } while (buf[boff++]!='\0');
}

fourbytes readfourbytes() {
  unsigned int b=boff+4;
  fourbytes slot1,slot;
  if (b<=buf_n) { // next 4 bytes are in memory
    memcpy((char*) &slot1.ptrval,(char*) &buf[boff],4);
    boff=b;
    slot1.ptrval = ntohl(slot1.ptrval); // return next 4 bytes, increase boff
    return slot1;
  } else { // next 4 bytes not in memory, read bytewise
    slot.byte[0] = nextbyte();
    slot.byte[1] = nextbyte();
    slot.byte[2] = nextbyte();
    slot.byte[3] = nextbyte();
    slot.ptrval = ntohl(slot.ptrval);
    return slot;
  }
}

/* skip number of bytes in buffer
 */
void skipbytes(int bytes) { boff+=bytes; }

#define POSNMAX 30
char posnbuf[POSNMAX+1];

unsigned long readpointer() {
  return readfourbytes().ptrval;
}

void skippointer() {skipbytes(4);}

int readint() {
  return readfourbytes().intval;
}

#define INTEGERMAX 30
char integerbuf[INTEGERMAX+1];

int readinteger() { // int for now!
  int n = (int)(nextbyte());
  int i = n;
  if (n==0) return 0;
  if (n==1) return readint();
  while (i-- > 0) (void)(readfourbytes());
  return 0; // dummy value for INTEGER!  
}

void skipinteger() {
  int n = (signed char) (nextbyte());
  if (n<0) n=-n;
  skipbytes(n*4);
}

void skiprational() {
  skipinteger();
  skipinteger();
}

typedef union {
  double d;
  fourbytes a[2];
} eightbytes;

/* double readdouble() {
  eightbytes v;
  v.a[0] = readfourbytes();
  v.a[1] = readfourbytes();
  v.a[1].ptrval = htonl(v.a[1].ptrval);
  v.a[0].ptrval = htonl(v.a[0].ptrval);
  return v.d;
  }*/

int hi3(char b) {
  return (int)(b>>5);
}

int lo5(char b) {
  return (int)(b&037);
}

int readarity() {
  return (int)(nextbyte());
}

int readfixpri() {
  return (int)(nextbyte());
}

int tagat(unsigned long offset) {
  char byte[1];
  int i;
  if (offset <= hatHandle[currentHandle].filesize) {
    lseek(f, offset, 0);
    i = read(f, byte, 1);
    lseek(f, foff + buf_n, 0);
    return (i==1 ? hi3(byte[0]) : INVALID);
  } else return BEYOND;
}

/* reading, checking and/or writing header and node information */
int hatTestHeader() {
  char *version;
  version = readstring();
  if (strcmp(version,"Hat v01")!=0) {
    fprintf(stderr,"ERROR: File is not a hat file or version is not supported.\nAborted.\n\n");
    return 0;
  } else {
    skipbytes(8);
    return 1;
  }
}

void hatSeqFirst() {
  hatSeekNode(0);
  skipstring();
  skipbytes(8);
}

int isSAT(unsigned long fileoffset) {
  char c;
  unsigned long old = hatNodeNumber();
  
  hatSeekNode(fileoffset);
  c = seenextbyte();
  hatSeekNode(old);

  return ((c==TRSATC)||(c==TRSATB)||(c==TRSATA));
}

void skipNode(char nodeType) {
  switch (hi3(nodeType)) {
  case TR:
    switch (lo5(nodeType)) {
    case APP: // Application
      {
	int arity = readarity();
	skipbytes(4*(1+1+arity+1));
      }
      break;
    case NAM: // Name
	skipbytes(4*3);
	break;
    case IND: //  Indirection
	skipbytes(4*2);
	break;
    case HIDDEN: // Hidden
    case SATA:
    case SATB:
    case SATC:
    case SATCIS:
    case SATBIS:
    case SATAIS:
	skippointer();
	break;
    default:
	fprintf(stderr, "strange low-bits tag %d in TR 0x%x\n",
		lo5(nodeType), hatNodeNumber()-1);
	exit(1);
    }
    break;
  case MD: // Module / trusted or untrusted
      skipstring();
      skipstring();
      break;
  case NT:
    switch (lo5(nodeType)) {
    case INT:
	skipbytes(4);
	break;
    case CHAR:
	skipbytes(1);
	break;
    case INTEGER:
	skipinteger();
	break;       
    case RATIONAL:
	skiprational();
	break;
    case FLOAT:
	skipbytes(4);
	break;
    case DOUBLE:
	skipbytes(8);
	break;
    case IDENTIFIER:
	skipstring();  // simply read string
	skipbytes(4+1+4);
	break; 
    case CONSTRUCTOR:
	skipstring();
	skipbytes(4+1+4);
	break; 
    case TUPLE:
    case FUN:
    case CASE:
    case LAMBDA:
    case DUMMY:
    case IF:
    case GUARD:
    case CONTAINER:
	break;
    case CSTRING:
	skipstring();
	break;
    default:
	fprintf(stderr, "strange low-bits tag %d in NT 0x%x\n",
		lo5(nodeType), hatNodeNumber()-1);
	exit(1);
    }
    break;
  case SR:
      skipbytes(4+4);
      break;
  default:
      fprintf(stderr, "strange high-bits tag %d at byte offset 0x%x\n",
	      hi3(nodeType), hatNodeNumber()-1);
      exit(1);
  }
}

/* follow the trace along SATs, indirections and TRNAMEs */
unsigned long followTrace(unsigned long fileoffset) {
  char nodeType;
  unsigned long p;
  
  while (1) {
    //printf("following Trace... 0x%x\n",fileoffset);
    hatSeekNode(fileoffset);
    nodeType = nextbyte();
    //printf("node type: %i\n",b);
    switch (nodeType) {
    case TRNAM:
    case TRIND: //  Indirection
      skippointer();
      fileoffset=readpointer();
      break;
    case TRSATCIS:
    case TRSATC:
      fileoffset=readpointer(); // follow link...
      break;
    default:
      hatSeekNode(fileoffset); // backup one byte
      return fileoffset;
    }
  }
}

/* follow the trace along HIDDEN traces */
unsigned long followHidden(unsigned long fileoffset) {
  char nodeType;
  unsigned long p;
  
  while (1) {
    hatSeekNode(fileoffset);
    nodeType = nextbyte();
    if (nodeType==TRHIDDEN) {
      fileoffset=readpointer(); // follow link...
    } else {
      hatSeekNode(fileoffset); // backup one byte
      return fileoffset;
    }
  }
}

/* follow the trace along SATs */
unsigned long followSATs(unsigned long fileoffset) {
  char nodeType;
  unsigned long p;
  
  while (1) {
    hatSeekNode(fileoffset);
    nodeType = nextbyte();
    switch (nodeType) {
    case TRSATCIS:
    case TRSATC:
      fileoffset=readpointer(); // follow link...
      break;
    default:
      hatSeekNode(fileoffset); // backup one byte
      return fileoffset;
    }
  }
}

/* find the SAT belonging to an application */
/* return value 0: no SAT found, otherwise: offset for SAT */
unsigned long getResult(unsigned long fileoffset) {
  char nodeType;
  unsigned long p,satc;

  while (1) {
    //printf("searching for App SAT... 0x%x\n",fileoffset);
    hatSeekNode(fileoffset);
    nodeType = getNodeType();
    //printf("node type: %i\n",nodeType);
    switch (nodeType) {
    case TRAPP: // Application
      p = getParent();
      hatSeqNext();
      satc = hatNodeNumber();
      if (isSAT(satc)) { // success! found the SATC!
 	return satc;
      }
      else fileoffset = p; // follow parent!
      break;
    case TRNAM:   // for finding CAFs. SATc should be behind the TRNAM
      p=getParent();
      //if (p==0) { // found CAF!
      hatSeqNext();
      satc = hatNodeNumber();
      if (isSAT(satc)) return satc;
      //}
      fileoffset=p;
      break;
    default: {
	unsigned long newfileoffset=followTrace(fileoffset);
	if (newfileoffset == fileoffset) {
	  if (isSAT(newfileoffset)) return fileoffset;
	  else return 0;
	} else
	  fileoffset = newfileoffset;
      }
    }
  }
}

/* show location in source file of this application/symbol */
char* getLocation(unsigned long fileoffset) {
  char nodeType;
  char *s,*tmp;
  unsigned long old = hatNodeNumber(),posn;
  while (1) {
    hatSeekNode(fileoffset);
    nodeType=getNodeType();
    switch(nodeType) {
    case SRCREF:
      fileoffset = getModInfo();
      s = getPosnStr();
      tmp = getLocation(fileoffset);
      replaceStr(&tmp,tmp,", ",s);
      // printf(", %s\n",s);
      hatSeekNode(old);
      return tmp;
    case MDSUSPECT:
    case MDTRUSTED:
      tmp = getName();
      replaceStr(&tmp,"module \"",tmp,"\", ");
      //printf("module \"%s\", ",s);
      s = getSrcName();
      replaceStr(&tmp,tmp,"file \"",s);
      replaceStr(&tmp,tmp,"\"","");
      freeStr(s);
      //printf("file \"%s\"",s);
      hatSeekNode(old);
      return tmp;
    case TRAPP:
      {
	//fileoffset = getSrcRef();
	fileoffset = getFunTrace();
	break;
      }
    case TRNAM:
      //fileoffset = getSrcRef();
      fileoffset = getNmType();
      break;
    case NTIDENTIFIER:
      fileoffset = getModInfo();
      s = getPosnStr();
      tmp = getLocation(fileoffset);
      replaceStr(&tmp,tmp,", ",s);
      freeStr(s);
      //printf(", %s\n",s);
      hatSeekNode(old);
      return tmp;
    default:
      hatSeekNode(old);
      return newStr("");
    }
  }
}

/* show location where function was defined */
char* getFunLocation(unsigned long fileoffset) {
  char nodeType;
  char *s,*tmp;
  unsigned long old = hatNodeNumber(),posn;
  while (1) {
    fileoffset = followSATs(fileoffset);
    hatSeekNode(fileoffset);
    nodeType=getNodeType();
    switch(nodeType) {
    case NTIDENTIFIER:
    case NTCONSTRUCTOR:
      fileoffset = getParent();
      s = getPosnStr();
      tmp = getLocation(fileoffset);
      replaceStr(&tmp,tmp,", ",s);
      freeStr(s);
      //printf(", %s\n",s);
      hatSeekNode(old);
      return tmp;
    case SRCREF:
      fileoffset = getModInfo();
      s = getPosnStr();
      tmp = getLocation(fileoffset);
      replaceStr(&tmp,tmp,", ",s);
      //printf(", %s\n",s);
      hatSeekNode(old);
      return tmp;
    case MDSUSPECT:
    case MDTRUSTED:
      s = getName();

      printf("module \"%s\", ",s);
      replaceStr(&s,"module \"",s,"\", ");
      tmp = getSrcName();
      replaceStr(&tmp,tmp,"file \"",s);
      replaceStr(&tmp,tmp,"\"","");
      freeStr(s);
      //printf("file \"%s\"",s);
      hatSeekNode(old);
      return tmp;
    case TRAPP:
      {
	fileoffset = getFunTrace();
	break;
      }
    case TRNAM:
      fileoffset = getNmType();
      break;
    default:
      //printf("no location at offset: 0x%x\n",fileoffset);
      hatSeekNode(old);
      return newStr("<no location>");
    }
  }
}

int isTrusted(unsigned long srcref) {
  char nodeType;
  unsigned long old = hatNodeNumber();
  while (1) {
    hatSeekNode(srcref);
    nodeType=getNodeType();
    switch(nodeType) {
    case TRAPP:
      srcref=getFunTrace();
      break;
    case TRSATA:
    case TRSATB:
    case TRSATC:
    case TRSATCIS:
    case TRSATBIS:
    case TRSATAIS:
    case SRCREF:
      srcref = getParent();
      break;
    case MDSUSPECT:
      hatSeekNode(old);
      return 0;
    case MDTRUSTED:
      hatSeekNode(old);
      return 1;
    case TRNAM:{
      unsigned long nmtype;
      nmtype=getNmType(); // follow nmType by default
      srcref=getSrcRef(); // use srcref if nmType is lambda
      hatSeekNode(nmtype);
      if (getNodeType()!=NTLAMBDA) srcref=nmtype;
      break;
    }
    case NTGUARD:
    case NTIF:  // IFs are trusted. They do the right thing...
    case NTCASE: // CASEs are trusted, same reason...
    case TUPLE:
    case NTCONSTRUCTOR: // constructors are "trusted"! => its applications are ok!
      hatSeekNode(old);
      return 1;
    case NTIDENTIFIER:
      srcref=getModInfo(); // follow module info
      break;
    default:
      hatSeekNode(old);
      return 0;
    }
  }
}

int isCAF(unsigned long fileoffset) {
  char nodeType,i;
  unsigned long old = hatNodeNumber();
  hatSeekNode(fileoffset);
  nodeType=getNodeType();
  hatSeekNode(old);
  switch(nodeType) {
  case TRNAM:
    return 1;
  default:
    return 0;
  }
}

int isTopLevel(unsigned long srcref) {
  char nodeType,i;
  unsigned long old = hatNodeNumber();
  while (1) {
    hatSeekNode(srcref);
    nodeType=getNodeType();
    switch(nodeType) {
    case SRCREF:
      i=(getPosnColumn()==1);
      hatSeekNode(old);
      return i;
    case TRSATA:
    case TRSATB:
    case TRSATC:
    case TRSATAIS:
    case TRSATBIS:
    case TRSATCIS:
      srcref=getParent();
      break;
    case TRNAM:
      srcref=getNmType(); // follow nmType
      break;
    case NTIDENTIFIER:
      i=(getPosnColumn()==1);
      hatSeekNode(old);
      return i;
    case TRAPP:
      srcref = getFunTrace();
      break;
    case NTCASE:
    case NTIF:
    case NTGUARD:
    case NTLAMBDA:
    case NTCONTAINER:
    case NTTUPLE:
    case NTDUMMY:
    case NTFUN:
    case NTCSTRING:
    case TRHIDDEN: // neccessary for isChild
      return 0; // not toplevel (necessary for hat-observe funA in funA!)
    default:
      hatSeekNode(old);
      return 1;
    }
  }
}

int isDescendantOf(unsigned long fileoffset,unsigned long parent) {
  char nodeType;
  unsigned long old = hatNodeNumber();

  if (parent==0) return 0;
  while (fileoffset!=0) {
    hatSeekNode(fileoffset);
    nodeType=getNodeType();
    switch(nodeType) {
    case TRHIDDEN:
      //case TRSATA:
      //case TRSATB:
    case TRSATC:
    case TRSATCIS:
      //case TRSATBIS:
      //case TRSATAIS:
      fileoffset=getParent();
      break;
    case TRNAM:
    case TRAPP:{
      unsigned long newoffs;
      newoffs = getParent();
      if (leftmostOutermost(fileoffset)==parent) {
	hatSeekNode(old);
	return 1;
      }
      fileoffset = newoffs;
      break;
    }
    default:
      hatSeekNode(old);
      return 0;
    }
  }
  hatSeekNode(old);
  return 0;
}

int isDirectDescendantOf(unsigned long fileoffset,unsigned long parent) {
  char nodeType;
  unsigned long old = hatNodeNumber();

  if (parent==0) return 0;
  while (fileoffset!=0) {
    hatSeekNode(fileoffset);
    nodeType=getNodeType();
    switch(nodeType) {
    case TRHIDDEN:
    case TRSATA:
    case TRSATB:
    case TRSATC:
    case TRSATCIS:
    case TRSATBIS:
    case TRSATAIS:
      fileoffset=getParent();
      break;
    case TRAPP:{
      unsigned long newoffs,lmo;
      newoffs = getParent();
      if ((lmo=leftmostOutermost(fileoffset))==parent) {
	hatSeekNode(old);
	return 1;
      }
      if (isTopLevel(fileoffset)) {
	hatSeekNode(old);
	return 0;
      }
      fileoffset = newoffs;
      break;
    }
    default:
      hatSeekNode(old);
      return 0;
    }
  }
  hatSeekNode(old);
  return 0;
}

unsigned long leftmostOutermost(unsigned long fileoffset) {
  char nodeType;
  while (1) {
    hatSeekNode(fileoffset);
    nodeType=getNodeType();
    switch(nodeType) {
    case TRSATA:
    case TRSATB:
    case TRSATBIS:
    case TRSATAIS:
      return 0;
    case TRSATC:
    case TRSATCIS:
      fileoffset=getParent();
      break;
    case TRNAM:
      fileoffset=getNmType(); // follow nmType
      break;
    case NTIDENTIFIER:
    case NTCONSTRUCTOR:
      return fileoffset;
    case TRAPP:
      fileoffset = getFunTrace();
      break;
    default:
      return 0;
    }
  }
}

filepointer hatMainCAF() {
  unsigned long currentOffset,satc,srcref;
  char nodeType;

  hatSeqFirst();
  while (!hatSeqEOF()) {
    currentOffset = hatNodeNumber();
    nodeType = getNodeType();
    switch (nodeType) {
    case TRNAM: // Name
      if (getParent()==0) { // is parent 0?
	srcref = getSrcRef();
	hatSeqNext();
	satc = hatNodeNumber();
	if ((srcref!=0)&&(isSAT(satc))) {  // SATC behind TRNAM?
	  // found a CAF!
	  if (isTrusted(srcref)) printf("isTrusted\n");
	  if (isTopLevel(currentOffset)==0) printf("not top-level!\n");
	  if ((isTrusted(srcref)==0)&&
	      (isTopLevel(currentOffset))) {
	    unsigned long lmo = leftmostOutermost(currentOffset);
	    hatSeekNode(lmo);
	    if ((lmo!=0)&&(getNodeType()==NTIDENTIFIER)&&
		(strcmp(getName(),"main")==0)) {
	      return currentOffset;
	    }
	    hatSeekNode(satc);
	  }
	}
      } else hatSeqNext();
      break;
    default:
      hatSeqNext();
      break;
    }
  }
  return 0;
}

/* set beginning of internal buffer to current position in file */
void resetFilepointer() {
  foff += boff;    // make position in buffer new file offset
  boff = 0;
  lseek(f,foff,0); // set filepointer
  buf_n = read(f,buf,bufsize); // read data
}

/* make sure, next <bytes> are in buffer */
void prepareBuffer(int bytes) {
  if (boff+bytes>=buf_n) {
    resetFilepointer();
  }
}

int getAppArity() {
  char c;
  prepareBuffer(1);
  return buf[boff+1];
}

filepointer getParent() {
  unsigned int lbuf;
  filepointer fp;

  prepareBuffer(5); // need 5 bytes after current position
  lbuf = boff;

  if (nextbyte()==TRAPP) {
    skipbyte(); // skip one byte in applications (arity)
  }
  fp = readpointer();
  boff = lbuf;
  return fp;
}

filepointer getNmType() {
  unsigned int lbuf;
  filepointer fp;

  prepareBuffer(8);
  lbuf = boff;

  skipbytes(5);
  fp = readpointer();

  boff = lbuf;
  return fp;  
}

filepointer getSrcRef() {
  int arity;
  unsigned int lbuf;
  filepointer fp;

  prepareBuffer(4);
  lbuf = boff;

  switch(nextbyte()) {
  case TRAPP:
    arity = nextbyte();
    boff = lbuf;
    prepareBuffer((arity+3)*4+1);
    lbuf = boff;
    skipbytes((arity+2)*4+2);
    fp = readpointer();
    break;
  case TRNAM:
    prepareBuffer(12);
    skipbytes(8);
    fp = readpointer();
    break;
  }
  
  boff = lbuf;
  return fp;
}

filepointer getAppArgument(int i) {
  unsigned int lbuf;
  filepointer fp;
  prepareBuffer((i+3)*4+1);
  lbuf = boff;
  
  skipbytes((i+2)*4+2);
  fp = readpointer();

  boff = lbuf;
  return fp;
}

filepointer getFunTrace() {
  unsigned int lbuf;
  filepointer fp;
  prepareBuffer(9);
  lbuf = boff;

  skipbytes(6);
  fp = readpointer();

  boff = lbuf;
  return fp;
  
}

char getCharValue() {
  unsigned int lbuf;
  char c;

  prepareBuffer(2);
  lbuf = boff;

  skipbyte();
  c=seenextbyte();
  
  boff=lbuf;
  return c;
}

int getIntValue() {
  int i;
  unsigned int lbuf;

  prepareBuffer(4);
  lbuf = boff;

  skipbyte();
  i = readfourbytes().intval;

  boff=lbuf;
  return i;
}


int getIntegerValue() { // int for now!
  int n;
  int i;
  int res=0;
  unsigned int lbuf;

  prepareBuffer(5);
  lbuf = boff;

  skipbyte();
  n = (signed char) nextbyte();
  //printf("integer length %i\n",n);
  i = n;
  if (i<0) i=-i;
  
  boff = lbuf;
  if (n==0) return 0;
  prepareBuffer(2+(i*4));
  lbuf = boff;
  skipbytes(2);
  
  // ATTENTION: |n| is number of bytes - mind the sign!
  //            n==0: value is 0!

  if (n==1) res = readint();else
    if (n==-1) res = - readint();
  // missing implementation for integers with more than 4 bytes!
  //while (i-- > 0) (void)(readfourbytes());

  boff = lbuf;
  return res;
}

#define RATMAX (2*INTEGERMAX+2)
char ratbuf[RATMAX+1];
// rationals not yet used: rational values are still represented as a constructor
// application to two integer arguments!
char *getRationalValue() {
  filepointer fp;

  fp = foff;
  skipbyte();
  sprintf(ratbuf, "%i :%% %i",readinteger(),readinteger());
  foff = fp;
  buf_n=0;
  return newStr(ratbuf);
}

float getFloatValue() {
  unsigned int lbuf;
  fourbytes a;

  prepareBuffer(5);
  lbuf = boff;

  skipbyte();
  a = readfourbytes();
  a.ptrval = htonl(a.ptrval);
  
  boff = lbuf;
  return a.floatval;
}

double getDoubleValue() {
  unsigned int lbuf;
  eightbytes v;
  prepareBuffer(8);
  lbuf = boff;

  v.a[0] = readfourbytes();
  v.a[1] = readfourbytes();
  v.a[1].ptrval = htonl(v.a[1].ptrval);
  v.a[0].ptrval = htonl(v.a[0].ptrval);

  boff = lbuf;
  return v.d;
}

char getNodeType() {
  return seenextbyte();
}

void hatSeqNext() {
  skipNode(nextbyte());
}

char* getName() { // get module, constructor or identifier name
  filepointer fp=foff;
  unsigned int lbuf=boff;
  char* s;
  
  skipbyte();
  s = readstring();
  
  if (fp==foff) boff=lbuf; // simply reset buffer pointer to old position
  else {
    foff = fp; // set filepointer and make buffer invalid
    buf_n = 0;
  }

  return s;
}

char getInfixPrio() {
  filepointer fp=foff;
  unsigned int lbuf=boff;
  char c;

  skipstring();
  skippointer();
  c = seenextbyte();

  if (fp==foff) boff=lbuf; // simply reset buffer pointer to old position
  else {
    foff = fp;             // set filepointer and make buffer invalid
    buf_n = 0;
  }

  return c;
}

filepointer getValueTrace() {
  unsigned int lbuf;
  filepointer trace;
  prepareBuffer(8);
  skipbytes(5);
  trace = readpointer();
  
  lbuf = boff;
  boff = lbuf;
  return trace;
}

char* getSrcName() {
  filepointer fp=foff;
  unsigned int lbuf=boff;
  char* s;
  
  skipbyte();
  skipstring();
  s = readstring();
  
  if (fp==foff) boff=lbuf; // simply reset buffer pointer to old position
  else {
    foff = fp; // set filepointer and make buffer invalid
    buf_n = 0;
  }

  return s;
}

filepointer getModInfo() {
  filepointer fp=foff;
  unsigned int lbuf=boff;
  filepointer res;
  
  if (nextbyte()!=SRCREF) {
    skipstring();  // skip string for NTIdentifier and NTConstructor
  }
  res = readpointer();
  
  if (fp==foff) boff=lbuf; // simply reset buffer pointer to old position
  else {
    foff = fp; // set filepointer and make buffer invalid
    buf_n = 0;
  }

  return res;
}

unsigned long _getPosn() {
  filepointer fp=foff;
  unsigned int lbuf=boff;
  filepointer res;
  
  if (nextbyte()==SRCREF) {
    skipbytes(4);
  } else {
    skipstring(); // for NTCONST, NTIDENT
    skipbytes(5);
  }
  res = readpointer();
  
  if (fp==foff) boff=lbuf; // simply reset buffer pointer to old position
  else {
    foff = fp; // set filepointer and make buffer invalid
    buf_n = 0;
  }

  return res;
}

int getPosnColumn() {
  return (_getPosn() % 10000);
}

int getPosnRow() {
  return (_getPosn() / 10000);
}

char* getPosnStr() {
  unsigned long posn = _getPosn();
  
  sprintf(posnbuf, "line %u, column %u", posn/10000, posn%10000);
  return posnbuf;
}

