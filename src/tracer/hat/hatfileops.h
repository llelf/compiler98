/* hatfileops.h
   Thorsten Brehm, 4/2001
   definitions and operations on hat files
*/

/* include constants for tags in hat files */
#include "hatfile.h"

int getline(char s[], int max); /* read string from keyboard */

char* filename(char* name);  /* make proper file extension, if missing */
int   openfile(char* name);  /* open file for reading, save descriptor internally */
void  closefile();           /* close file in internal file descriptor */
void  seek(unsigned long ofs);/* set new file position */
int   more();                 /* check for more data in file */
unsigned long byteoffset();   /* return position in file */


/*********************************************************************/
/* Routines to extract values encoded as one or more bytes.          */
/*                                                                   */
/*********************************************************************/
typedef union {char byte[4];
               unsigned long ptrval;
	       long intval;
	       float floatval;} fourbytes;

char nextbyte();    /* read one byte from file */
char seenextbyte(); /* get the next byte without moving the file pointer */
void skipbyte();    /* just skip one byte in file - don't care to read it */
char *readstring(); /* read a string */
void skipstring();  /* skip one string in file, don't bother about its value */
fourbytes readfourbytes(); /* read four bytes from file */
void skipbytes(int bytes); /* skip number of bytes in buffer */
char *readposn();   /* read source position, return as pretty print */
void skipposn();    /* skip position value */
unsigned long readpointer();
void skippointer();
char readchar();
int readint();
void skipint();
int readinteger();
void skipinteger();
char *readrational();
float readfloat();
double readdouble();
int readarity();
int readfixpri();
void skipNode(char nodeType); /* skip entire node of given type */

int tagat(unsigned long offset);
int hi3(char b);
int lo5(char b);

int testheader(); /* reading and checking header information */


unsigned long followHidden(unsigned long fileoffset); /* follow Hidden traces */

/* follow trace until an application or name */
unsigned long followTrace(unsigned long fileoffset);

/* search for the SAT of a given application */
unsigned long findAppSAT(unsigned long fileoffset);


/* show location in source file of this application/symbol */
void showLocation(unsigned long fileoffset);
/* show location where function was defined */
void showFunLocation(unsigned long fileoffset);


/* read whole expression to memory */
ExprNode* buildExpr(unsigned long fileoffset,int buildUnevaldepth);


/*********************************************************************/
/* list data structure: keep a list of file offsets in memory        */
/*                                                                   */
/*********************************************************************/

typedef struct tnode* NodePtr;

typedef struct tnode {
  unsigned long fileoffset;
  NodePtr next;
} NodeElement;

typedef struct {
  NodePtr first;
  NodePtr last;
} NodeList;


NodeList* newList(); /* new, empty list */
void      appendToList(NodeList *nl,unsigned long foffset);
void      insertInList(NodeList *nl,unsigned long foffset);
void      addBeforeList(NodeList *nl,unsigned long foffset);
int       isInList(NodeList *nl,unsigned long foffset);
void      showList(NodeList *nl);
void      showPretty(NodeList *nl,int verbosemode);
void      freeList(NodeList *nl);
unsigned long listLength(NodeList *nl);

struct stat hatStatBuf;





