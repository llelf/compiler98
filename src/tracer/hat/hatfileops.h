/**************************************************************************/
/* hatfileops.h: general operations on hat files                          */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/

/* include constants for tags in hat files */
#include "hatfile.h"


int getline(char s[], int max); /* read string from keyboard */

char*         filename(char* name);   /* make proper file extension, if missing */
int           openfile(char* name);   /* open file for reading, save descriptor */
void          closefile();            /* close file in internal file descriptor */
void          seek(unsigned long ofs);/* set new file position */
int           more();                 /* check for more data in file */
unsigned long byteoffset();           /* return position in file */
int           testheader();            /* reading and checking header information */


void          saveHandle(int h);
void          loadHandle(int h);
void          switchToHandle(int h);
int           newHatHandle();

/*********************************************************************/
/* Routines to extract values encoded as one or more bytes.          */
/*                                                                   */
/*********************************************************************/

typedef unsigned long filepointer;

/* abstracting interface below */

char          getNodeType();         // get type of node
void          nextNode();            // jump to next node
filepointer   getTrace();            // get trace to node's parent
int           getAppArity();         // get arity of an application node
filepointer   getAppArgument(int i); // get argument of an application node
filepointer   getFunTrace();         // get function's trace of an application node
int           getInfixPrio();        // get infix priority of an application node
filepointer   getNmType();           // get NameType
filepointer   getSrcRef();           // get pointer to source reference
char          getCharValue();        // get value of a char node
int           getIntValue();         // get value of an int node
int           getIntegerValue();     // get value of an integer node
char*         getRationalValue();    // get value of a rational node
float         getFloatValue();       // get value of a float node
double        getDoubleValue();      // get value of a double node
char*         getName();             // get constructor, identifier or module name
char*         getSrcName();          // get name of source of an module node
filepointer   getValueTrace();       // get value trace of an indirection (projection)
filepointer   getModInfo();          // get module info
unsigned long getPosn();             // get position in source
char*         getPosnStr();          // get position in source as a formatted string


unsigned long followHidden(unsigned long fileoffset); /* follow Hidden traces */

/* follow trace until an application or name */
unsigned long followTrace(unsigned long fileoffset);

/* search for the SAT of a given application */
unsigned long findAppSAT(unsigned long fileoffset);

filepointer   mainCAF(); /* return the main CAF of the trace */

unsigned long leftmostOutermost(unsigned long fileoffset);
int           isDirectDescendantOf(unsigned long fileoffset,unsigned long parent);
int           isDescendantOf(unsigned long fileoffset,unsigned long parent);
int           isChildOf(unsigned long fileoffset,unsigned long parent);
int           isCAF(unsigned long fileoffset);
int           isTopLevel(unsigned long srcref);

/* show location in source file of this application/symbol */
void          showLocation(unsigned long fileoffset);
/* show location where function was defined */
void          showFunLocation(unsigned long fileoffset);


/* read whole expression to memory */
ExprNode*     buildExpr(unsigned long fileoffset,int verbose,
			unsigned int precision);









