/**************************************************************************/
/* hatinterface.h: general operations on hat files                        */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/

/* include constants for tags in hat files */
#include "hatfile.h"

#ifndef hatinterface
#define hatinterface

typedef unsigned long filepointer;
typedef int BOOL;
typedef int HatFile;

char*         hatFilename(char* name);   // make proper file extension, if missing
HatFile       hatOpenFile(char* name);   // open file for reading, return internal handle
void          hatCloseFile(HatFile h);   // close file in internal handle
void          hatSwitchToHandle(HatFile h);  // switch to hatfile of given handle
HatFile       hatCurrentHandle();        // return current handle (or if non active -1)
BOOL          hatTestHeader();           // check header information

unsigned long hatFileSize();             // return size of file
void          hatSeekNode(filepointer nodenumber);// set new file position
filepointer   hatNodeNumber();           // return position in file
filepointer   hatMainCAF();              // return the main CAF of the trace

/*********************************************************************/
/* Routines to extract values encoded as one or more bytes.          */
/*                                                                   */
/*********************************************************************/


void          hatSeqFirst();         // go to the first node in the trace file
void          hatSeqNext();          // jump to next node in sequential order
BOOL          hatSeqEOF();           // check for EOF

char          getNodeType();         // get type of node

filepointer   getParent();           // get trace to node's parent

int           getAppArity();         // get arity of an application node
filepointer   getAppArgument(int i); // get argument of an application node
filepointer   getFunTrace();         // get function's trace of an application node
char          getInfixPrio();        // get infix priority of an application node

filepointer   getNmType();           // get NameType
filepointer   getValueTrace();       // get value trace of an indirection (projection)

filepointer   getSrcRef();           // get pointer to source reference

char          getCharValue();        // get value of a char node
int           getIntValue();         // get value of an int node
int           getIntegerValue();     // get value of an integer node
char*         getRationalValue();    // get value of a rational node
float         getFloatValue();       // get value of a float node
double        getDoubleValue();      // get value of a double node

char*         getName();             // get constructor, identifier or module name

char*         getSrcName();          // get name of source of an module node

filepointer   getModInfo();          // get module info

int           getPosnColumn();       // get column position in source
int           getPosnRow();          // get row position in source
char*         getPosnStr();          // get position in source as a formatted string

filepointer   getResult(filepointer nodenumber);    // get result node of an application


filepointer   followHidden(filepointer nodenumber); // follow along hidden traces
filepointer   followTrace(filepointer nodenumber);  // follow trace to app or name nodes

filepointer   leftmostOutermost(filepointer nodenumber);

BOOL          isDirectDescendantOf(filepointer nodenumber,filepointer parent);
BOOL          isDescendantOf(filepointer nodenumber,filepointer parent);
BOOL          isCAF(filepointer nodenumber);
BOOL          isTopLevel(filepointer srcref);

char*         getLocation(filepointer nodenumber);    // show source location of
                                                      // application/name
char*         getFunLocation(filepointer nodenumber); // show source location of the
                                                      // applied's function definition
#endif
