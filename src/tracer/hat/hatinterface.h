/**************************************************************************/
/* hatinterface.h: general operations on hat files                        */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/

#ifndef hatinterface
#define hatinterface

/* include constants for tags in hat files */
#include "hatfile.h"

#define HatFileNotFound    -1
#define HatFileBadVersion  -2
#define InvalidFilePointer 0

typedef unsigned long filepointer;
typedef int BOOL;
typedef int HatFile;

char*         hatFileExtension (char* name); // add file extension if missing
HatFile       hatOpenFile  (char* name);  // open file, return internal handle
void          hatCloseFile (HatFile h);   // close file in internal handle
char*         hatFileName  (HatFile h);   // return file name
unsigned long hatFileSize  (HatFile h);   // return size of file

filepointer   hatErrorPoint(HatFile h);   // last redex of an aborted evaluation
                                          // (otherwise InvalidFilePointer)
char*         hatErrorText (HatFile h);   // error message, otherwise NULL
filepointer   hatMainCAF   (HatFile h);   // return the main CAF of the trace

filepointer   hatSeqFirst  (HatFile h);   // go to the first node in seq. order
filepointer   hatSeqNext   (HatFile h,    // jump to next node in seq. order
			    filepointer nodenumber);
BOOL          hatSeqEOF    (HatFile h,    // check for EOF
			    filepointer nodenumber);

filepointer   hatNodeNumber(HatFile h);   // return current position in file
void          hatSeekNode  (HatFile h,    // set position in file
			    filepointer ofs);

/*********************************************************************/
/* Routines to extract information from the structure.               */
/*                                                                   */
/*********************************************************************/

char          getNodeType  (HatFile h,               // get type of node and
			    filepointer nodenumber); // prepare node for access


filepointer   getParent       ();     // trace to node's parent
filepointer   getResult       ();     // result node of a redex
                                      // InvalidPointer returned otherwise
char          getAppArity     ();     // arity of an application node
filepointer   getAppArgument  (int i);// argument of an application node
filepointer   getAppFun       ();     // function trace of an application

char          getInfixType    ();     // infix type of constructr/identifier
char          getInfixPrio    ();     // infix priority of constr./ident. node

filepointer   getNameType     ();     // type reference of a name node

filepointer   getProjValue    ();     // value of a projection or SAT node

filepointer   getSrcRef       ();     // get source reference of a node

char          getCharValue    ();     // value of a char node
char*         getStringValue  ();     // value of a CString node
int           getIntValue     ();     // value of an int node
int           getIntegerValue ();     // value of an integer node
char*         getRationalValue();     // value of a rational node
float         getFloatValue   ();     // value of a float node
double        getDoubleValue  ();     // value of a double node

char*         getName         ();     // constructor, identifier or module name
BOOL          getTopLevelFlag ();     // determine toplevel flag TRUE/FALSE
filepointer   getModInfo      ();     // module info
char*         getModuleSrcName();     // source module name of a module node
BOOL          getModuleTrusted();     // 0 for untrusted module, 1 for trusted

int           getPosnColumn   ();     // column position in source code
int           getPosnRow      ();     // row position in source code
char*         getPosnStr      ();     // source position as a formatted string


/*********************************************************************/
/* Routines for convenience: testing                                 */
/*                                                                   */
/*********************************************************************/

BOOL          isTopLevel(HatFile handle,
			 filepointer nodenumber);
BOOL          isTrusted (HatFile handle,
			 filepointer nodenumber);
BOOL          isCAF     (HatFile handle,
			 filepointer nodenumber);
BOOL          isSAT     (HatFile handle,
			 filepointer nodenumber);

/*********************************************************************/
/* Routines for convenience: following traces                        */
/*                                                                   */
/*********************************************************************/

filepointer   hatFollowHidden(HatFile handle,         // follow trail of
			      filepointer nodenumber);// hidden traces
filepointer   hatFollowTrace(HatFile handle,          // follow trail of SATCs,
			     filepointer nodenumber); // Projections to constr.,
                                                      // identifier or applicat.

filepointer   hatFollowSATCs(HatFile handle,filepointer fileoffset);
filepointer   hatOutermostSymbol(HatFile handle,  // return outermost symbol
		     filepointer nodenumber);     // of an application 
filepointer   hatOutermostName(HatFile handle,    // return outermost name node
			 filepointer nodenumber); // representing a symbol
filepointer   hatInitialCAF(HatFile handle,       // find top-most ancestor (is
			    filepointer fileoffset); // always a CAF)
filepointer   hatResult    (HatFile handle,       // result node of a redex
			    filepointer nodenumber);


/*********************************************************************/
/* Routines for convenience: Locations as strings                    */
/*                                                                   */
/*********************************************************************/

char*         hatLocationStr(HatFile handle,          // show source location of
			     filepointer nodenumber); // application/name
char*         hatFunLocationStr(HatFile handle,       // show source location of
				filepointer nodenumber); // function definition

#endif




