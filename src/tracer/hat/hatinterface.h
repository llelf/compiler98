/******************************************************************************/
/* hatinterface.h: general operations on hat files                            */
/*                                                                            */
/* Thorsten Brehm, 11/2001                                                    */
/******************************************************************************/

#ifndef hatinterface
#define hatinterface

/* include constants for tags in hat files */
#include "hatfile.h"

typedef int BOOL;

typedef struct hiddenFilePointer* filepointer;  // abstract data type for a node
#define InvalidFilePointer (filepointer) 0

typedef struct hiddenHatFile* HatFile;          // abstract data type for a file
#define HatFileNotFound    (HatFile) -1
#define HatFileBadVersion  (HatFile) -2

char*         hatVersionNumber ();              // current Hat version number
char*         hatFileExtension (char* name);    // add file extension if missing


/******************************************************************************/
/* Trace management: Operations for abstract data type "HatFile"              */
/******************************************************************************/

HatFile       hatOpenFile  (char* name);  // open file, return internal handle
void          hatCloseFile (HatFile h);   // close file in internal handle
char*         hatFileName  (HatFile h);   // return file name
unsigned long hatFileSize  (HatFile h);   // return size of file

filepointer   hatErrorRedex(HatFile h);   // last redex of an aborted evaluation
                                          // (otherwise InvalidFilePointer)
char*         hatErrorMessage(HatFile h); // error message, otherwise NULL
filepointer   hatMainCAF   (HatFile h);   // return the main CAF of the trace

filepointer   hatSeqFirst  (HatFile h);   // go to the first node in seq. order
filepointer   hatSeqNext   (HatFile h,    // jump to next node in seq. order
			    filepointer nodenumber);
BOOL          hatSeqEOF    (HatFile h,    // check for EOF
			    filepointer nodenumber);
int           perProgress  (HatFile h,    // return progress indication (0-100)
			   filepointer f);// percental filepos. relative to size

filepointer   hatNodeNumber(HatFile h);   // return current position in file
void          hatSeekNode  (HatFile h,    // set position in file
			    filepointer ofs);


/******************************************************************************/
/* Node data access                                                           */
/******************************************************************************/

char          getNodeType  (HatFile h,               // get type of node and
			    filepointer nodenumber); // prepare node for access

 // for all Trace nodes:
filepointer   getParent       ();     // trace to node's parent
filepointer   getResult       ();     // result node of a redex
                                      // InvalidPointer returned otherwise
 // for application nodes only:
char          getAppArity     ();     // arity of an application node
filepointer   getAppArgument  (int i);// argument "i" of an application node
filepointer   getAppFun       ();     // function trace of an application

 // for constructor/identifier nodes:
char          getInfixType    ();     // infix type of constructr/identifier
char          getInfixPrio    ();     // infix priority of constr./ident. node

 // for constructor/identifier/module nodes:
char*         getName         ();     // constructor, identifier or module name

 // for identifier nodes:
BOOL          getTopLevelFlag ();     // determine toplevel flag TRUE/FALSE

 // for constant nodes:
filepointer   getAtom         ();     // type reference of a name node

 // for projection nodes:
filepointer   getProjValue    ();     // value of a projection or SAT node

 // for constant/application/constructor/identifier nodes:
filepointer   getSrcRef       ();     // get source reference of a node

 // for Atom nodes of according type
char          getCharValue    ();     // value of a char node
char*         getStringValue  ();     // value of a CString node
int           getIntValue     ();     // value of an int node
int           getIntegerValue ();     // value of an integer node
char*         getRationalValue();     // value of a rational node
float         getFloatValue   ();     // value of a float node
double        getDoubleValue  ();     // value of a double node

 // for source reference nodes:
filepointer   getModInfo      ();     // returns a module node
int           getPosnColumn   ();     // column position in source code
int           getPosnRow      ();     // row position in source code
char*         getPosnStr      ();     // source position as a formatted string

 // for module nodes:
char*         getModuleSrcName();     // source module name of a module node
BOOL          getModuleTrusted();     // 0 for untrusted module, 1 for trusted


/******************************************************************************/
/* Common operations                                                          */
/******************************************************************************/

/* testing                                                                    */
BOOL          isTopLevel     (HatFile handle,
			      filepointer nodenumber);
BOOL          isTrusted      (HatFile handle,
			      filepointer nodenumber);
BOOL          isTopLevelOrTrusted(HatFile handle,
				  filepointer srcref);
BOOL          isCAF          (HatFile handle,
			      filepointer nodenumber);
BOOL          isSAT          (HatFile handle,
			      filepointer nodenumber);

/* following trails                                                          */
filepointer   hatFollowHidden(HatFile handle,         // follow trail of
			      filepointer nodenumber);// hidden traces

filepointer   hatFollowTrace (HatFile handle,         // follow trail of SATCs,
			      filepointer nodenumber);// Projections to constr.,
                                                      // identifier or applicat.

filepointer   hatFollowSATCs (HatFile handle,         // follow trail of SATCs
			      filepointer fileoffset);// only

filepointer   hatOutermostSymbol(HatFile handle,      // outermost symbol
                              filepointer nodenumber);// of an application 

filepointer   hatOutermostName(HatFile handle,        // outermost name node
			 filepointer nodenumber);     // representing a symbol
filepointer   hatTopAncestor(HatFile handle,          // find top-most ancestor
			     filepointer fileoffset); // (is always a CAF)

/* get result of a redex (getNodeType(..)+getResult)                          */
filepointer   hatResult    (HatFile handle,           // result node of a redex
			    filepointer nodenumber);

/* Locations as strings                                                       */
char*         hatLocationStr(HatFile handle,          // show source location of
			     filepointer nodenumber); // application/name
char*         hatFunLocationStr(HatFile handle,       // show source location of
				filepointer nodenumber); // function definition

#endif




