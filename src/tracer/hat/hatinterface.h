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

char*         hatFileExtension (char* name);  // add proper file extension, if missing
HatFile       hatOpenFile  (char* name);  // open file, return internal handle
void          hatCloseFile (HatFile h);   // close file in internal handle
char*         hatFileName  (HatFile h);   // return file name represented by handler
unsigned long hatFileSize  (HatFile h);   // return size of file

filepointer   hatErrorPoint(HatFile h);   // return the error entry point, if
                                          // evaluation failed, otherwise 0 returned
char*         hatErrorText (HatFile h);   // return the error message, otherwise NULL
filepointer   hatMainCAF   (HatFile h);   // return the main CAF of the trace

filepointer   hatSeqFirst  (HatFile h);   // go to the first node in the trace file
filepointer   hatSeqNext   (HatFile h,    // jump to next node in sequential order
			    filepointer nodenumber);
BOOL          hatSeqEOF    (HatFile h,    // check for EOF
			    filepointer nodenumber);

filepointer   hatNodeNumber(HatFile h);   // return position in file
void          hatSeekNode  (HatFile h,    // set position in file
			    filepointer ofs);

/*********************************************************************/
/* Routines to extract information from the structure.               */
/*                                                                   */
/*********************************************************************/

filepointer   getResult    (HatFile handle, // get result node of an application
			    filepointer nodenumber);


char          getNodeType  (HatFile h,      // get type of node
			    filepointer nodenumber);


filepointer   getParent       ();     // get trace to node's parent

char          getAppArity     ();     // get arity of an application node
filepointer   getAppArgument  (int i);// get argument of an application node
filepointer   getAppFun       ();     // get function's trace of an application node

char          getInfixType    ();     // get infix type of constructr/identifier node
char          getInfixPrio    ();     // get infix priority of constr/ident node

filepointer   getNameType     ();     // get type reference of a name node

filepointer   getProjTrace    ();     // get value trace of an indirection (projection)

filepointer   getSrcRef       ();     // get pointer to source reference

char          getCharValue    ();     // get value of a char node
char*         getStringValue  ();     // get value of a CString node
int           getIntValue     ();     // get value of an int node
int           getIntegerValue ();     // get value of an integer node
char*         getRationalValue();     // get value of a rational node
float         getFloatValue   ();     // get value of a float node
double        getDoubleValue  ();     // get value of a double node

char*         getName         ();     // get constructor, identifier or module name

filepointer   getModInfo      ();     // get module info

char*         getModuleSrcName();     // get name of source of an module node
BOOL          getModuleTrusted();     // return 0 for untrusted module, 1 for trusted

int           getPosnColumn   ();     // get column position in source
int           getPosnRow      ();     // get row position in source
char*         getPosnStr      ();     // get position in source as a formatted string


/*********************************************************************/
/* Routines for convenience: testing                                 */
/*                                                                   */
/*********************************************************************/

BOOL          isDirectDescendantOf(HatFile handle,
				   filepointer nodenumber,
				   filepointer parent);
BOOL          isDescendantOf      (HatFile handle,
			           filepointer nodenumber,
				   filepointer parent);
BOOL          isCAF     (HatFile handle,
			 filepointer nodenumber);
BOOL          isTopLevel(HatFile handle,
			 filepointer nodenumber);
BOOL          isSAT     (HatFile handle,
			 filepointer nodenumber);
BOOL          isTrusted (HatFile handle,
			 filepointer nodenumber);

/*********************************************************************/
/* Routines for convenience: following traces                        */
/*                                                                   */
/*********************************************************************/

filepointer   hatFollowHidden(HatFile handle, // follow along hidden traces
			      filepointer nodenumber);
filepointer   hatFollowTrace(HatFile handle,  // follow trace to app or name nodes
			     filepointer nodenumber);
filepointer   hatFollowSATs(HatFile handle,filepointer fileoffset);
filepointer   hatLMO(HatFile handle,          // return leftmost outermost symbol
		     filepointer nodenumber); // of application 
filepointer   hatLMOName(HatFile handle,      // return leftmost outermost symbol
			 filepointer nodenumber); // represented by its name node
filepointer   hatInitialCAF(HatFile handle,   // find the top-most CAF which initially
			    filepointer fileoffset); // demanded evaluation of fileoffset

/*********************************************************************/
/* Routines for convenience: Locations as strings                    */
/*                                                                   */
/*********************************************************************/

char*         hatLocationStr(HatFile handle,             // show source location of
			     filepointer nodenumber);    // application/name
char*         hatFunLocationStr(HatFile handle,          // show source location of the
				filepointer nodenumber); // applied's function definition

#endif




