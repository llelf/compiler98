/**************************************************************************/
/* Expressions.h:                                                         */
/* Basic definition of expression tree representation and definition of   */
/* functions operating on them                                            */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/

#include "hatinterface.h"

/* types for syntax tree of expressions */
typedef struct expnode *ExprPtr;

/* Node for an application */
typedef struct apnode {
  int   arity;
  ExprPtr fun;
  ExprPtr *args; // Array of all arguments
  char* cycle;   // string with cycle ID (or NULL): do NOT free this string
  //                referenced string is freed by MESSAGE node
} AppNode;

/* node for an identifier */
typedef struct idnode {
  char* name;
  int infixtype;
  int infixpriority;
} IdentNode;

/* general type of node. */
typedef union {
  char charval;
  IdentNode* identval;
  AppNode* appval;
  long intval;
  float floatval;
  char* message;
  double* doubleval;
  filepointer fptr;
  ExprPtr expr;
} NodeTypes;

/* expression node: type determines representation of node in v */
typedef struct expnode {
  int type;
  NodeTypes v;
} ExprNode;


/* functions operating on expressions */

AppNode*   newAppNode(int arity);
void       freeAppNode(AppNode* a);
void       setAppNodeFun(AppNode* a,ExprNode* e);
ExprNode*  getAppNodeFun(AppNode* a);
void       setAppNodeArg(AppNode* a,int i,ExprNode* e);
ExprNode*  getAppNodeArg(AppNode* a,int i);

IdentNode* newIdentNode(char* name,int infix,int prio);
void       freeIdentNode(IdentNode* id);

ExprNode*  newExprNode(int type);
void       freeExprNode(ExprNode* e); /* free a single node */

void       freeExpr(ExprNode* e); /* free the entire structure */

int        getExprArity(ExprNode* e);
int        getExprInfixPrio(ExprNode* e);


ExprNode*  buildExpr(HatFile handle,
		     filepointer nodenumber,int verbose, // build expression tree
		     unsigned int precision);            // in memory

/* return pretty print of the expression */
char*      prettyPrintExpr(ExprNode* exp,unsigned int precision,int verboseMode);

/* compares two expressions.
   result: 0 if same,
          -1 if e1<e2, (e1 less defined than e2),
           1 if e1>e2,
           2 if expressions incomparable
*/
int        compareExpr(ExprNode* e1, ExprNode* e2);

char* treePrint(ExprNode* exp,int verbose,int topInfixprio);

void showNode(HatFile handle,filepointer fileoffset,int verboseMode,
	      unsigned int precision);

filepointer showAppAndResult(HatFile handle,filepointer fileoffset,int verboseMode,
			       unsigned int precision);

