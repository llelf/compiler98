/**************************************************************************/
/* Expressions.h:                                                         */
/* Basic definition of expression tree representation and definition of   */
/* functions operating on them                                            */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/

/* types for syntax tree of expressions */
typedef struct expnode *ExprPtr;

/* Node for an application */
typedef struct apnode {
  int   arity;
  ExprPtr fun;
  ExprPtr *args; // Array of all arguments
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

int        getArity(ExprNode* e);
int        getInfixPrio(ExprNode* e);

/* return pretty print of the expression */
char*      prettyPrintExpr(ExprNode* exp,int verboseMode);

/* compares two expressions.
   result: 0 if same,
          -1 if e1<e2, (e1 less defined than e2),
           1 if e1>e2,
           2 if expressions incomparable
*/
int        compareExpr(ExprNode* e1, ExprNode* e2);

/* basic functions for allocating and freeing strings on heap */ 

/* reserve space on heap for given string - and copy */
char*      newStr(char* str);

/* append strings and reserve space for the result string */
char*      catStr (char* s1, char* s2, char* s3);

/* replace string in s with the concatenation of s1,s2 and s3 */
void       replaceStr(char** s,char* s1,char *s2,char* s3);

/* free memory space */
void       freeStr(char* s);

char* treePrint(ExprNode* exp,int verbose,int topInfixprio);
