/* FunTable.h
   Thorsten Brehm, 4/2001
   Stores a function table. For each function application the list of arguments
   is saved. Applications may be compared, only the most general application
   is stored.
*/

typedef struct lnode* FunTablePtr;

typedef struct lnode {
  ExprNode* funAppl;
  ExprNode* res;
  FunTablePtr next;
} FunTable;

FunTable* newFunTable();
void freeFunTable(FunTable* e);
void addToFunTable(FunTable* l,ExprNode* funAppl,ExprNode* res);
void showFunTable(FunTable* l);
int isInFunTable(FunTable* p,ExprNode* funAppl,ExprNode* res);
