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
  unsigned long fileoffset;
  FunTablePtr next;
} FunTable;

FunTable* newFunTable();
void freeFunTable(FunTable* e);
void addToFunTable(FunTable* l,ExprNode* funAppl,ExprNode* res,unsigned long fileoffset);
void showFunTable(FunTable* l);
long showFunTablePaged(FunTable* l);
int isInFunTable(FunTable* p,ExprNode* funAppl,ExprNode* res);
unsigned long getFunTableFileOffs(FunTable*l,long i);
