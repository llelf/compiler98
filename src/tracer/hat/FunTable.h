/**************************************************************************/
/* FunTable.h                                                             */
/* Stores a function table. For each function application the list of     */
/* arguments is saved. Applications may be compared, only the most        */
/* general application is stored.                                         */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/


typedef struct _FunTable* FunTable;

FunTable      newFunTable();
void          freeFunTable(FunTable e);

int           FunTableLength(FunTable e);

void          addToFunTable(FunTable l,ExprNode* funAppl,ExprNode* res,
			    filepointer nodenumber);

int           isInFunTable(FunTable p,ExprNode* funAppl,ExprNode* res);

void          getFunTableEntry(FunTable l,long i,
			       filepointer* fp,
			       ExprNode** appl,
			       ExprNode** res);

void          FunTableCheckArities(FunTable ftable);

unsigned long showFunTable(FunTable l);
