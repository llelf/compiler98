/**************************************************************************/
/* observe.h: searches a hat redex file for all applications of a given   */
/* top level identifier.                                                  */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/

unsigned long observeNode(unsigned long identifierNode,unsigned long topIdentifierNode,
			  int verbosemode,int uniqueMode,int recursivemode,
			  unsigned int precision,FunTable* results);

void observeIdentifier(char* ident,char* topIdent,
		       int verbosemode,int uniqueMode,int recursivemode,
		       unsigned int precision,FunTable* results);

int getObserve(char* ident,char* topIdent,
	       int verbosemode,int uniqueMode,int recursivemode,
	       unsigned int precision,int **childrenArray);
