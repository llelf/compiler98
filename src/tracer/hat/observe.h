/******************************************************************************/
/* observe.h: module providing observational view on Redex Trail files        */
/*                                                                            */
/* Thorsten Brehm, 11/2001                                                    */
/******************************************************************************/

  /* abstract data type: ObserveQuery                                         */
typedef struct hiddenObserveQuery* ObserveQuery;

  /* newQuery: creates a query, searching for all applications of
     identifierNode.
     When topIdentifierNode!=InvalidFilePointer then the scope is
     restricted to the function body of the function "topIdentifierNode".
     When recursiveMode==1 then applications representing recursive
     calls are omitted.                                                       */
ObserveQuery newObserveQuery(
	     HatFile handle,
	     filepointer identifierNode,
	     filepointer topIdentifierNode,
	     BOOL recursiveFilter,
	     BOOL showProgress);

  /* same as newObserveQuery but the identifier (and topIdentifier) are
     passed as strings rather than the node number.
     String for topIdentifier maybe "null" if the scope shall not be
     restricted.                                                              */
ObserveQuery newObserveQueryIdent(
             HatFile handle,
	     char* identifier,
	     char* topIdentifier,
	     BOOL recursiveFilter,
	     BOOL showProgress);

  /* Query to observe all values at a given source position.
     The source position is given by the moduleName and the line/column
     position.
     If moduleName is NULL, the main module is searched (main module is the
     module where the main CAF is defined.                                    */
ObserveQuery newObserveQuerySource(HatFile handle,
				   char* moduleName,
				   unsigned long line,unsigned long column,
				   BOOL showProgress);

  /* Query searching for all observable identifiers and modules.              */
ObserveQuery newObservableQuery(HatFile handle,
				BOOL showProgress);
  /* end query, free memory                                                   */
void         freeObserveQuery(ObserveQuery query);

  /* get next observation from query.
     InvalidFilePointer is returned when query has finished.                  */
filepointer  nextObserveQueryNode(ObserveQuery query);

  /* Observe the most general applications within the query only.
     The entire file is searched first, before the result is determined.
     Observed applications are returned as a FunTable. (obsolete)
  */
FunTable    observeUnique(      // obsolete
            ObserveQuery query,
	    BOOL verboseMode,
	    int precision);

  /* get node representing the searched identifier within the query           */
filepointer observeIdentifier(ObserveQuery query);

  /* get node representing the topIdentifier within the query                 */
filepointer observeTopIdentifier(ObserveQuery query);

  /* get number of nodes observed (so far!)                                   */
unsigned long observedNodes(ObserveQuery query); // obsolete!

  /* get the HatFile in which the query searches                              */
HatFile     observeHatFile(ObserveQuery query);
