/**************************************************************************/
/* observe.h: searches a hat redex file for all applications of a given   */
/* top level identifier.                                                  */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/

typedef struct _ObserveQuery *ObserveQuery;

  /* newQuery: creates a query on a hat file, searching for applications of
     identifierNode in the file.
     If topIdentifierNode not null, only applications within this function
     are selected.
     If recursiveMode is 1, applications of identifierNode within itself are
     omitted. */
ObserveQuery newObserveQuery(            // create a new observe query
	     HatFile handle,             // hat file handler
	     filepointer identifierNode, // node number of searched identifier
	     filepointer topIdentifierNode, // 0=none
	     BOOL recursiveFilter,       // =1: omit recursive applications
	     BOOL showProgress);         // =1: show progress indicator (0-100%)

  /* same as newQuery, but the identifier (and topIdentifier) are passed as strings
     rather than the node number. */
ObserveQuery newObserveQueryIdent(
             HatFile handle,
	     char* identifier,     // searched identifier
	     char* topIdentifier,  // within topIdentifier (or NULL=none)
	     BOOL recursiveFilter, // =1: omit recursive applications
	     BOOL showProgress);   // =1: show progress indicator (0-100%)

  /* create a query to observe all values at a given source position.
     The source position is given by the moduleName and the line/column
     position.
     If moduleName is NULL, the main module is searched, that is the module,
     in which the main CAF was defined. */
ObserveQuery newObserveQuerySource(int handle,
				   char* moduleName,
				   unsigned long line,unsigned long column,
				   BOOL showProgress);

  /* newQuery: creates a query on a hat file, searching for all observable
     identifiers and modules within the file. */
ObserveQuery newObservableQuery(int handle,
				BOOL showProgress);

void         freeObserveQuery(ObserveQuery query); // end query, free memory

filepointer  nextObserveQueryNode(ObserveQuery query); // 0=none, otherwise: next node returned

  /* observeUnique: observe all unique and most general applications within the query.
     The entire file is searched first, before the result is determined.
     Observed applications are returned as a FunTable.
  */
FunTable    observeUnique(
            ObserveQuery query,  // observe unique, most general applications
	    BOOL verboseMode,    // =1: show unevaluated expressions
	    int precision);      // precision depth of output (how
                                 //  far to recurse into structures)

filepointer observeIdentifier(ObserveQuery query);

filepointer observeTopIdentifier(ObserveQuery query);

unsigned long observedNodes(ObserveQuery query);

HatFile     observeHatFile(ObserveQuery query); // return HatFile handler
