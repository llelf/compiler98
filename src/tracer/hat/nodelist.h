/**************************************************************************/
/* nodelist.h:                                                            */
/* simple list operations to manage sorted lists of fileoffsets           */
/*                                                                        */
/* Thorsten Brehm, 5/2001                                                 */
/**************************************************************************/

typedef struct tnode* hNodePtr;  // one pointer to a node

typedef struct tnode {          // one node
  unsigned long fileoffset;     // stored value
  hNodePtr next;                 // pointer to next
} NodeElement;

typedef struct {                // main structure: keep pointer to
  hNodePtr first;                // first and
  hNodePtr last;                 // last element of the list
} NodeList;


NodeList*     newList();        // return an empty list
void          appendToList(NodeList *nl,unsigned long foffset); // append to its end
void          insertInList(NodeList *nl,unsigned long foffset); // insert
void          addBeforeList(NodeList *nl,unsigned long foffset);
int           isInList(NodeList *nl,unsigned long foffset);     // check for value
void          freeList(NodeList *nl);
unsigned long listLength(NodeList *nl);
void          showList(NodeList *nl);                           // show values in list
void          showPretty(NodeList *nl,int verbosemode,
			 unsigned int precision);               // show pretty print






