/**************************************************************************/
/* integerList.h:                                                         */
/* simple list operations to manage sorted lists of integers              */
/*                                                                        */
/* Thorsten Brehm, 5/2001                                                 */
/**************************************************************************/

typedef struct tnode* hNodePtr;  // one pointer to a node

typedef struct tnode {             // one node
  filepointer fileoffset;             // stored value
  hNodePtr next;                 // pointer to next
} NodeElement;

typedef struct {                   // main structure: keep pointer to
  hNodePtr first;                // first and
  hNodePtr last;                 // last element of the list
} NodeList;


NodeList*     newList();           // return an empty list
void          appendToList(NodeList *nl,filepointer foffset); // append to its end
void          insertInList(NodeList *nl,filepointer foffset); // insert
void          addBeforeList(NodeList *nl,filepointer foffset);
int           isInList(NodeList *nl,filepointer foffset);     // check for value
void          removeFromList(NodeList *nl,filepointer foffset);// remove from list
void          freeList(NodeList *nl);
unsigned long listLength(NodeList *nl);
void          showList(NodeList *nl);                           // show values in list
void          showPretty(HatFile handle,NodeList *nl,int verbosemode,
			 unsigned int precision);               // show pretty print
filepointer firstElement(NodeList *nl);
filepointer firstBigger(NodeList *nl,filepointer current);



