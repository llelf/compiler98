/**************************************************************************/
/* detect.h: module for algorithmic debugging of traces                   */
/*                                                                        */
/* Thorsten Brehm, 5/2001                                                 */
/**************************************************************************/

/*
 typedef struct _EDTQuery *EDTQuery;

 EDTQuery     newEDTQuery(            // create a new observe query
	     HatFile handle,         // hat file handler
	     filepointer parent);    // node number of searched identifier

 void         freeEDTQuery(EDTQuery query); // end query, free memory

 filepointer  nextEDTQueryNode(EDTQuery query); // 0=none, otherwise: next node returned
*/

BOOL         isChildOf(filepointer nodenumber,filepointer parent);



void getChildrenFor(NodeList* nl,unsigned long parentTrace,unsigned long current,
		    HashTable* hash);

int  getEDTchildren(unsigned long parentTrace,int **childrenArray);

void freeArray(int *array);
