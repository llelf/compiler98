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

BOOL isChildOf(HatFile handle,filepointer nodenumber,filepointer parent);



void getChildrenFor(HatFile handle,NodeList* nl,
		    filepointer parentTrace,filepointer current,
		    HashTable* hash);

int  getEDTchildren(HatFile handle,filepointer parentTrace,int **childrenArray);

void freeArray(int *array);
