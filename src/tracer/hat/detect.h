/**************************************************************************/
/* detect.h: module for algorithmic debugging of traces                   */
/*                                                                        */
/* Thorsten Brehm, 5/2001                                                 */
/**************************************************************************/


BOOL isChildOf(filepointer nodenumber,filepointer parent);

void getChildrenFor(NodeList* nl,unsigned long parentTrace,unsigned long current,
		    HashTable* hash);

int  getEDTchildren(unsigned long parentTrace,int **childrenArray);

void freeArray(int *array);
