/**************************************************************************/
/* detect.h: module providing EDT view on Redex Trail files               */
/*                                                                        */
/* Thorsten Brehm, 11/2001                                                */
/**************************************************************************/


  /* abstract data type: EDTQuery                                         */
typedef struct hiddenEDTQuery* EDTQuery;

  /* create new query: get EDT children of "parent"                       */
EDTQuery     newEDTQuery      (HatFile handle,
			       filepointer parent);

  /* end query, free memory                                               */
void         freeEDTQuery     (EDTQuery query);

  /* get next EDT child in query (InvalidFilePointer signals end of query)*/
filepointer  nextEDTQueryNode (EDTQuery query);

  /* get root node of EDT                                                 */
filepointer  getEDTroot       (HatFile handle);

  /* determine whether "maybechild" is an EDT child of "parent"           */
BOOL         isEDTChild       (HatFile handle,
			       filepointer maybechild,
			       filepointer parent);

