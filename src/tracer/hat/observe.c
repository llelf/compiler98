/**************************************************************************/
/* observe.c: searches a hat redex file for all applications of a given   */
/* top level identifier.                                                  */
/*                                                                        */
/* Thorsten Brehm, 4/2001                                                 */
/**************************************************************************/

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include "Expressions.h"
#include "FunTable.h"
#include "hatinterface.h"
#include "hashtable.h"
#include "menu.h"
#include "observe.h"
// optional switch


//#define showNodeInfo // show additional information about file offsets

typedef struct {
  HashTable* htable;
  unsigned long identifierNode;
  unsigned long topIdentifierNode;
  int recursiveMode;
  unsigned long currentOffset;
  unsigned long fsz;
  unsigned long lsz;
  int found;
  int finished;
  int showProgress;
  int handle;
} _ObserveQuery;


ObserveQuery newObserveQuery(int handle,
		       filepointer identifierNode,
		       filepointer topIdentifierNode,
		       BOOL recursiveMode,
		       BOOL showProgress) {
  _ObserveQuery* newQ = (_ObserveQuery*) calloc(1,sizeof(_ObserveQuery));
  newQ->handle = handle;
  newQ->htable = newHashTable(80000);
  newQ->identifierNode = identifierNode;
  newQ->topIdentifierNode = topIdentifierNode;
  newQ->recursiveMode = recursiveMode;
  newQ->showProgress = showProgress;
  newQ->currentOffset = identifierNode;
  if (topIdentifierNode>identifierNode) newQ->currentOffset = topIdentifierNode;
  newQ->fsz = hatFileSize(handle)/1000;
  if (newQ->fsz==0) newQ->fsz = 1;
  //if (newQ->fsz<20000) newQ->lsz=200;
  return ((ObserveQuery) newQ);
}

void freeObserveQuery(ObserveQuery query) {
  freeHashTable(((_ObserveQuery*) query)->htable);
  ((_ObserveQuery*) query)->htable = NULL;
  free(query);
}

filepointer observeIdentifier(ObserveQuery query) {
  return ((_ObserveQuery*) query)->identifierNode;
}

filepointer observeTopIdentifier(ObserveQuery query) {
  return ((_ObserveQuery*) query)->topIdentifierNode;
}

unsigned long observedNodes(ObserveQuery query) {
  return ((_ObserveQuery*) query)->found;
}

HatFile observeHatFile(ObserveQuery query) {
  return ((_ObserveQuery*) query)->handle;
}

filepointer nextObserveQueryNode(ObserveQuery query) {
  unsigned long p,currentOffset;
  char nodeType;
  int arity;
  HashTable* htable = ((_ObserveQuery*) query)->htable;
  unsigned long identifierNode = ((_ObserveQuery*) query)->identifierNode;
  unsigned long topIdentifierNode = ((_ObserveQuery*) query)-> topIdentifierNode;
  int recursiveMode = ((_ObserveQuery*) query)->recursiveMode;
  int handle = ((_ObserveQuery*) query)->handle;

  currentOffset = hatSeqNext(handle,((_ObserveQuery*) query)->currentOffset);

  while (!hatSeqEOF(handle,currentOffset)) {

    if ((((_ObserveQuery*) query)->showProgress)&&
	((currentOffset/10)/((_ObserveQuery*) query)->fsz > 
	 ((_ObserveQuery*) query)->lsz)) {
      ((_ObserveQuery*) query)->lsz = (currentOffset/10)/((_ObserveQuery*) query)->fsz;
      fprintf(stderr,"\b\b\b\b%3u%%",((_ObserveQuery*) query)->lsz);
      fflush(stderr);
    }

    nodeType = getNodeType(handle,currentOffset);
    switch (nodeType) {
    case TRAPP:
      {
	int arity = getAppArity();
	filepointer apptrace;
	apptrace = getParent();  // fileoffset of App-trace
	p = getAppFun();         // fileoffset of Function-trace
	if (isInHashTable(htable,p)) {
	  filepointer satc = getResult(handle,currentOffset);  // find SATC for the application!	  
	  if (isSAT(satc)) {
	    if (hatFollowSATs(handle,satc)==currentOffset) {
	      addToHashTable(htable,currentOffset); // remember partial application
	      addToHashTable(htable,satc);
	    } else {
	      addToHashTable(htable,currentOffset);
	      if (((recursiveMode==0)||(isDescendantOf(handle,
						       apptrace,identifierNode)==0))&&
		  ((topIdentifierNode==0)||
		   (isDirectDescendantOf(handle,apptrace,topIdentifierNode)))) {
		((_ObserveQuery*) query)->currentOffset = currentOffset;
		((_ObserveQuery*) query)->found++;
		return currentOffset;
	      }
	    }
	  }
	}
      }
      currentOffset = hatSeqNext(handle,currentOffset);
      break;
    case TRNAM: // Name
      if ((getNameType()==identifierNode)&&(identifierNode!=0)) {
	filepointer satc;
	//printf("found name reference for identifier at: %u\n",p);
	addToHashTable(htable,currentOffset);
	satc = hatSeqNext(handle,currentOffset);
	if (isSAT(satc)) {  // SATC behind TRNAM?
	  // found a CAF!
	  if (hatFollowSATs(handle,satc)==currentOffset) { // save this satc for future reference
	    addToHashTable(htable,satc);
	  } else {  // makes no sense to print equation of form "identifier = identifier"
	    ((_ObserveQuery*) query)->currentOffset = currentOffset;
	    ((_ObserveQuery*) query)->found++;
	    return currentOffset;
	  }
	  currentOffset = hatSeqNext(handle,satc); // skip the satc node
	} else currentOffset = satc;
      } else currentOffset = hatSeqNext(handle,currentOffset);
      break;
    default:
      currentOffset = hatSeqNext(handle,currentOffset);
    }
  }
  ((_ObserveQuery*) query)->finished = 1;
  if ((((_ObserveQuery*) query)->showProgress)&&(((_ObserveQuery*) query)->lsz<200)) {
    fprintf(stderr,"\b\b\b\b");
    fflush(stderr);
  }
  return 0;
}

void findNodes(HatFile handle,
	       char* identifier,
	       char* topIdentifier,
	       unsigned long *identNode,
	       unsigned long *topIdentNode,
	       BOOL showProgress) {
  unsigned long identifierNode = 0,p,currentOffset,fsz,lsz=0;
  unsigned long topIdentifierNode = 0;
  char nodeType;
  fsz = hatFileSize(handle)/1000;
  if (fsz<20000) lsz=200;
  else if (showProgress) {
    fprintf(stderr,"  0%%");
    fflush(stderr); // force printing to screen, even though LF is still missing...
  }
  if (fsz==0) fsz=1;
  currentOffset = hatSeqFirst(handle);
  while ((!hatSeqEOF(handle,currentOffset))&&
	 ((identifierNode==0)||
	  ((topIdentifierNode==0)&&(topIdentifier!=NULL)))) {
    if ((showProgress)&&((currentOffset/10)/fsz>lsz)) {
      lsz = (currentOffset/10)/fsz;
      fprintf(stderr,"\b\b\b\b%3u%%",lsz);
      fflush(stderr);
    }
    nodeType = getNodeType(handle,currentOffset);
    if (nodeType==NTIDENTIFIER) {
      if ((identifierNode==0)||((topIdentifier!=NULL)&&(topIdentifierNode==0))) {
	if (isTopLevel(handle,currentOffset)) {
	  // search for identifier
	  char *currentIdent = getName(); // name of current identifier
	  //printf("identifier '%s'\n",currentIdent);
	  if ((identifierNode==0)&&(strcmp(currentIdent,identifier)==0)) { // found?
	    //printf("FOUND Identifier at: %u!\n",currentOffset);
	    identifierNode = currentOffset;
	    if ((topIdentifier!=NULL)&&(strcmp(identifier,topIdentifier)==0))
	      topIdentifierNode=currentOffset; // both identifier are the same!
	  } else
	    if ((topIdentifier!=NULL)&&(strcmp(currentIdent,topIdentifier)==0))
	      topIdentifierNode=currentOffset;
	}
      }
    }
    currentOffset = hatSeqNext(handle,currentOffset);
  }
  *identNode=identifierNode;
  *topIdentNode=topIdentifierNode;
}

ObserveQuery newObserveQueryIdent(int handle,char* ident,char* topIdent,
				  BOOL recursiveMode,BOOL showProgress) {
  unsigned long identifierNode=0,topIdentifierNode=0;
  
  findNodes(handle,ident,topIdent,&identifierNode,&topIdentifierNode,showProgress);
  return newObserveQuery(handle,identifierNode,topIdentifierNode,
			 recursiveMode,showProgress);
}

// get table of all unique observations
FunTable observeUnique(ObserveQuery query,BOOL verboseMode,int precision) {
  FunTable results = newFunTable();
  filepointer currentOffset;
  int arity,maxarity = -1;
  HatFile handle = ((_ObserveQuery*) query)->handle;

  currentOffset = nextObserveQueryNode(query);
  while (!((_ObserveQuery*) query)->finished) {
    unsigned long satc = getResult(handle,currentOffset);
    ExprNode* r=buildExpr(handle,satc,verboseMode,precision);
    ExprNode* a=buildExpr(handle,currentOffset,verboseMode,precision);
    arity = getExprArity(a);
    if (arity>=maxarity) {
      addToFunTable(results,a,r,currentOffset);
      maxarity=arity;
    } else {
      freeExpr(r);
      freeExpr(a);
    }
    currentOffset = nextObserveQueryNode(query);
  }
  checkArities(results); // remove partial applications with missing arguments
  return results;
}
