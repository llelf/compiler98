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
#include <stdlib.h>
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
  filepointer identifierNode;
  filepointer topIdentifierNode;
  filepointer currentOffset;
  unsigned long line,column;
  filepointer moduleRef;
  int recursiveMode;
  unsigned long fsz,lsz;
  int srcSearchMode;
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
  //printf("new query: %u\n",identifierNode);
  return ((ObserveQuery) newQ);
}

filepointer findModule(HatFile handle,
		       char* moduleName,
		       BOOL showProgress) {
  unsigned long currentOffset,fsz,lsz=0;
  char nodeType;
  filepointer moduleRef=0;
  fsz = hatFileSize(handle)/1000;
  if (fsz<20000) lsz=200;
  else if (showProgress) {
    fprintf(stderr,"  0%%");
    fflush(stderr); // force printing to screen, even though LF is still missing...
  }
  if (fsz==0) fsz=1;
  currentOffset = hatSeqFirst(handle);
  while ((!hatSeqEOF(handle,currentOffset))&&(moduleRef==0)) {
    if ((showProgress)&&((currentOffset/10)/fsz>lsz)) {
      lsz = (currentOffset/10)/fsz;
      fprintf(stderr,"\b\b\b\b%3u%%",lsz);
      fflush(stderr);
    }
    nodeType = getNodeType(handle,currentOffset);
    if (nodeType==HatModule) {
      char* name = getName();
      if ((name!=NULL)&&(strcmp(name,moduleName)==0)) moduleRef=currentOffset;
      else 
	currentOffset = hatSeqNext(handle,currentOffset);
    } else
      currentOffset = hatSeqNext(handle,currentOffset);
  }
  if (showProgress) fprintf(stderr,"\b\b\b\b");
  return moduleRef;
}

ObserveQuery newObserveQuerySource(int handle,
				   char* moduleName,
				   unsigned long line,unsigned long column,
				   BOOL showProgress) {
  _ObserveQuery* newQ = (_ObserveQuery*) calloc(1,sizeof(_ObserveQuery));
  newQ->handle = handle;
  newQ->htable = newHashTable(80000);
  newQ->srcSearchMode = 1;
  newQ->line = line;
  newQ->column = column;
  newQ->showProgress = showProgress;
  if (moduleName==NULL) {
    filepointer p = hatMainCAF(handle);
    if (p!=0) {
      getNodeType(handle,p); // go to main CAF
      p = getNameType();
      if (getNodeType(handle,p)==HatIdentifier) {
	newQ->moduleRef = getModInfo();
      }
    }
  } else {
    newQ->moduleRef = findModule(handle,moduleName,showProgress);
  }
  newQ->currentOffset = newQ->moduleRef;
  newQ->fsz = hatFileSize(handle)/1000;
  if (newQ->fsz==0) newQ->fsz = 1;
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

void queryCleanOutput(ObserveQuery query) {
  if (((_ObserveQuery*) query)->showProgress) {
    fprintf(stderr,"\b\b\b\b");
    //fflush(stderr);
  }
}


filepointer nextObserveSrcMode(_ObserveQuery* query) {
  unsigned long p,currentOffset;
  char nodeType;
  HashTable* htable = query->htable;
  filepointer moduleRef = query->moduleRef;
  unsigned long column = query->column,line = query->line;
  int handle = query->handle;

  currentOffset = hatSeqNext(handle,query->currentOffset);
  if (query->showProgress) {
    fprintf(stderr,"\b\b\b\b%3u%%",query->lsz);
  }
  while (!hatSeqEOF(handle,currentOffset)) {

    if ((query->showProgress)&&
	((currentOffset/10)/((_ObserveQuery*) query)->fsz > 
	 ((_ObserveQuery*) query)->lsz)) {
      ((_ObserveQuery*) query)->lsz = (currentOffset/10)/((_ObserveQuery*) query)->fsz;
      fprintf(stderr,"\b\b\b\b%3u%%",((_ObserveQuery*) query)->lsz);
      fflush(stderr);
    }

    nodeType = getNodeType(handle,currentOffset);
    switch (nodeType) {
    case HatSrcRef:
      if ((getModInfo()==moduleRef)&&(getPosnColumn()==column)&&(getPosnRow()==line))
	addToHashTable(htable,currentOffset);
      currentOffset = hatSeqNext(handle,currentOffset);
      break;
    case HatApplication:
    case HatName:
      {
	filepointer apptrace;
	if (isInHashTable(htable,getSrcRef())) {
	  filepointer fun = hatLMO(handle,currentOffset);
	  char lmoType = 0;
	  if (fun!=0) lmoType = getNodeType(handle,fun);
	  if (lmoType==HatIdentifier) {
	    filepointer result = getResult(handle,currentOffset);
	    if ((result!=0)&&(isSAT(handle,result))&&
		(getNodeType(handle,result)!=HatSATA)) { // make sure, result is available!
	      result = hatFollowSATs(handle,result);
	      if (result!=currentOffset) { // no partial applications!
		if (result!=0) {
		  query->currentOffset = currentOffset;
		  query->found++;
		  queryCleanOutput(query);
		  return currentOffset; //result;
		}
	      }
	    }
	  } else
	    if (lmoType==HatConstructor) {
	      query->currentOffset = currentOffset;
	      query->found++;
	      queryCleanOutput(query);
	      return currentOffset;
	    }
	}
      }
      currentOffset = hatSeqNext(handle,currentOffset);
      break;
    default:
      currentOffset = hatSeqNext(handle,currentOffset);
    }
  }
  ((_ObserveQuery*) query)->finished = 1;
  queryCleanOutput(query);
  return 0;
}

/*
 extern int bufferMiss;
 int bufferMisses = 0;
 int bufferHits = 0;
*/

filepointer nextObserveQueryNode(ObserveQuery query) {
  unsigned long p,currentOffset;
  char nodeType;
  HashTable* htable = ((_ObserveQuery*) query)->htable;
  unsigned long identifierNode = ((_ObserveQuery*) query)->identifierNode;
  unsigned long topIdentifierNode = ((_ObserveQuery*) query)-> topIdentifierNode;
  int recursiveMode = ((_ObserveQuery*) query)->recursiveMode;
  int handle = ((_ObserveQuery*) query)->handle;

  if (((_ObserveQuery*) query)->srcSearchMode) {
    return nextObserveSrcMode((_ObserveQuery*) query);
  }

  currentOffset = hatSeqNext(handle,((_ObserveQuery*) query)->currentOffset);
  if (((_ObserveQuery*) query)->showProgress) {
    fprintf(stderr,"\b\b\b\b%3u%%",((_ObserveQuery*) query)->lsz);
  }
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
    case HatApplication:
      {
	filepointer apptrace;
	apptrace = getParent();  // fileoffset of App-trace
	//bufferMiss = 0;
	p = hatFollowSATs(handle,getAppFun());         // fileoffset of Function-trace
	//if (bufferMiss>0) bufferMisses++;else bufferHits++;
	if (isInHashTable(htable,p)) {
	  filepointer satc = getResult(handle,currentOffset);  // find SATC for the application!	  
	  addToHashTable(htable,currentOffset);
	  if (isSAT(handle,satc)) {
	    if ((hatFollowSATs(handle,satc)!=currentOffset)&&
		(getNodeType(handle,satc)!=SATA)) { // rhs evaluated!
	      if (getParent() != currentOffset) {
		if (((recursiveMode==0)||(isDescendantOf(handle,
							 apptrace,identifierNode)==0))&&
		    ((topIdentifierNode==0)||
		     (isDirectDescendantOf(handle,currentOffset,topIdentifierNode)))) {
		  ((_ObserveQuery*) query)->currentOffset = currentOffset;
		  ((_ObserveQuery*) query)->found++;
		  queryCleanOutput(query);
		  return currentOffset;
		}
	      }
	    }
	  }
	}
      }
      currentOffset = hatSeqNext(handle,currentOffset);
      break;
    case HatName: // Name
      if ((getNameType()==identifierNode)&&(identifierNode!=0)) {
	filepointer satc;
	//printf("found name reference for identifier at: %u\n",p);
	addToHashTable(htable,currentOffset);
	satc = hatSeqNext(handle,currentOffset);
	if (isSAT(handle,satc)) {  // SATC behind TRNAM?
	  // found a CAF!
	  if (hatFollowSATs(handle,satc)==currentOffset) { // save this satc for future reference
	    addToHashTable(htable,satc);
	  } else {  // makes no sense to print equation of form "identifier = identifier"
	    ((_ObserveQuery*) query)->currentOffset = currentOffset;
	    ((_ObserveQuery*) query)->found++;
	    queryCleanOutput(query);
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
  queryCleanOutput(query);
  return 0;
}

void findNodes(HatFile handle,
	       char* identifier,
	       char* topIdentifier,
	       unsigned long *identNode,
	       unsigned long *topIdentNode,
	       BOOL showProgress) {
  unsigned long identifierNode = 0,currentOffset,fsz,lsz=0;
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
  if ((topIdentifier!=NULL)&&(strcmp(topIdentifier,"")==0)) topIdentifier=NULL;
  while ((!hatSeqEOF(handle,currentOffset))&&
	 ((identifierNode==0)||
	  ((topIdentifierNode==0)&&(topIdentifier!=NULL)))) {
    if ((showProgress)&&((currentOffset/10)/fsz>lsz)) {
      lsz = (currentOffset/10)/fsz;
      fprintf(stderr,"\b\b\b\b%3u%%",lsz);
      fflush(stderr);
    }
    nodeType = getNodeType(handle,currentOffset);
    if (nodeType==HatIdentifier) {
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
  if (showProgress) fprintf(stderr,"\b\b\b\b");
}

ObserveQuery newObserveQueryIdent(int handle,char* ident,char* topIdent,
				  BOOL recursiveMode,BOOL showProgress) {
  unsigned long identifierNode=0,topIdentifierNode=0;
  ObserveQuery query;
  
  if ((topIdent!=NULL)&&(strcmp(topIdent,"")==0)) topIdent=NULL;
  findNodes(handle,ident,topIdent,&identifierNode,&topIdentifierNode,showProgress);
  query = newObserveQuery(handle,identifierNode,topIdentifierNode,
			  recursiveMode,showProgress);
  if ((topIdentifierNode==0)&&(topIdent!=NULL))
    ((_ObserveQuery*) query)->currentOffset=hatFileSize(handle);
  return query;  
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
  FunTableCheckArities(results); // remove partial applications with missing arguments
  return results;
}
