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

// observe an identifier
#define IDENTMODE      0
// observe values at a source position
#define SRCMODE        1
// observe all observable identifiers/modules
#define OBSERVABLEMODE 2

// initial size of hash table and size by which it may be increased stepwise
#define HASHTABLEINCSIZE 80000

typedef struct {
  HashTable* htable;
  filepointer identifierNode;
  filepointer topIdentifierNode;
  filepointer currentOffset;
  unsigned long line,column;
  filepointer moduleRef;
  int recursiveMode;
  unsigned long fsz,lsz;
  int searchMode;
  int found;
  int finished;
  int showProgress;
  int handle;
} _ObserveQuery;



int isDescendantOf(HatFile handle,filepointer fileoffset,filepointer parent) {
  char nodeType;
  filepointer old = hatNodeNumber(handle);

  if (parent==0) return 0; 

  if (getNodeType(handle,fileoffset)==HatApplication) {
    fileoffset = hatOutermostName(handle,fileoffset);
    if (fileoffset!=0) {
      getNodeType(handle,fileoffset);
      fileoffset=getParent();
    }
  }
  if (fileoffset==parent) return 1;

  while (fileoffset!=0) {
    nodeType=getNodeType(handle,fileoffset);
    switch(nodeType) {
    case HatHidden:
    case HatProjection:
      fileoffset=getParent();
      break;
    case HatSATC:
      fileoffset=getProjValue();
      break;
    case HatName:
      if (getNameType()==parent) {
	hatSeekNode(handle,old);
	return 1;
      }
      fileoffset = getParent();
      break;
    case HatApplication:{
      filepointer newoffs;
      newoffs = getAppFun(); //Parent();
      if (hatOutermostSymbol(handle,fileoffset)==parent) {
	hatSeekNode(handle,old);
	return 1;
      }
      getNodeType(handle,newoffs);
      fileoffset = getParent();
      break;
    }
    default:
      hatSeekNode(handle,old);
      return 0;
    }
  }
  hatSeekNode(handle,old);
  return 0;
}

int isDirectDescendantOf(HatFile handle,filepointer fileoffset,filepointer parent) {
  char nodeType;
  filepointer old = hatNodeNumber(handle);
  int debug=0;

  if (parent==0) return 0;
  
  if (getNodeType(handle,fileoffset)==HatApplication) {
    fileoffset = hatOutermostName(handle,fileoffset);
    if (fileoffset!=0) {
      getNodeType(handle,fileoffset);
      fileoffset=getParent();
    }
  }
  if (fileoffset==parent) return 1;

  while (fileoffset!=0) {
    nodeType=getNodeType(handle,fileoffset);
    switch(nodeType) {
    case HatHidden:
      fileoffset=getParent();
      break;
    case HatSATA:
    case HatSATB:
    case HatSATC:
      fileoffset=getProjValue();
      break;
    case HatProjection:
      fileoffset=getParent();
      break;
    case HatName:
      if (getNameType()==parent) {
	hatSeekNode(handle,old);
	return 1;
      }
      fileoffset = getParent();
      break;
    case HatApplication:{
      filepointer newoffs;
      newoffs = getAppFun();
      if (hatOutermostSymbol(handle,fileoffset)==parent) {
	hatSeekNode(handle,old);
	return 1;
      }
      if (isTopLevel(handle,fileoffset)) {
	hatSeekNode(handle,old);
	return 0;
      }
      getNodeType(handle,newoffs);
      fileoffset = getParent();
      break;
    }
    default:
      hatSeekNode(handle,old);
      return 0;
    }
  }
  hatSeekNode(handle,old);
  return 0;
}


// new query to observe all applications of identifierNode within the trace file
ObserveQuery newObserveQuery(int handle,
		       filepointer identifierNode,
		       filepointer topIdentifierNode,
		       BOOL recursiveMode,
		       BOOL showProgress) {
  _ObserveQuery* newQ = (_ObserveQuery*) calloc(1,sizeof(_ObserveQuery));
  newQ->handle = handle;
  newQ->htable = newHashTable(HASHTABLEINCSIZE);
  addToHashTable(newQ->htable,identifierNode);
  newQ->identifierNode = identifierNode;
  newQ->topIdentifierNode = topIdentifierNode;
  newQ->recursiveMode = recursiveMode;
  newQ->showProgress = showProgress;
  newQ->currentOffset = hatSeqFirst(handle);// identifierNode;
  newQ->searchMode = IDENTMODE;
  //if (topIdentifierNode>identifierNode) newQ->currentOffset = topIdentifierNode;
  newQ->fsz = hatFileSize(handle)/1000;
  if (newQ->fsz==0) newQ->fsz = 1;
  //if (newQ->fsz<20000) newQ->lsz=200;
  //printf("new query: %u\n",identifierNode);
  return ((ObserveQuery) newQ);
}

// new query to observe all observable identifiers/modules in the trace file
ObserveQuery newObservableQuery(int handle,BOOL showProgress) {
  _ObserveQuery* newQ = (_ObserveQuery*) calloc(1,sizeof(_ObserveQuery));
  newQ->handle = handle;
  newQ->htable = NULL;
  newQ->showProgress = showProgress;
  newQ->currentOffset = hatSeqFirst(handle);
  newQ->searchMode = OBSERVABLEMODE;
  newQ->fsz = hatFileSize(handle)/1000;
  if (newQ->fsz==0) newQ->fsz = 1;
  return ((ObserveQuery) newQ);
}

filepointer hatfindModule(HatFile handle,
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
  newQ->htable = newHashTable(HASHTABLEINCSIZE);
  newQ->searchMode = SRCMODE;
  newQ->line = line;
  newQ->column = column;
  newQ->showProgress = showProgress;
  if ((moduleName==NULL)||(strcmp(moduleName,"")==0)) {
    filepointer p = hatMainCAF(handle);
    if (p!=0) {
      getNodeType(handle,p); // go to main CAF
      p = getNameType();
      if (getNodeType(handle,p)==HatIdentifier) {
	newQ->moduleRef = getModInfo();
      }
    }
  } else {
    newQ->moduleRef = hatfindModule(handle,moduleName,showProgress);
  }
  newQ->currentOffset = hatSeqFirst(handle); //newQ->moduleRef;
  newQ->fsz = hatFileSize(handle)/1000;
  if (newQ->fsz==0) newQ->fsz = 1;
  return ((ObserveQuery) newQ);
}

void freeObserveQuery(ObserveQuery query) {
  if (((_ObserveQuery*) query)->htable) // OBSERVALE queries have no hash table
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

void queryCleanProgress(ObserveQuery query) {
  if (((_ObserveQuery*) query)->showProgress) {
    fprintf(stderr,"\b\b\b\b    \b\b\b\b");
    //fflush(stderr);
  }
}

void updateProgress(_ObserveQuery* query,filepointer currentOffset) {
  if ((query->showProgress)&&
      ((currentOffset/10)/((_ObserveQuery*) query)->fsz > 
       ((_ObserveQuery*) query)->lsz)) {
    ((_ObserveQuery*) query)->lsz = (currentOffset/10)/((_ObserveQuery*) query)->fsz;
    fprintf(stderr,"\b\b\b\b%3u%%",((_ObserveQuery*) query)->lsz);
    fflush(stderr);
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
    updateProgress(query,currentOffset);

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
	filepointer src = getSrcRef();
	if (isInHashTable(htable,src)) {
	  filepointer next=hatSeqNext(handle,currentOffset);
	  if ((next!=0)&&(getNodeType(handle,next)==HatApplication)&&
	      (getAppFun()==currentOffset)&&(getSrcRef()==src)) {
	    // ok, this is an name/application being a part of the following application
	    // do not include this name/application, but the whole structure
	    // do nothing now, wait for next application
	  } else {
	    filepointer fun = hatOutermostSymbol(handle,currentOffset);
	    char lmoType = 0;
	    if (fun!=0) lmoType = getNodeType(handle,fun);
	    if (lmoType==HatIdentifier) {
	      filepointer result = hatResult(handle,currentOffset);
	      if ((result!=0)&&(isSAT(handle,result))&&
		  (getNodeType(handle,result)!=HatSATA)) { // make sure, result is available!
		result = hatFollowSATCs(handle,result);
		if (result!=currentOffset) { // no partial applications!
		  if (result!=0) {
		    query->currentOffset = currentOffset;
		    query->found++;
		    queryCleanProgress((ObserveQuery)query);
		    return currentOffset; //result;
		  }
		}
	      }
	    } else
	      if (lmoType==HatConstructor) {
		query->currentOffset = currentOffset;
		query->found++;
		queryCleanProgress((ObserveQuery)query);
		return currentOffset;
	      }
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
  queryCleanProgress((ObserveQuery)query);
  return 0;
}

// find all observable identifiers and all Modules within the TraceFile
filepointer nextObservable(_ObserveQuery* query) {
  unsigned long p,currentOffset;
  char nodeType;
  int handle = query->handle;

  currentOffset = hatSeqNext(handle,query->currentOffset);
  if (query->showProgress) fprintf(stderr,"\b\b\b\b%3u%%",query->lsz);
  while (!hatSeqEOF(handle,currentOffset)) {
    updateProgress(query,currentOffset);
    nodeType = getNodeType(handle,currentOffset);
    switch (nodeType) {
    case HatIdentifier:
      if (isTopLevel(handle,currentOffset)) {
	query->currentOffset = currentOffset;
	query->found++;
	queryCleanProgress((ObserveQuery)query);
	return currentOffset;
      }
      break;
    case HatModule:
      query->currentOffset = currentOffset;
      query->found++;
      queryCleanProgress((ObserveQuery)query);
      return currentOffset;
    }
    currentOffset = hatSeqNext(handle,currentOffset);
  }
  ((_ObserveQuery*) query)->finished = 1;
  queryCleanProgress((ObserveQuery)query);
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

  switch (((_ObserveQuery*) query)->searchMode) {
  case SRCMODE: // use function to find next value at source reference
    return nextObserveSrcMode((_ObserveQuery*) query);
  case OBSERVABLEMODE: // use function to find next observable identifier/module
    return nextObservable((_ObserveQuery*) query);
  }

  currentOffset = hatSeqNext(handle,((_ObserveQuery*) query)->currentOffset);
  if (((_ObserveQuery*) query)->showProgress) {
    fprintf(stderr,"\b\b\b\b%3u%%",((_ObserveQuery*) query)->lsz);
  }
  while (!hatSeqEOF(handle,currentOffset)) {
    updateProgress((_ObserveQuery*)query,currentOffset);

    nodeType = getNodeType(handle,currentOffset);
    switch (nodeType) {
    case HatApplication:
      {
	filepointer apptrace;
	apptrace = getParent();  // fileoffset of App-trace
	p = getAppFun();
	//bufferMiss = 0;
	p = hatFollowSATCs(handle,p);         // fileoffset of Function-trace
	//if (bufferMiss>0) bufferMisses++;else bufferHits++;
	if (p>currentOffset) { // whoo! The function which is applied here is forward
	  // in the file! We haven't been there yet: so we need to check it!
	  p = hatOutermostSymbol(handle,p); // follow to the leftmost outermost identifier
	}
	if (isInHashTable(htable,p)) {
	  filepointer satc = hatResult(handle,currentOffset);  // find SATC for the application!	  
	  addToHashTable(htable,currentOffset);
	  if (isSAT(handle,satc)) {
	    if ((hatFollowSATCs(handle,satc)!=currentOffset)&&
		(getNodeType(handle,satc)!=SATA)) { // rhs evaluated!
	      if (getParent() != currentOffset) {
		if (((recursiveMode==0)||(isDescendantOf(handle,
							 apptrace,identifierNode)==0))&&
		    ((topIdentifierNode==0)||
		     (isDirectDescendantOf(handle,currentOffset,topIdentifierNode)))) {
		  ((_ObserveQuery*) query)->currentOffset = currentOffset;
		  ((_ObserveQuery*) query)->found++;
		  queryCleanProgress(query);
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
	  if (hatFollowSATCs(handle,satc)==currentOffset) { // save this satc for future reference
	    addToHashTable(htable,satc);
	  } else {  // makes no sense to print equation of form "identifier = identifier"
	    ((_ObserveQuery*) query)->currentOffset = currentOffset;
	    ((_ObserveQuery*) query)->found++;
	    queryCleanProgress(query);
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
  queryCleanProgress(query);
  //fprintf(stderr,"bufferMisses: %u, bufferHits: %u\n",bufferMisses,bufferHits);
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
    unsigned long satc = hatResult(handle,currentOffset);
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
