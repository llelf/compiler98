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
#include "hatfileops.h"
#include "hashtable.h"
#include "menu.h"

// optional switch


//#define showNodeInfo // show additional information about file offsets


void checkNodes(unsigned long identifierNode,
		unsigned long topIdentifierNode,
		FunTable* result,int verboseMode,int uniqueMode,
		int recursiveMode,unsigned int precision) {
  unsigned long p,progress=0,currentOffset,fsz,lsz=0;
  char nodeType;
  HashTable* htable = newHashTable(80000);
  int arityProblem=0,arity,maxarity = -1;
  int found = 0;
  fsz = filesize()/1000;
  if (fsz<20000) lsz=200;
  /* else if (uniqueMode) {
    fprintf(stderr,"  0%%");
    fflush(stderr); // force printing to screen, even though LF is still missing...
    }*/
  if (fsz==0) fsz=1;
  while (more()) {
    currentOffset = byteoffset();
    if ((uniqueMode)&&((currentOffset/10)/fsz>lsz)) {
      lsz = (currentOffset/10)/fsz;
      fprintf(stderr,"\b\b\b\b%3u%%",lsz);
      fflush(stderr);
    }
    nodeType = getNodeType();
    switch (nodeType) {
    case TRAPP:
      {
	int arity = getAppArity();
	unsigned long apptrace;
	apptrace = getTrace();  // fileoffset of App-trace
	p = getFunTrace();      // fileoffset of Function-trace
	if (isInHashTable(htable,p)) {
	  unsigned long old  = byteoffset();
	  unsigned long satc = findAppSAT(currentOffset);  // find SATC for the application!	  
	  if (isSAT(satc)) {
	    if (followSATs(satc)==currentOffset) {
	      addToHashTable(htable,currentOffset); // remember partial application
	      addToHashTable(htable,satc);
	    } else
	      if (uniqueMode) {
		addToHashTable(htable,currentOffset);
		if (((recursiveMode==0)||(isDescendantOf(apptrace,identifierNode)==0))&&
		    ((topIdentifierNode==0)||
		     (isDirectDescendantOf(apptrace,topIdentifierNode)))) {
		  ExprNode* r=buildExpr(satc,verboseMode,precision);
		  ExprNode* a=buildExpr(currentOffset,verboseMode,precision);
		  arity = getExprArity(a);
		  if (arity>=maxarity) {
		    addToFunTable(result,a,r,currentOffset);
		    maxarity=arity;
		  } else {
		    freeExpr(r);
		    freeExpr(a);
		  }
		}
	      } else {
		addToHashTable(htable,currentOffset);
		if (((recursiveMode==0)||(isDescendantOf(apptrace,identifierNode)==0))&&
		    ((topIdentifierNode==0)||
		     (isDirectDescendantOf(apptrace,topIdentifierNode)))) {
		  ExprNode* a=buildExpr(currentOffset,verboseMode,precision);
		  arity = getExprArity(a);
		  if (arity>=maxarity) {
		    if ((arity>maxarity)&&(maxarity!=-1)) {
		      arityProblem=found;
		      printf("Partial applications detected. Ignore all lines above!\n");
		      printf("------------------------------------------------------\n");
		    }
		    maxarity = arity;
#ifdef showNodeInfo
		    printf("(%u) ",currentOffset);
#endif
		    showAppAndResult(currentOffset,verboseMode,precision);
		    found++;
		  }
		  freeExpr(a);
		}
	      }
	  }
	  seek(old);
	}
      }
      nextNode();
      break;
    case TRNAM: // Name
      if ((getNmType()==identifierNode)&&(identifierNode!=0)) {
	//printf("found name reference for identifier at: %u\n",p);
	addToHashTable(htable,currentOffset);
	nextNode();
	if (isSAT(byteoffset())) {  // SATC behind TRNAM?
	  // found a CAF!
	  unsigned long satc = byteoffset();
	  if (followSATs(satc)==currentOffset) { // save this satc for future reference
	    addToHashTable(htable,satc);
	  } else {  // makes no sense to print equation of form "identifier = identifier"
	    if (uniqueMode) {
	      ExprNode* r=buildExpr(satc,verboseMode,precision);
	      ExprNode* a=buildExpr(currentOffset,verboseMode,precision);
	      addToFunTable(result,a,r,currentOffset);
	    } else { // print CAF and its value
	      showAppAndResult(currentOffset,verboseMode,precision);
	    }
	  }
	  seek(satc); // go to satc
	  nextNode(); // skip this node
	}
      } else nextNode();
      break;
    default:
      nextNode();
    }
  }
  if ((uniqueMode)&&(lsz<200)) {
    fprintf(stderr,"\b\b\b\b");
    fflush(stderr);
  }
  if (arityProblem>0) {
    printf("\nAttention: Due to partial applications the first %i line(s)\n",arityProblem);
    printf("are missing arguments - please ignore them!\n");
    printf("Use the -u option to see the correct applications only.\n\n");
  }
}

void findNodes(char* identifier,
	       char* topIdentifier,
	       unsigned long *identNode,
	       unsigned long *topIdentNode,
	       int uniqueMode) {
  unsigned long identifierNode = 0,p,progress=0,currentOffset,fsz,lsz=0;
  unsigned long topIdentifierNode = 0;
  char nodeType;
  fsz = filesize()/1000;
  if (fsz<20000) lsz=200;
  else if (uniqueMode) {
    fprintf(stderr,"  0%%");
    fflush(stderr); // force printing to screen, even though LF is still missing...
  }
  if (fsz==0) fsz=1;
  seek(0);
  while ((more())&&(identifierNode==0)&&((topIdentifierNode==0)||
					 (topIdentifier==NULL))) {
    currentOffset = byteoffset();
    if ((uniqueMode)&&((currentOffset/10)/fsz>lsz)) {
      lsz = (currentOffset/10)/fsz;
      fprintf(stderr,"\b\b\b\b%3u%%",lsz);
      fflush(stderr);
    }
    nodeType = getNodeType();
    if (nodeType==NTIDENTIFIER) {
      if ((identifierNode==0)||((topIdentifier!=NULL)&&(topIdentifierNode==0))) {
	if (isTopLevel(currentOffset)) {
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
    nextNode();
  }
  *identNode=identifierNode;
  *topIdentNode=topIdentifierNode;
}

unsigned long observeNode(unsigned long identifierNode,unsigned long topIdentifierNode,
			  int verbosemode,int uniqueMode,int recursivemode,
			  unsigned int precision,FunTable* results) {
  unsigned long result=0;

  checkNodes(identifierNode,topIdentifierNode,
	     results,verbosemode,uniqueMode,recursivemode,precision);
  if (uniqueMode) { 
    checkArities(results); // remove partial applications with missing arguments
  }
}

void observeIdentifier(char* ident,char* topIdent,
		       int verbosemode,int uniqueMode,int recursivemode,
		       unsigned int precision,FunTable* results) {
  unsigned long identifierNode=0,topIdentifierNode=0;
  
  findNodes(ident,topIdent,&identifierNode,&topIdentifierNode,uniqueMode);
  observeNode(identifierNode,topIdentifierNode,verbosemode,uniqueMode,recursivemode,
	      precision,results);
}


int getObserve(char* ident,char* topIdent,
	       int verbosemode,int uniqueMode,int recursivemode,
	       unsigned int precision,int **childrenArray) {
  FunTable* results=newFunTable();
  int l;

  observeIdentifier(ident,topIdent,verbosemode,uniqueMode,recursivemode,
		    precision,results);
  l = FunTableLength(results);
  //printf("New Observe: %i for %s\n",l,ident);
  {
    int i=0;
    FunTable *e = results->next;
    *childrenArray = (int*) calloc(l+1,sizeof(long));
    while (i<l) {
      (*childrenArray)[i]=e->fileoffset;
      i++;
      e=e->next;
    }
    (*childrenArray)[i]=-1;
    //printf("observed %i\n",i);
  }
  freeFunTable(results);
  return l;
}
