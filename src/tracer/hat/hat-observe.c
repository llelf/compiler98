/**************************************************************************/
/* hat-observe: searches a hat redex file for all applications of a given */
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

void checkNodes(char* identifier,char* topIdentifier,
		FunTable* result,int verboseMode,int uniqueMode,
		int recursiveMode);

// optional switch
//#define showNodeInfo // show additional information about file offsets

main (int argc, char *argv[])
{ int verbosemode=0,uniquemode=0,recursivemode=0;
  int c = 1,err=0,paramerr=0;
  char *fname=NULL,*ident=NULL,*topIdent=NULL;
  while (c<argc) {
    err = checkParameters(argv[c],"vur"); // check for supported parameters
    if (err==1) { // not a parameter!
      err=0;
      break;
    }
    if (err==2) {
      fprintf(stderr,"\nUnsupported option! See below for possible flags!\n");
      break;
    }
    if (strchr(argv[c],'v')!=NULL) verbosemode=1;
    if (strchr(argv[c],'u')!=NULL) uniquemode=1;
    if (strchr(argv[c],'r')!=NULL) recursivemode=1;
    c++;
  }
  if (argc-c==4) {
    if (strcmp(argv[c+1],"in")!=0) paramerr = 1;
    else {ident = argv[c];topIdent = argv[c+2];fname = argv[c+3];}
  } else
    if (argc-c!=2) paramerr=1;else {ident=argv[c];fname=argv[c+1];}
  if ((err)||(paramerr)) {
    if ((err==0)&&(paramerr)) fprintf(stderr,"\nNot enough parameters!\n");
    fprintf(stderr,"\nusage: hat-observe [-uvr] identifier [in topidentifier] file-name\n");
    fprintf(stderr,"       prints a table of all applications and results of the given\n");
    fprintf(stderr,"       top-level identifier [within the application of topidentifier].\n\n");
    fprintf(stderr,"options:\n");
    fprintf(stderr,"       u: unique mode. Only the most general function application is shown.\n");
    fprintf(stderr,"       v: verbose mode. Unevaluated expressions are shown in full.\n");;
    fprintf(stderr,"       r: recursive mode. Omitt recursive function applications.\n\n");
    exit(1);
  }
  if (!openfile(fname)) {
    fprintf(stderr, "cannot open trace file %s\n\n",fname);
    exit(1);
  }
  if (testheader()) {
    FunTable* results = newFunTable();
    checkNodes(ident,topIdent,results,verbosemode,uniquemode,recursivemode);

    if (uniquemode) {
      
      checkArities(results); // remove partial applications with missing arguments
      showFunTable(results);
      freeFunTable(results);
    }
  }
  closefile();
}

int isDescendantOf(unsigned long fileoffset,unsigned long parent) {
  char nodeType;
  if (parent==0) return 0;
  while (fileoffset!=0) {
    seek(fileoffset);
    nodeType=nextbyte();
    switch(nodeType) {
    case TRHIDDEN:
    case TRSATA:
    case TRSATB:
    case TRSATC:
      fileoffset=readpointer();
      break;
    case TRAPP:{
      unsigned long newoffs;
      skipbytes(1);
      newoffs = readpointer();
      if (leftmostOutermost(fileoffset)==parent) return 1;
      fileoffset = newoffs;
      break;
    }
    default:
      return 0;
    }
  }
  return 0;
}

int isDirectDescendantOf(unsigned long fileoffset,unsigned long parent) {
  char nodeType;
  if (parent==0) return 0;
  while (fileoffset!=0) {
    seek(fileoffset);
    nodeType=nextbyte();
    switch(nodeType) {
    case TRHIDDEN:
    case TRSATA:
    case TRSATB:
    case TRSATC:
      fileoffset=readpointer();
      break;
    case TRAPP:{
      unsigned long newoffs,lmo;
      skipbytes(1);
      newoffs = readpointer();
      if ((lmo=leftmostOutermost(fileoffset))==parent) return 1;
      if (isTopLevel(fileoffset)) return 0;
      fileoffset = newoffs;
      break;
    }
    default:
      return 0;
    }
  }
  return 0;
}

void checkNodes(char* identifier,
		char* topIdentifier,
		FunTable* result,int verboseMode,int uniqueMode,
		int recursiveMode) {
  unsigned long identifierNode = 0,p,progress=0,currentOffset,fsz,lsz=0;
  unsigned long topIdentifierNode = 0;
  char nodeType;
  HashTable* htable = newHashTable(80000);
  int arityProblem=0,arity,maxarity = -1;
  int found = 0;
  fsz = filesize()/1000;
  if (fsz<20000) lsz=200;
  else if (uniqueMode) {
    fprintf(stderr,"  0%%");
    fflush(stderr); // force printing to screen, even though LF is still missing...
  }
  if (fsz==0) fsz=1;
  while (more()) {
    currentOffset = byteoffset();
    if ((uniqueMode)&&((currentOffset/10)/fsz>lsz)) {
      lsz = (currentOffset/10)/fsz;
      fprintf(stderr,"\b\b\b\b%3u%%",lsz);
      fflush(stderr);
    }
    nodeType = nextbyte();
    switch (nodeType) {
    case TRAPP:
      {
	int arity = readarity();
	unsigned long apptrace;
	apptrace = readpointer();  // fileoffset of App-trace
	p = readpointer();         // fileoffset of Function-trace
	skipbytes(4*(arity+1));
	if (isInHashTable(htable,p)) {
	  unsigned long old = byteoffset();
	  unsigned long satc=findAppSAT(currentOffset);  // find SATC for the application!	  
	  if (seenextbyte()==TRSATC) {
	    if (followSATs(satc)==currentOffset) {
	      addToHashTable(htable,currentOffset); // remember partial application
	      addToHashTable(htable,satc);
	    } else
	      if (uniqueMode) {
		addToHashTable(htable,currentOffset);
		if (((recursiveMode==0)||(isDescendantOf(apptrace,identifierNode)==0))&&
		    ((topIdentifier==NULL)||
		     (isDirectDescendantOf(apptrace,topIdentifierNode)))) {
		  ExprNode* r=buildExpr(satc,verboseMode);
		  ExprNode* a=buildExpr(currentOffset,verboseMode);
		  arity = getArity(a);
		  if (arity>=maxarity) {
		    addToFunTable(result,a,r);
		    maxarity=arity;
		  } else {
		    freeExpr(r);
		    freeExpr(a);
		  }
		}
	      } else {
		addToHashTable(htable,currentOffset);
		if (((recursiveMode==0)||(isDescendantOf(apptrace,identifierNode)==0))&&
		    ((topIdentifier==NULL)||
		     (isDirectDescendantOf(apptrace,topIdentifierNode)))) {
		  ExprNode* a=buildExpr(currentOffset,verboseMode);
		  arity = getArity(a);
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
		    showAppAndResult(currentOffset,verboseMode);
		    found++;
		  }
		  freeExpr(a);
		}
	      }
	  }
	  seek(old);
	}
      }
      break;
    case TRNAM: // Name
      skippointer(); // continue now as SATC
      if ((readpointer()==identifierNode)&&(identifierNode!=0)) {
	//printf("found name reference for identifier at: %u\n",p);
	addToHashTable(htable,currentOffset);
	skippointer();
	if (seenextbyte()==TRSATC) {  // SATC behind TRNAM?
	  // found a CAF!
	  unsigned long satc = byteoffset();
	  if (uniqueMode) {
	    ExprNode* r=buildExpr(satc,verboseMode);
	    ExprNode* a=buildExpr(currentOffset,verboseMode);
	    addToFunTable(result,a,r);
	  } else { // print CAF and its value
	    showAppAndResult(currentOffset,verboseMode);
	  }
	  seek(satc);
	}
      } else skippointer();
      break;
    case NTIDENTIFIER:
      if ((identifierNode==0)||((topIdentifier!=NULL)&&(topIdentifierNode==0))) {
	if (isTopLevel(currentOffset)) {
	  // search for identifier
	  unsigned long offset = byteoffset()-1;
	  char *currentIdent = readstring(); // name of current identifier
	  //printf("identifier '%s'\n",currentIdent);
	  if ((identifierNode==0)&&(strcmp(currentIdent,identifier)==0)) { // found?
	    //printf("FOUND Identifier at: %u!\n",offset);
	    identifierNode = offset;
	    if ((topIdentifier!=NULL)&&(strcmp(identifier,topIdentifier)==0))
	      topIdentifierNode=offset; // both identifier are the same!
	  } else
	    if ((topIdentifier!=NULL)&&(strcmp(currentIdent,topIdentifier)==0))
	      topIdentifierNode=offset;
	} else {
	  skipstring();  // simply read string
	}
      } else skipstring();
      skipbytes(4+1+4);
      break; 
    default:
      skipNode(nodeType);
      break;
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






