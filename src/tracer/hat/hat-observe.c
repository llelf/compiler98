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
#include <ctype.h>
#include "Expressions.h"
#include "FunTable.h"
#include "hatinterface.h"
#include "hashtable.h"
#include "observe.h"
#include "hatgeneral.h"


int getline(char s[], int max) {
  int c,i;
  fflush(stdout);
  c=getchar();
  for (i=0;(i<max-1) && (c!=EOF) && (c!='\n');i++) {
    if (c==-1) {
      i--;
    } else {
      s[i]=c;
    }
    c=getchar();
  }
  s[i]=0;
  return i;
}
//#define showNodeInfo
void showObserveAll(ObserveQuery query,int verboseMode,int precision) {
  int maxarity = -1;
  int arity,arityProblem = 0,found=0;
  filepointer currentOffset;
  HatFile handle = observeHatFile(query);

  currentOffset = nextObserveQueryNode(query);
  while (currentOffset != 0) {
    ExprNode* a=buildExpr(handle,currentOffset,verboseMode,precision);
    arity = getExprArity(a);
    maxarity=-1;
    if (arity>=maxarity) {
      if ((arity>maxarity)&&(maxarity!=-1)) {
	arityProblem=found;
	printf("Partial applications detected. Ignore all lines above!\n");
	printf("------------------------------------------------------\n");
      }
      maxarity = arity;
      //fprintf(stderr,"\b\b\b\b");
#ifdef showNodeInfo
      printf("(%u) ",currentOffset);
#endif
      showAppAndResult(handle,currentOffset,verboseMode,precision);
      found++;
    }
    freeExpr(a);
    currentOffset = nextObserveQueryNode(query);
  }
  if (arityProblem>0) {
    printf("\nAttention: Due to partial applications the first %i line(s)\n",
	   arityProblem);
    printf("are missing arguments - please ignore them!\n");
    printf("Use the -u option to see the correct applications only.\n\n");
  }
}

// extern bufferMisses;
// extern bufferHits;

int main (int argc, char *argv[])
{ int verbosemode=0,uniquemode=1,recursivemode=0,sourceRefMode=0;
  int c = 1,err=0,paramerr=0,handle;
  unsigned int precision = 100;
  char *fname=NULL,*ident=NULL,*topIdent=NULL,*sub;
  unsigned long line=0,column=0;
  char* moduleName = NULL;

  ObserveQuery query;
  while (c<argc) {
    err = checkParameters(argv[c],"vxurs"); // check for supported parameters
    if (err==1) { // not a parameter!
      err=0;
      break;
    }
    if (err==2) {
      fprintf(stderr,"\nUnsupported option! See below for possible flags!\n");
      break;
    }
    //if (strchr(argv[c],'i')!=NULL) interactmode=1;
    if (strchr(argv[c],'v')!=NULL) verbosemode=1;
    if (strchr(argv[c],'s')!=NULL) sourceRefMode=1;
    if ((sub=strchr(argv[c],'x'))!=NULL) {
      if (*(++sub)!='u') {
	err = 2;
	fprintf(stderr,"\nUnsupported 'x' option! See below for possible flags!\n");
	break;
      }
      uniquemode=0;
    }
    if (strchr(argv[c],'r')!=NULL) recursivemode=1;
    c++;
  }
  if (sourceRefMode) { // moduleName line column file
    if ((argc-c>=3)||(argc-c<=4)) {
      if (argc-c==4) moduleName=argv[c++];
      line = atoi(argv[c]);
      column = atoi(argv[c+1]);
      fname = argv[c+2];
      if ((line==0)||(column==0)) {
	fprintf(stderr,"line/column position need to be greater than 0.\n");
	err=1;
      }
    } else err=1;
  } else
  if (argc-c==4) {
    if (strcmp(argv[c+1],"in")!=0) paramerr = 1;
    else {ident = argv[c];topIdent = argv[c+2];fname = argv[c+3];}
  } else
    if (argc-c!=2) paramerr=1;else {ident=argv[c];fname=argv[c+1];}
  if ((err)||(paramerr)) {
    if ((err==0)&&(paramerr)) fprintf(stderr,"\nNot enough parameters!\n");
    fprintf(stderr,"\nusage: hat-observe [-v] [-r] [-xu] identifier [in topidentifier] file-name\n");
    fprintf(stderr,"       prints a table of all applications and results of the given\n");
    fprintf(stderr,"       top-level identifier [within the application of topidentifier].\n\n");
    fprintf(stderr,"options:\n");
    fprintf(stderr,"       v: verbose mode. Unevaluated expressions are shown in full.\n");;
    fprintf(stderr,"       r: recursive mode. Omitt recursive function applications.\n\n");
    fprintf(stderr,"       xu: expert's mode for a very fast response. All applications\n");
    fprintf(stderr,"           of the identifier are shown, rather than only the most\n");
    fprintf(stderr,"           general ones. Using this option may result in incomplete\n");
    fprintf(stderr,"           applications, missing arguments. \n\n");
    exit(1);
  }
  if ((handle=hatOpenFile(fname))==-1) {
    fprintf(stderr, "cannot open trace file %s\n\n",fname);
    exit(1);
  }
  if (handle==-2) {
    fprintf(stderr, "format of file unknwon/not supported %s\n\n",fname);
    exit(1);
  }
  if (sourceRefMode) {
    filepointer h;
    query = newObserveQuerySource(handle,moduleName,line,column,1);
    while (h=nextObserveQueryNode(query)) {
      filepointer fun;
      printf("(%u) ",h);
      fun = hatOutermostSymbol(handle,h);
      if (getNodeType(handle,fun)==HatConstructor) {
	showNode(handle,h,verbosemode,precision);
	printf("\n");
      } else
	showAppAndResult(handle,h,verbosemode,precision);
    }
  } else {
    query = newObserveQueryIdent(handle,ident,topIdent,recursivemode,1);
    if (observeIdentifier(query)==0) {
      printf("No toplevel identifier named \"%s\" found!\n",ident);
      exit(1);
    }
    if ((topIdent!=NULL)&&(observeTopIdentifier(query)==0)) {
      printf("No toplevel identifier named \"%s\" found!\n",topIdent);
      exit(1);
    }
    if (uniquemode) {
      unsigned long c;
      FunTable results = observeUnique(query,verbosemode,precision);
      c = FunTableLength(results);
      if (c==0)	{
	fprintf(stderr,"No matching applications of \"%s\" found!\n",ident);
      } else {
	unsigned long total = observedNodes(query);
	fprintf(stderr,"%i unique application(s) found (%i in total).\n",c,total);
	if (c>20) {
	  char answer[10];
	  fprintf(stderr,"Show all %i applications? ",c);
	  if ((getline(answer,9)==0)||(toupper(answer[0])=='N')) exit(0);
	}
	showFunTable(results,precision);
      }
      freeFunTable(results);
    } else {
      showObserveAll(query,verbosemode,precision);
    
      //fprintf(stderr,"buffer misses: %i  buffer hits: %i\n",bufferMisses,bufferHits);
    }
  }
  freeObserveQuery(query);
  hatCloseFile(handle);
  return 0;
}

