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
#include "hatinterface.h"
#include "hashtable.h"
#include "observe.h"
#include "hatgeneral.h"


void showObserveAll(ObserveQuery query,int verboseMode,int precision) {
  int maxarity = -1;
  int arity,arityProblem = 0,found=0;
  filepointer currentOffset;

  currentOffset = nextQueryNode(query);
  while (currentOffset != 0) {
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
    currentOffset = nextQueryNode(query);
  }
  if (arityProblem>0) {
    printf("\nAttention: Due to partial applications the first %i line(s)\n",
	   arityProblem);
    printf("are missing arguments - please ignore them!\n");
    printf("Use the -u option to see the correct applications only.\n\n");
  }
}

main (int argc, char *argv[])
{ int verbosemode=0,uniquemode=1,recursivemode=0;
  int c = 1,err=0,paramerr=0,handle;
  unsigned int precision = 30;
  char *fname=NULL,*ident=NULL,*topIdent=NULL,*sub;
  while (c<argc) {
    err = checkParameters(argv[c],"vxur"); // check for supported parameters
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
  if ((handle=hatOpenFile(fname))<0) {
    fprintf(stderr, "cannot open trace file %s\n\n",fname);
    exit(1);
  }
  if (hatTestHeader()) {
    ObserveQuery query = newIdentifierQuery(handle,ident,topIdent,recursivemode,1);
    if (uniquemode) {
      FunTable results = observeUnique(query,verbosemode,precision);
      showFunTable(results);
      freeFunTable(results);
    } else {
      showObserveAll(query,verbosemode,precision);
    }
  }
  hatCloseFile(handle);
}
