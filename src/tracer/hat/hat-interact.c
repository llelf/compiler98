/* hat-interactive: nice tool for traversing the hat file structure
 */

/* #include <unistd.h> */
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include "Expressions.h"
#include "hatfileops.h"

/* Main driver and routines to providing basic interface to archive file.
 */

void interactive();
char* fname;
int verboseMode = 0;
unsigned int precision = 30;

main (int argc, char *argv[])
{
  if (argc!=2) {
    fprintf(stderr,"\nusage: hat-interactive file-name\n");
    exit(1);
  }
  fname = filename(argv[1]);
  if (!openfile(fname)) {
    fprintf(stderr, "cannot open trace file %s\n\n",argv[1]);
    exit(1);
  }
  if (testheader()) {

    interactive(0);

  }
  closefile();
}
/*
void showOutput() {
  unsigned long p =  
    p <- readpointer bridge
    c <- readchar output
    putStr (prettyPrintChar c)
    outputForSamePointer output bridge p
    putStrLn ""
    return p  -- returns first pointer

outputForSamePointer :: Handle -> Handle -> Int -> IO()
outputForSamePointer output bridge p =
 do
   m <- more output
   if m == False then
     putStr ("\t -> "++(show p))
    else
      do
        p2 <- readpointer bridge
        c <- readchar output
        if (p == p2) then
           putStr (prettyPrintChar c) >>
           outputForSamePointer output bridge p
         else
           putStr ("\t -> "++(show p)++"\n"++(prettyPrintChar c)) >>
           outputForSamePointer output bridge p2

void checkOutput() {
  char str[255];
  int output,bridge;

  sprintf(str,"%s%s",fname,".output");
  output = open(str, 0);
  sprintf(str,"%s%s",fname,".bridge");
  bridge = open(str, 0);
  if (isEOF(output)) printf("Program had no output\n\n");
  else {
    printf("output text\t -> address\n");
    showOutput(output,bridge);
    printf("\n");
  }
  close(output);
  close(bridge);
}*/

int isCmd(char* s,char* s1,char* s2) {
  if (strcmp(s,s1)==0) return 1;
  return (strcmp(s,s2)==0);
}

#define FIXPRIMAX 8
char fixpribuf[FIXPRIMAX+1];

char *getfixpriStr() {
  int b = (int)(getInfixPrio());
  switch (b % 4) {
  case 0:
    sprintf(fixpribuf, "infix %d", b/4);
    break;
  case 1:
    sprintf(fixpribuf, "infixr %d", b/4);
    break;
  case 2:
    sprintf(fixpribuf, "infixl %d", b/4);
    break;
  case 3:
    sprintf(fixpribuf, "");
    break;
  }
  return fixpribuf;
}

unsigned long printNode(unsigned long offset) {
  char b,showAble=0;
  unsigned long next;
  seek(offset);
  b = getNodeType();
  switch (hi3(b)) {
  case TR:
    printf("TR %u: ", offset);
    switch (lo5(b)) {
    case APP:
      { 
	int i=0;
	int arity = getAppArity();
	showAble = 1;
	printf("Application: ");
	printf("AppTrace %u, ",getTrace());
	printf("AppFun %u, ",getFunTrace());
	printf("Arguments [");
	while (i++<arity) {
	  printf("TR %u",getAppArgument(i-1));
	  if (i<arity) printf(",");
	}
	printf("], SRCREF %u",getSrcRef());
	break;
      }
    case NAM:
      showAble = 1;
      printf(" Name: ");
      printf("TR %u, ", getTrace());
      printf("NT %u, ", getNmType());
      printf("SR %u ", getSrcRef());
      break;
    case IND:
      printf(" Indirection: ");
      printf("TR %u, ", getTrace());
      printf("TR %u ", getValueTrace());
      break;
    case HIDDEN:
      showAble = 1;
      printf(" Hidden: ");
      printf("TR %u", getTrace());
      break;
    case SATA:
      showAble = 1;
      printf(" SAT(A): ");
      printf("TR %u", getTrace());
      break;
    case SATAIS:
      showAble = 1;
      printf(" isolated SAT(A): ");
      printf("TR %u", getTrace());
      break;
    case SATB:
      showAble = 1;
      printf(" SAT(B): ");
      printf("TR %u\t", getTrace());
      break;
    case SATBIS:
      showAble = 1;
      printf(" isolated SAT(B): ");
      printf("TR %u\t", getTrace());
      break;
    case SATC:
      showAble = 1;
      printf(" SAT(C): ");
      printf("TR %u", getTrace());
      break;
    case SATCIS:
      showAble = 1;
      printf(" isolated SAT(C): ");
      printf("TR %u", getTrace());
      break;
    default:
      printf("strange low-bits tag %d in TR %u\n",
	     lo5(b), offset);
    }
    break;
  case MD:
    printf("MD %u: ", offset);
    switch (lo5(b)) {
    case SUSPECT: printf("module (suspect), "); break;
    case TRUSTED: printf("module (trusted), "); break;
    default: printf("WRONG, "); break;
    }
    printf("%s, ", getName());
    printf("\"%s\"", getSrcName());
    break;
  case NT:
    printf("NT %u:  ", offset);
    switch (lo5(b)) {
    case INT:
      printf("INT %d", getIntValue());
      break;
    case CHAR:
      printf("CHAR '%c'", getCharValue());
      break;
    case INTEGER:
      printf("INTEGER %i", getIntegerValue());
      break;       
    case RATIONAL:
      printf("RATIONAL %s", getRationalValue());
      break;
    case FLOAT:
      printf("FLOAT %f", getFloatValue());
      break;
    case DOUBLE:
      printf("DOUBLE %f", getDoubleValue());
      break;
    case IDENTIFIER:
      printf("identifier: %s ", getName());
      {
	unsigned long modinfo = getModInfo();
	char *fp = getfixpriStr();
	if (*fp!='\0') printf("%s ", fp);
	printf("MD %u, ", modinfo);
	printf(" %s", getPosnStr());
      }
      break; 
    case CONSTRUCTOR:
      printf("constructor\t\t%s ", getName());
      { 
	unsigned long modinfo = getModInfo();
	char *fp = getfixpriStr();
	if (*fp!='\0') printf("%s ", fp);
	printf("MD %u, ", modinfo);
	printf(" %s", getPosnStr());
      }
      break; 
    case TUPLE:
      printf("TUPLE");
      break;
    case FUN:
      printf("FUN");
      break;
    case CASE:
      printf("CASE");
      break;
    case LAMBDA:
      printf("LAMBDA");
      break;
    case DUMMY:
      printf("DUMMY");
      break;
    case CSTRING:
      printf("CSTRING \"%s\"", getName());
      break;
    case IF:
      printf("IF");
      break;
    case GUARD:
      printf("GUARD");
      break;
    case CONTAINER:
      printf("CONTAINER");
      break;
    default:
      printf("strange low-bits tag %d in NT %u\n",
	     lo5(b), offset);
    }
    break;
  case SR:
    printf("SR %u:  Source reference\t", offset);
    printf("MD %u\t", getModInfo());
    printf(" %s", getPosnStr());
    break;
  default:
    printf("strange high-bits tag %d at byte offset %u\n",
	   hi3(b), offset);
  }
  printf("\n");
  nextNode();
  next = byteoffset();

  if (showAble) {
    unsigned long satc;
    char *appstr;
    ExprNode* exp;

    exp = buildExpr(offset,verboseMode,precision);
    appstr = prettyPrintExpr(exp,1);
    
    satc = findAppSAT(followSATs(offset));
    seek(satc);
    if ((isSAT(satc))&&(satc!=offset)) {
      printf("corresponding SAT at: %u\n\n",satc);
      printf("reduction: %s = ",appstr);
      freeStr(appstr);
      exp = buildExpr(satc,verboseMode,precision);
      appstr = prettyPrintExpr(exp,1);
      printf("%s\n",appstr);
    } else {
      printf("\nredex: %s\n",appstr);
    }
    freeStr(appstr);
  }
  seek(next);
  return next;
}

void interactive(unsigned long current) {
  char command[255];
  int toplevel=0;
  unsigned long adr,next;

  toplevel = (current==0);
  if (current == 0) current = byteoffset();
  while (1) {
    next = 0;
    if (current!=0) next=printNode(current);
    printf("\nenter <address>, (b)ack, (n)ext=%u or (f)ollow SATs, (q)uit: ",next);
    getline(command,80);
    if (isCmd(command,"back","b")) {
      if (toplevel) printf("Sorry. Already at beginning. Can't go back!\n");
      else return;
    }  else
      if (isCmd(command,"quit","q")) {
	printf("Ok. Goodbye. Thank you for using hat-interactive! :)\n\n");
	exit(0);
      } else {
	if (isCmd(command,"next","n")) {
	  if (next!=0) {
	    interactive(next);
	  }
	} else {
	  if (isCmd(command,"follow","f")) {
	    adr = followSATs(current);
	  } else
	    adr = atol(command);
	  if (adr!=0) {
	    if (current==0) current=adr;else
	      interactive(adr);
	  } else {
	    if (isCmd(command,"verbose","v")) {
	      verboseMode=1-verboseMode;
	      printf("verbose mode is now ");
	      if (verboseMode) printf("ON\n"); else printf("OFF\n");
	    } else
	      printf("Please enter 'b','n','q' or an address!\n");
	  }
	}
      }	
  }
}