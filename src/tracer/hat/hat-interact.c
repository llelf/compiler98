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

char *readfixpriStr() {
  int b = (int)(nextbyte());
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
  b = nextbyte();
  switch (hi3(b)) {
  case TR:
    printf("TR %u: ", offset);
    switch (lo5(b)) {
    case APP:
      { 
	int arity = readarity();
	showAble = 1;
	printf("Application: ");
	printf("AppTrace %u, ",readpointer());
	printf("AppFun %u, ",readpointer());
	printf("Arguments [");
	for (; arity-- > 0;) {
	  printf("TR %u",readpointer());
	  if (arity>0) printf(",");
	}
	printf("], SRCREF %u",readpointer());
	break;
      }
    case NAM:
      showAble = 1;
      printf(" Name: ");
      printf("TR %u, ", readpointer());
      printf("NT %u, ", readpointer());
      printf("SR %u ", readpointer());
      break;
    case IND:
      printf(" Indirection: ");
      printf("TR %u, ", readpointer());
      printf("TR %u ", readpointer());
      break;
    case HIDDEN:
      showAble = 1;
      printf(" Hidden: ");
      printf("TR %u", readpointer());
      break;
    case SATA:
      showAble = 1;
      printf(" SAT(A): ");
      printf("TR %u", readpointer());
      break;
    case SATAIS:
      showAble = 1;
      printf(" isolated SAT(A): ");
      printf("TR %u", readpointer());
      break;
    case SATB:
      showAble = 1;
      printf(" SAT(B): ");
      printf("TR %u\t", readpointer());
      break;
    case SATBIS:
      showAble = 1;
      printf(" isolated SAT(B): ");
      printf("TR %u\t", readpointer());
      break;
    case SATC:
      showAble = 1;
      printf(" SAT(C): ");
      printf("TR %u", readpointer());
      break;
    case SATCIS:
      showAble = 1;
      printf(" isolated SAT(C): ");
      printf("TR %u", readpointer());
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
    printf("%s, ", readstring());
    printf("\"%s\"", readstring());
    break;
  case NT:
    printf("NT %u:  ", offset);
    switch (lo5(b)) {
    case INT:
      printf("INT %d", readint());
      break;
    case CHAR:
      printf("CHAR '%c'", readchar());
      break;
    case INTEGER:
      printf("INTEGER %i", readinteger());
      break;       
    case RATIONAL:
      printf("RATIONAL %s", readrational());
      break;
    case FLOAT:
      printf("FLOAT %f", readfloat());
      break;
    case DOUBLE:
      printf("DOUBLE %f", readdouble());
      break;
    case IDENTIFIER:
      printf("identifier: %s ", readstring());
      {
	unsigned long modinfo = readpointer();
	char *fp = readfixpriStr();
	if (*fp!='\0') printf("%s ", fp);
	printf("MD %u, ", modinfo);
	printf(" %s", readposn());
      }
      break; 
    case CONSTRUCTOR:
      printf("constructor\t\t%s ", readstring());
      { 
	unsigned long modinfo = readpointer();
	char *fp = readfixpriStr();
	if (*fp!='\0') printf("%s ", fp);
	printf("MD %u, ", modinfo);
	printf(" %s", readposn());
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
      printf("CSTRING \"%s\"", readstring());
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
    printf("MD %u\t", readpointer());
    printf(" %s", readposn());
    break;
  default:
    printf("strange high-bits tag %d at byte offset %u\n",
	   hi3(b), offset);
  }
  printf("\n");
  next = byteoffset();

  if (showAble) {
    unsigned long satc;
    char *appstr;
    ExprNode* exp;

    exp = buildExpr(offset,verboseMode);
    appstr = prettyPrintExpr(exp,1);
    
    satc = findAppSAT(followSATs(offset));
    seek(satc);
    if ((isSAT())&&(satc!=offset)) {
      printf("corresponding SAT at: %u\n\n",satc);
      printf("reduction: %s = ",appstr);
      freeStr(appstr);
      exp = buildExpr(satc,verboseMode);
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
