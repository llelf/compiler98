#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include "hat.h"
#include "utils.h"

#define MAX_DEPTH 32

int
main (int argc, char** argv)
{
  char *expr; Ident* msg; SrcRef* sr;
  int i=0, infix;
  FileOffset parent;

  initialise(argc,argv);
  if (errorMsg) {
    msg = readNmTypeAt(errorMsg);
    fprintf(stdout,"Program terminated with error:\n    %s\n",msg->idname);
    parent = errorRoot;
    fprintf(stdout,"Virtual stack trace:\n");
    while (parent) {
      parent = readTraceAt(parent,&expr,&sr,&infix);
      fprintf(stdout,"    %s\t\t(%s: line-%d/col-%d)\n"
                    ,expr,sr->srcname,sr->line,sr->column);
      if (i++ > MAX_DEPTH) parent=0;
    }
  } else {
    fprintf(stdout,"No runtime error in program %s\n",argv[1]);
  }
  finalise();
}
