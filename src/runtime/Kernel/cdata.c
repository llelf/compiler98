#include <stdlib.h>
#include <stdio.h>

#include "runtime.h"

#define MAX_CDATA 1024

static CData cdata[MAX_CDATA];

CData cdata_stdin;
CData cdata_stdout;
CData cdata_stderr;

void initCData(void)
{
  int i;
  for(i=0; i<MAX_CDATA; i++) {
    cdata[i].used = 0;
    cdata[i].arg.gc = NULL;
  }
  cdata_stdin.used = 1; 
  cdata_stdin.arg.fp = stdin; 
  cdata_stdin.arg.bm = _IOLBF; 
  cdata_stdin.arg.size = -1; 
  cdata_stdin.arg.gc = gcNone;
  cdata_stdout.used = 1; 
  cdata_stdout.arg.fp = stdout;
  cdata_stdout.arg.bm = _IOLBF;
  cdata_stdout.arg.size = -1;
  cdata_stdout.arg.gc = gcNone;
  cdata_stderr.used = 1; 
  cdata_stderr.arg.fp = stderr;
  cdata_stderr.arg.bm = _IOLBF;
  cdata_stderr.arg.size = -1;
  cdata_stderr.arg.gc = gcNone;
}

CData * allocCData(Arg arg)
{
  int i;
  for(i=0; i<MAX_CDATA; i++) {
    if(!cdata[i].used) {
      cdata[i].used = 1;
      cdata[i].arg = arg;
      return &cdata[i];
    }
  }
  fprintf(stderr,"Warning: allocation limit exceeded for ForeignObj\n");
  return 0;
}

void freeCData(CData *cd)
{
  cd->arg.gc(cd);
  cd->used = 0;
}

Arg *cdataArg(CData *cd)
{
  return &cd->arg;
}

void clearCData(void)
{
  int i;
  for(i=0; i<MAX_CDATA; i++)
    cdata[i].used = 0;
}

void markCData(CData *cd)
{
  cd->used++;
}

void gcCData(void)
{
  int i;
  for(i=0; i<MAX_CDATA; i++)
    if(cdata[i].used == 0 && cdata[i].arg.gc) {
      cdata[i].arg.gc(&cdata[i]);
      cdata[i].arg.gc = NULL;
    } 
}

void gcFile(CData *cd)
{
  Arg *a = cdataArg(cd);
  fclose(a->fp);
}

void gcSocket(CData *cd)
{
  Arg *a = cdataArg(cd);
  close(a->fdesc);
}

void gcCVal(CData *cd)		/* added by MW */
{
  Arg *a = cdataArg(cd);
  a->gcc(a->cval);
  a->cval = NULL;
  a->gcc  = NULL;
}

void gcNone(CData *cd)
{
  return;
}
