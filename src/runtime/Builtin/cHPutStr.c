#include "haskell2c.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>

#if 0
/* This is inefficient for large strings, because the preceding packString
 * in Haskell takes a lot of heap.
 */
/* foreign import hPutStrC :: IO.Handle -> PackedString -> IO () */
void hPutStrC (FileDesc *f, char *s)
{
    int err;
    err = fputs(s,f->fp);
 /* if (err==EOF) return mkLeft(mkInt(errno)); */
 /* else return mkRight(mkUnit); */
}
#endif

/* So, we need to 'chunk' the string on the C-side to improve performance.
 */
/* foreign import hPutStrC :: IO.Handle -> String -> IO () */
#define CHUNK	BUFSIZ

static void hPutStr_ByChar (FileDesc *f, NodePtr s);
static void hPutStr_ByLine (FileDesc *f, NodePtr s);
static void hPutStr_ByBuff (FileDesc *f, NodePtr s, int size);

void hPutStrC (FileDesc *f, NodePtr s)
{
  switch (f->bm) {
    case _IONBF: hPutStr_ByChar(f,s); break;
    case _IOLBF: hPutStr_ByLine(f,s); break;
    case _IOFBF: hPutStr_ByBuff(f,s,f->size); break;
  }
  return;
}

static void 
hPutStr_ByChar (FileDesc *f, NodePtr s)
{
  int err;
  char c;
  NodePtr src=s, chr;

  while (1) {
    C_PUSH(src);
    C_EVALTOS(src);
    C_POP();
    IND_REMOVE(src);
    switch (GET_CONSTR(src)) { 
      case 0: /* []  */
            return;
            break;
      case 1: /* (:) */
            chr = GET_POINTER_ARG1(src,1);
            src = GET_POINTER_ARG1(src,2);
            C_PUSH(chr);
            C_EVALTOS(chr);
            C_POP();
            IND_REMOVE(chr);
            c = GET_CHAR_VALUE(chr);
            err = fputc(c,f->fp);
            break;
      default: /* error */
            fprintf(stderr,"hPutStr: internal error");
    }
  }
}


static void 
hPutStr_ByLine (FileDesc *f, NodePtr s)
{
  int err, count;
  char buf[CHUNK];
  char *dstptr;
  NodePtr src=s, chr;

  while (1) {
    dstptr = &buf[0];
    count=CHUNK-1;
    while (--count) {
      C_PUSH(src);
      C_EVALTOS(src);
      C_POP();
      IND_REMOVE(src);
      switch (GET_CONSTR(src)) { 
        case 0: /* []  */
              *dstptr = '\0';
              err = fputs(buf,f->fp);
              return;
              break;
        case 1: /* (:) */
              chr = GET_POINTER_ARG1(src,1);
              src = GET_POINTER_ARG1(src,2);
              C_PUSH(chr);
              C_EVALTOS(chr);
              C_POP();
              IND_REMOVE(chr);
              *dstptr = GET_CHAR_VALUE(chr);
              switch (*dstptr++) {
                case '\n': *dstptr = '\0';
                           err = fputs(buf,f->fp);
                           dstptr = &buf[0];	/* re-initialise loop */
                           count=CHUNK-1;
                           break;
                default: break;
              }
              break;
        default: /* error */
              fprintf(stderr,"hPutStr: internal error");
      }
    }
    *dstptr = '\0';
    err = fputs(buf,f->fp);
  }
}


static void 
hPutStr_ByBuff (FileDesc *f, NodePtr s, int reqsize)
{
  int err, count;
  static char *buf;
  static int actualsize=CHUNK;
  char *dstptr;
  NodePtr src=s, chr;

  if (buf==0) {
    /*fprintf(stderr,"hPutStr_ByBuff:  initial malloc %d\n",actualsize);*/
    buf = (char*)malloc(actualsize*sizeof(char));
  }
  if (reqsize > actualsize) {
    /*fprintf(stderr,"hPutStr_ByBuff:    realloc %d\n",reqsize);*/
    buf = (char*)realloc(buf,reqsize*sizeof(char));
    actualsize = reqsize;
  }
  if (reqsize<=0)
    reqsize=actualsize;

  while (1) {
    dstptr = &buf[0];
    count=reqsize-1;
    while (--count) {
      C_PUSH(src);
      C_EVALTOS(src);
      C_POP();
      IND_REMOVE(src);
      switch (GET_CONSTR(src)) { 
        case 0: /* []  */
              *dstptr = '\0';
              err = fputs(buf,f->fp);
              return;
              break;
        case 1: /* (:) */
              chr = GET_POINTER_ARG1(src,1);
              src = GET_POINTER_ARG1(src,2);
              C_PUSH(chr);
              C_EVALTOS(chr);
              C_POP();
              IND_REMOVE(chr);
              *dstptr++ = GET_CHAR_VALUE(chr);
              break;
        default: /* error */
              fprintf(stderr,"hPutStr: internal error");
      }
    }
    *dstptr = '\0';
    err = fputs(buf,f->fp);
  }
}

