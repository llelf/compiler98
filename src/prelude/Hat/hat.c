#include <stdio.h>
#include <string.h>
#include <signal.h>
#include "art.h"
#include "hat.h"

#if defined(DEBUG)
#define HIDE(x) x
#else
#define HIDE(x)
#endif


FILE *HatFile, *HatOutput, *HatBridge;

FileOffset hiddenMask;

/* from initexit ********************************************************* */

/* initialisation handler */

void
openTrace (char *progname)
{
    unsigned p = 0;
    char filename[256];

    hiddenMask = htonl(~(1<<31)); /* only 0 at highest bit */
    /* logical & with possibly hidden fileoffset yields fileoffset */

    strcpy(filename,progname);
    strcat(filename,".hat");		/* the .hat file holds the archive */
    HatFile = fopen(filename,"w");	/* of redex trails */
    p = ftell(HatFile);                 /* should be 0 */
    fprintf(HatFile,"Hat%s",VERSION);	/* initialise file */
    fputc(0,HatFile);
    fwrite(&p,sizeof(FileOffset),1,HatFile);
    fwrite(&p,sizeof(FileOffset),1,HatFile);

    initialiseSATstack();

    strcpy(filename,progname);		/* the .output file is a copy of */
    strcat(filename,".hat.output");	/* stdout */
    HatOutput = fopen(filename,"w");
    strcpy(filename,progname);		/* the .bridge file links the output */
    strcat(filename,".hat.bridge");	/* to the archived trails */
    HatBridge = fopen(filename,"w");

    signal(SIGQUIT, hat_interrupted);   /* install handler for Control-C */
    signal(SIGINT, hat_interrupted);
}


/* exit handlers */

void
closeTrace ()
{
  updateSatBs();  /* there should not be any */
  updateSatCs();

  fclose(HatFile);
  fclose(HatOutput);
  fclose(HatBridge);
}


void
errorTraceExit(char* errmsg, FileOffset trace, int ecode)
{
  FileOffset nt;

  fprintf(stderr, "%s\n", errmsg);
  nt = primNTCString(errmsg);
  nt &= hiddenMask;
  trace &= hiddenMask;
  fseek(HatFile,8+sizeof(FileOffset),SEEK_SET);
  fwrite(&nt, sizeof(FileOffset), 1, HatFile);
  fseek(HatFile,8,SEEK_SET);
  fwrite(&trace, sizeof(FileOffset), 1, HatFile);

  closeTrace();

  exit(ecode);	
}

void
fatal(FileOffset trace)
{ errorTraceExit("No match in pattern.", trace, 1); }

/* The following is not used currently
   Previously it was called from the dbg_trans version of the interpreter */
void
dbg_blackhole()
{ errorTraceExit("Blackhole detected.", readCurrentSatB(), 2); }

void
hat_interrupted(int sig)
{ errorTraceExit("Program interrupted. (^C)", readCurrentSatB(), 3); }


/* from fileformat ********************************************************* */

#define NUM_SATB   32000  /* should be equal to program's runtime stack size */
#define	NUM_SATC   32000

static FileOffset HatCounter = 8 + 2*sizeof(FileOffset);
static FileOffset *SATstack;  /* stack of traces of entered redexes */
static int        *SATstackSort; /* info for each SatB if it is lonely */
static int SATp = 0;
static FileOffset SATqueueA[NUM_SATC];
static FileOffset SATqueueC[NUM_SATC];
static int        SATqueueSort[NUM_SATC];
static int SATq = 0;

void
initialiseSATstack (void)
{
    SATstack = (FileOffset*)malloc(NUM_SATB * sizeof(FileOffset));
    if (SATstack==(FileOffset*)0) {
        fprintf(stderr,"Couldn't allocate %d words for SAT stack.\n",NUM_SATB);
        exit(10);
    }
    SATstackSort = (int*)malloc(NUM_SATB * sizeof(int));
    if (SATstackSort==(int*)0) {
        fprintf(stderr,"Couldn't allocate %d words for SAT stack.\n",NUM_SATB);
        exit(10);
    }
}

/* Remaining problems include (at least) the following:
 *   . All Rational values are currently dummy.
 *   . Floats and Doubles are written to the file without regard for endianness.
 */

FileOffset
primTRoot (void)
{
    HIDE(fprintf(stderr,"primTRoot\n");)
    return (FileOffset)0;
}

FileOffset
primTAp1 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp1 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x01,HatFile);
    tap &= hiddenMask;
    tfn &= hiddenMask;
    targ1 &= hiddenMask;
    fwrite(&(tap),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (4*sizeof(FileOffset));
    return fo;
}

FileOffset
primTAp2 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp2 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x02,HatFile);
    tap &= hiddenMask;
    tfn &= hiddenMask;
    targ1 &= hiddenMask;
    targ2 &= hiddenMask;
    fwrite(&(tap),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (5*sizeof(FileOffset));
    return fo;
}

FileOffset
primTAp3 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset targ3
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp3 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x03,HatFile);
    tap &= hiddenMask;
    tfn &= hiddenMask;
    targ1 &= hiddenMask;
    targ2 &= hiddenMask;
    targ3 &= hiddenMask;
    fwrite(&(tap),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (6*sizeof(FileOffset));
    return fo;
}

FileOffset
primTAp4 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset targ3
                        , FileOffset targ4
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp4 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x04,HatFile);
    tap &= hiddenMask;
    tfn &= hiddenMask;
    targ1 &= hiddenMask;
    targ2 &= hiddenMask;
    targ3 &= hiddenMask;
    targ4 &= hiddenMask;
    fwrite(&(tap),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (7*sizeof(FileOffset));
    return fo;
}

FileOffset
primTAp5 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset targ3
                        , FileOffset targ4
                        , FileOffset targ5
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp5 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x05,HatFile);
    tap &= hiddenMask;
    tfn &= hiddenMask;
    targ1 &= hiddenMask;
    targ2 &= hiddenMask;
    targ3 &= hiddenMask;
    targ4 &= hiddenMask;
    targ5 &= hiddenMask;
    fwrite(&(tap),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (8*sizeof(FileOffset));
    return fo;
}

FileOffset
primTAp6 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset targ3
                        , FileOffset targ4
                        , FileOffset targ5
                        , FileOffset targ6
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp6 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x06,HatFile);
    tap &= hiddenMask;
    tfn &= hiddenMask;
    targ1 &= hiddenMask;
    targ2 &= hiddenMask;
    targ3 &= hiddenMask;
    targ4 &= hiddenMask;
    targ5 &= hiddenMask;
    targ6 &= hiddenMask;
    fwrite(&(tap),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ6), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (9*sizeof(FileOffset));
    return fo;
}

FileOffset
primTAp7 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset targ3
                        , FileOffset targ4
                        , FileOffset targ5
                        , FileOffset targ6
                        , FileOffset targ7
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp7 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x07,HatFile);
    tap &= hiddenMask;
    tfn &= hiddenMask;
    targ1 &= hiddenMask;
    targ2 &= hiddenMask;
    targ3 &= hiddenMask;
    targ4 &= hiddenMask;
    targ5 &= hiddenMask;
    targ6 &= hiddenMask;
    targ7 &= hiddenMask;
    fwrite(&(tap),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ6), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ7), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (10*sizeof(FileOffset));
    return fo;
}

FileOffset
primTAp8 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset targ3
                        , FileOffset targ4
                        , FileOffset targ5
                        , FileOffset targ6
                        , FileOffset targ7
                        , FileOffset targ8
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp8 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x08,HatFile);
    tap &= hiddenMask;
    tfn &= hiddenMask;
    targ1 &= hiddenMask;
    targ2 &= hiddenMask;
    targ3 &= hiddenMask;
    targ4 &= hiddenMask;
    targ5 &= hiddenMask;
    targ6 &= hiddenMask;
    targ7 &= hiddenMask;
    targ8 &= hiddenMask;
    fwrite(&(tap),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ6), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ7), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ8), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (11*sizeof(FileOffset));
    return fo;
}

FileOffset
primTAp9 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset targ3
                        , FileOffset targ4
                        , FileOffset targ5
                        , FileOffset targ6
                        , FileOffset targ7
                        , FileOffset targ8
                        , FileOffset targ9
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp9 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x09,HatFile);
    tap &= hiddenMask;
    tfn &= hiddenMask;
    targ1 &= hiddenMask;
    targ2 &= hiddenMask;
    targ3 &= hiddenMask;
    targ4 &= hiddenMask;
    targ5 &= hiddenMask;
    targ6 &= hiddenMask;
    targ7 &= hiddenMask;
    targ8 &= hiddenMask;
    targ9 &= hiddenMask;
    fwrite(&(tap),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ6), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ7), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ8), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ9), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (12*sizeof(FileOffset));
    return fo;
}

FileOffset
primTAp10 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset targ3
                        , FileOffset targ4
                        , FileOffset targ5
                        , FileOffset targ6
                        , FileOffset targ7
                        , FileOffset targ8
                        , FileOffset targ9
                        , FileOffset targ10
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp10 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x0a,HatFile);
    tap &= hiddenMask;
    tfn &= hiddenMask;
    targ1 &= hiddenMask;
    targ2 &= hiddenMask;
    targ3 &= hiddenMask;
    targ4 &= hiddenMask;
    targ5 &= hiddenMask;
    targ6 &= hiddenMask;
    targ7 &= hiddenMask;
    targ8 &= hiddenMask;
    targ9 &= hiddenMask;
    targ10 &= hiddenMask;
    fwrite(&(tap),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ6), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ7), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ8), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ9), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ10), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (13*sizeof(FileOffset));
    return fo;
}

FileOffset
primTAp11 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset targ3
                        , FileOffset targ4
                        , FileOffset targ5
                        , FileOffset targ6
                        , FileOffset targ7
                        , FileOffset targ8
                        , FileOffset targ9
                        , FileOffset targ10
                        , FileOffset targ11
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp11 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x0b,HatFile);
    tap &= hiddenMask;
    tfn &= hiddenMask;
    targ1 &= hiddenMask;
    targ2 &= hiddenMask;
    targ3 &= hiddenMask;
    targ4 &= hiddenMask;
    targ5 &= hiddenMask;
    targ6 &= hiddenMask;
    targ7 &= hiddenMask;
    targ8 &= hiddenMask;
    targ9 &= hiddenMask;
    targ10 &= hiddenMask;
    targ11 &= hiddenMask;
    fwrite(&(tap),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ6), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ7), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ8), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ9), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ10), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ11), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (14*sizeof(FileOffset));
    return fo;
}

FileOffset
primTAp12 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset targ3
                        , FileOffset targ4
                        , FileOffset targ5
                        , FileOffset targ6
                        , FileOffset targ7
                        , FileOffset targ8
                        , FileOffset targ9
                        , FileOffset targ10
                        , FileOffset targ11
                        , FileOffset targ12
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp12 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x0c,HatFile);
    tap &= hiddenMask;
    tfn &= hiddenMask;
    targ1 &= hiddenMask;
    targ2 &= hiddenMask;
    targ3 &= hiddenMask;
    targ4 &= hiddenMask;
    targ5 &= hiddenMask;
    targ6 &= hiddenMask;
    targ7 &= hiddenMask;
    targ8 &= hiddenMask;
    targ9 &= hiddenMask;
    targ10 &= hiddenMask;
    targ11 &= hiddenMask;
    targ12 &= hiddenMask;
    fwrite(&(tap),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ6), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ7), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ8), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ9), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ10), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ11), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ12), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (15*sizeof(FileOffset));
    return fo;
}

FileOffset
primTAp13 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset targ3
                        , FileOffset targ4
                        , FileOffset targ5
                        , FileOffset targ6
                        , FileOffset targ7
                        , FileOffset targ8
                        , FileOffset targ9
                        , FileOffset targ10
                        , FileOffset targ11
                        , FileOffset targ12
                        , FileOffset targ13
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp13 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x0d,HatFile);
    tap &= hiddenMask;
    tfn &= hiddenMask;
    targ1 &= hiddenMask;
    targ2 &= hiddenMask;
    targ3 &= hiddenMask;
    targ4 &= hiddenMask;
    targ5 &= hiddenMask;
    targ6 &= hiddenMask;
    targ7 &= hiddenMask;
    targ8 &= hiddenMask;
    targ9 &= hiddenMask;
    targ10 &= hiddenMask;
    targ11 &= hiddenMask;
    targ12 &= hiddenMask;
    targ13 &= hiddenMask;
    fwrite(&(tap),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ6), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ7), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ8), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ9), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ10), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ11), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ12), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ13), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (16*sizeof(FileOffset));
    return fo;
}

FileOffset
primTAp14 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset targ3
                        , FileOffset targ4
                        , FileOffset targ5
                        , FileOffset targ6
                        , FileOffset targ7
                        , FileOffset targ8
                        , FileOffset targ9
                        , FileOffset targ10
                        , FileOffset targ11
                        , FileOffset targ12
                        , FileOffset targ13
                        , FileOffset targ14
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp14 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x0e,HatFile);
    tap &= hiddenMask;
    tfn &= hiddenMask;
    targ1 &= hiddenMask;
    targ2 &= hiddenMask;
    targ3 &= hiddenMask;
    targ4 &= hiddenMask;
    targ5 &= hiddenMask;
    targ6 &= hiddenMask;
    targ7 &= hiddenMask;
    targ8 &= hiddenMask;
    targ9 &= hiddenMask;
    targ10 &= hiddenMask;
    targ11 &= hiddenMask;
    targ12 &= hiddenMask;
    targ13 &= hiddenMask;
    targ14 &= hiddenMask;
    fwrite(&(tap),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ6), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ7), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ8), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ9), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ10), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ11), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ12), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ13), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ14), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (17*sizeof(FileOffset));
    return fo;
}

FileOffset
primTAp15 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset targ3
                        , FileOffset targ4
                        , FileOffset targ5
                        , FileOffset targ6
                        , FileOffset targ7
                        , FileOffset targ8
                        , FileOffset targ9
                        , FileOffset targ10
                        , FileOffset targ11
                        , FileOffset targ12
                        , FileOffset targ13
                        , FileOffset targ14
                        , FileOffset targ15
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp15 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x0f,HatFile);
    tap &= hiddenMask;
    tfn &= hiddenMask;
    targ1 &= hiddenMask;
    targ2 &= hiddenMask;
    targ3 &= hiddenMask;
    targ4 &= hiddenMask;
    targ5 &= hiddenMask;
    targ6 &= hiddenMask;
    targ7 &= hiddenMask;
    targ8 &= hiddenMask;
    targ9 &= hiddenMask;
    targ10 &= hiddenMask;
    targ11 &= hiddenMask;
    targ12 &= hiddenMask;
    targ13 &= hiddenMask;
    targ14 &= hiddenMask;
    targ15 &= hiddenMask;
    fwrite(&(tap),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ6), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ7), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ8), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ9), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ10), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ11), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ12), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ13), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ14), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ15), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (18*sizeof(FileOffset));
    return fo;
}






FileOffset
primTNm (FileOffset tnm, FileOffset nm, FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTNm 0x%x 0x%x 0x%x -> 0x%x\n",tnm,nm,sr,fo);)
    fputc(((Trace<<5) | TNm),HatFile);
    tnm &= hiddenMask;
    fwrite(&(tnm), sizeof(FileOffset), 1, HatFile);
    fwrite(&(nm),  sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 1 + (3*sizeof(FileOffset));
    return fo;
}

FileOffset
primTInd (FileOffset t1, FileOffset t2)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTInd 0x%x 0x%x -> 0x%x\n",t1,t2,fo);)
    fputc(((Trace<<5) | TInd),HatFile);
    t1 &= hiddenMask;
    t2 &= hiddenMask;
    fwrite(&(t1), sizeof(FileOffset), 1, HatFile);
    fwrite(&(t2), sizeof(FileOffset), 1, HatFile);
    HatCounter += 1 + (2*sizeof(FileOffset));
    return fo;
}



int
hidden (FileOffset t)
{
  return (!((t & hiddenMask) == t));
}

FileOffset
primTHidden (FileOffset t1)
{
    FileOffset fo;

    if (hidden(t1)) 
      return t1;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTHidden 0x%x -> 0x%x\n",t1,fo);)
    fputc(((Trace<<5) | THidden),HatFile);
    fwrite(&(t1), sizeof(FileOffset), 1, HatFile);
    HatCounter += 1 + (sizeof(FileOffset));
    return(fo | (~hiddenMask));
}

FileOffset
primTSatA (FileOffset t1)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTSatA 0x%x -> 0x%x\n",t1,fo);)
    fputc(((Trace<<5) | TSatA),HatFile);
    t1 &= hiddenMask;
    fwrite(&(t1), sizeof(FileOffset), 1, HatFile);
    HatCounter += 1 + (sizeof(FileOffset));
    return fo;
}


FileOffset
primTSatALonely (FileOffset t1)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTSatALonely 0x%x -> 0x%x\n",t1,fo);)
    fputc(((Trace<<5) | TSatAL),HatFile);
    t1 &= hiddenMask;
    fwrite(&(t1), sizeof(FileOffset), 1, HatFile);
    HatCounter += 1 + (sizeof(FileOffset));
    return fo;
}


/* The implementation of SatBs is as follows.
 *   Every SatB represents a function entry, and every SatC a function
 *   return.  Returns must be matched with enters in strictly
 *   LIFO order.  So we store SatBs in a stack in memory, and do not
 *   write them to file.  Every time we write a SatC we also pop the
 *   corresponding SatB off the stack, checking that it is indeed the
 *   expected SatB.  If the program fails, a separate routine scans
 *   the stack and writes the SatB markers to file.
 */

FileOffset
primTSatB (FileOffset t1)
{
    SATstackSort[SATp] = False;
    SATstack[SATp++] = t1&hiddenMask;
    if (SATp >= NUM_SATB) {
        fprintf(stderr,"Exceeded size of SAT stack\n");
        exit(1);
    }
    return t1;
}


FileOffset
primTSatBLonely (FileOffset t1)
{
    SATstackSort[SATp] = True;
    SATstack[SATp++] = t1&hiddenMask;
    if (SATp >= NUM_SATB) {
        fprintf(stderr,"Exceeded size of SAT stack\n");
        exit(1);
    }
    return t1;
}

/* This implementation of SatC saves up a bunch of updates to be done
 * all at once.  We make no attempt yet to place the updates into order,
 * which could potentially make the combined update even faster.
 */

FileOffset
primTSatC (FileOffset torig,FileOffset teval)
{
    torig &= hiddenMask;
    teval &= hiddenMask;
    if (SATstack[--SATp] != torig) {
        fprintf(stderr,"SAT stack is corrupt.\n");
        exit(1);
    } else {
	/* save updates until we have a bunch of them */
        SATqueueSort[SATq] = False;
        SATqueueA[SATq] = torig;
        SATqueueC[SATq] = teval;
        SATq++;
        if (SATq >= NUM_SATC) {
            updateSatCs();
        }
        return torig;
/*
	-- do each update individually
        fseek(HatFile,ntohl(torig),SEEK_SET);
        fputc(((Trace<<5) | TSatC),HatFile);
        fwrite(&teval, sizeof(FileOffset), 1, HatFile);
        fseek(HatFile,HatCounter,SEEK_SET);
        return torig;
*/
    }
}


FileOffset
primTSatCLonely (FileOffset torig,FileOffset teval)
{
    torig &= hiddenMask;
    teval &= hiddenMask;
    if (SATstack[--SATp] != torig) {
        fprintf(stderr,"SAT stack is corrupt.\n");
        exit(1);
    } else {
	/* save updates until we have a bunch of them */
        SATqueueSort[SATq] = True;
        SATqueueA[SATq] = torig;
        SATqueueC[SATq] = teval;
        SATq++;
        if (SATq >= NUM_SATC) {
            updateSatCs();
        }
        return torig;
/*
	-- do each update individually
        fseek(HatFile,ntohl(torig),SEEK_SET);
        fputc(((Trace<<5) | TSatC),HatFile);
        fwrite(&teval, sizeof(FileOffset), 1, HatFile);
        fseek(HatFile,HatCounter,SEEK_SET);
        return torig;
*/
    }
}

/* updateSatCs() can be called at any time.  It clears out the queue
 * of pending SatC requests, overwriting the appropriate SatA/Bs in
 * file, and resets the queue counter to zero.
 */

void
updateSatCs (void)
{
    int i;
    HIDE(fprintf(stderr,"\tupdateSatCs (%d SatCs) (%d SatBs)\n",SATq,SATp);)
    for (i=0; i<SATq; i++) {
        fseek(HatFile,ntohl(SATqueueA[i]),SEEK_SET);
        if (SATqueueSort[i]) 
          fputc(((Trace<<5) | TSatCL),HatFile);
        else
          fputc(((Trace<<5) | TSatC),HatFile);
        fwrite(&(SATqueueC[i]), sizeof(FileOffset), 1, HatFile);
    }
    fseek(HatFile,HatCounter,SEEK_SET);
    SATq = 0;
}

/* updateSatBs() can be called at any time.  It scans the stack
 * of pending SatB requests and overwrites the appropriate SatAs.
 * It does /not/ reset the stack pointer - the SatB stack could still
 * be in use for verifying SatC requests.
 */

void
updateSatBs (void)
{
    int i;
    HIDE(fprintf(stderr,"\tupdateSatBs (%d SatBs)\n",SATp);)
    for (i=0; i<SATp; i++) {
        fseek(HatFile,ntohl(SATstack[i]),SEEK_SET);
        if (SATstackSort[i])
          fputc(((Trace<<5) | TSatBL),HatFile);
        else
          fputc(((Trace<<5) | TSatB),HatFile);
    }
    fseek(HatFile,HatCounter,SEEK_SET);
}

/* readCurrentSatB() is used when the program is interrupted, to
 * identify the expression currently under evaluation.
 */

FileOffset
readCurrentSatB (void)
{
  return SATstack[SATp-1];
}


FileOffset
primNTInt (int i)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTInt %d -> 0x%x\n",i,fo);)
    fputc(((NmType<<5) | NTInt),HatFile);
    i = htonl(i);
    fwrite(&i, sizeof(int), 1, HatFile);
    HatCounter += 1 + (sizeof(int));
    return fo;
}

FileOffset
primNTChar (char c)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTChar '%c' -> 0x%x\n",c,fo);)
    fputc(((NmType<<5) | NTChar),HatFile);
    fwrite(&c, sizeof(char), 1, HatFile);
    HatCounter += 1 + (sizeof(char));
    return fo;
}


/* only works for Integer that fits into an Int */
FileOffset 
primNTInteger (int i)
{
    FileOffset fo;
    int ni;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTInteger -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTInteger),HatFile);
    fputc(0x01,HatFile);
    ni = htonl(i);
    fwrite(&ni, sizeof(int), 1, HatFile);
    HatCounter += 2+(sizeof(int));
    return fo;
}


FileOffset
primNTRational (int numerator, int denominator)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTRational -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTRational),HatFile);
    fputc(0x01,HatFile);	
    fwrite(&numerator, sizeof(int), 1, HatFile);
    fputc(0x01,HatFile);	
    fwrite(&denominator, sizeof(int), 1, HatFile);
    HatCounter += 3 + (2 * sizeof(int));
    return fo;
}

FileOffset
primNTFloat (float f)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTFloat -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTFloat),HatFile);
    fwrite(&f, sizeof(float), 1, HatFile);	/* ignore endian problems */
    HatCounter += 1 + (sizeof(float));
    return fo;
}

FileOffset
primNTDouble (double d)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTDouble -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTDouble),HatFile);
    fwrite(&d, sizeof(double), 1, HatFile);	/* ignore endian problems */
    HatCounter += 1 + (sizeof(double));
    return fo;
}

FileOffset
primNTTuple ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTTuple -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTTuple),HatFile);
    HatCounter += 1;
    return fo;
}

FileOffset
primNTFun ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTFun -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTFun),HatFile);
    HatCounter += 1;
    return fo;
}

FileOffset
primNTCase ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTCase -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTCase),HatFile);
    HatCounter += 1;
    return fo;
}

FileOffset
primNTLambda ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTLambda -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTLambda),HatFile);
    HatCounter += 1;
    return fo;
}

FileOffset
primNTDummy ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTDummy -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTDummy),HatFile);
    HatCounter += 1;
    return fo;
}

FileOffset
primNTCString (char *s)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTCString \"%s\" -> 0x%x\n",s,fo);)
    fputc(((NmType<<5) | NTCString),HatFile);
    fprintf(HatFile,"%s",s);
    fputc(0x0,HatFile);
    HatCounter = ftell(HatFile);
    return fo;
}

FileOffset
primNTIf ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTIf -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTIf),HatFile);
    HatCounter += 1;
    return fo;
}

FileOffset
primNTGuard ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTGuard -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTGuard),HatFile);
    HatCounter += 1;
    return fo;
}

FileOffset
primNTContainer ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTContainer -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTContainer),HatFile);
    HatCounter += 1;
    return fo;
}


FileOffset
primSourceRef (FileOffset moduleTraceInfo,int pos)
{
  int tracePos = htonl (pos);
  FileOffset fo = htonl(HatCounter);
  HIDE(fprintf(stderr,"\tprimSourceRef -> 0x%x\n",fo);)
  fputc((SR<<5),HatFile);
  fwrite(&moduleTraceInfo, sizeof(FileOffset), 1, HatFile);
  fwrite(&tracePos, sizeof(int), 1, HatFile);
  HatCounter += 1 + sizeof(FileOffset) + sizeof(int);
  return fo;
}


FileOffset
primAtomCon (FileOffset moduleTraceInfo, int pos, int fixPri, char *name)
{
  int tracePos = htonl (pos);
  FileOffset fo = htonl(HatCounter);
  HIDE(fprintf(stderr,"\tprimAtomId \"%s\" -> 0x%x\n",name,fo);)
  fputc(((NmType<<5) | NTConstr),HatFile);
  fprintf(HatFile,"%s",name);
  fputc(0x0,HatFile);
  fwrite(&moduleTraceInfo, sizeof(FileOffset), 1, HatFile);
  fputc(fixPri,HatFile);
  fwrite(&tracePos, sizeof(int), 1, HatFile);
  HatCounter = ftell(HatFile);
  return fo; 
}

FileOffset
primAtomId (FileOffset moduleTraceInfo, int pos, int fixPri, char *name)
{
  int tracePos = htonl (pos);
  FileOffset fo = htonl(HatCounter);
  HIDE(fprintf(stderr,"\tprimAtomId \"%s\" -> 0x%x\n",name,fo);)
  fputc(((NmType<<5) | NTId),HatFile);
  fprintf(HatFile,"%s",name);
  fputc(0x0,HatFile);
  fwrite(&moduleTraceInfo, sizeof(FileOffset), 1, HatFile);
  fputc(fixPri,HatFile);
  fwrite(&tracePos, sizeof(int), 1, HatFile);
  HatCounter = ftell(HatFile);
  return fo;
}

FileOffset
primAtomIdToplevel (FileOffset moduleTraceInfo, int pos, int fixPri, char *name)
{
  int tracePos = htonl (pos);
  FileOffset fo = htonl(HatCounter);
  HIDE(fprintf(stderr,"\tprimAtomId \"%s\" -> 0x%x\n",name,fo);)
  fputc(((NmType<<5) | NTToplevelId),HatFile); 
  fprintf(HatFile,"%s",name);
  fputc(0x0,HatFile);
  fwrite(&moduleTraceInfo, sizeof(FileOffset), 1, HatFile);
  fputc(fixPri,HatFile);
  fwrite(&tracePos, sizeof(int), 1, HatFile);
  HatCounter = ftell(HatFile);
  return fo;
}


FileOffset
primModule (char *modname, char *srcfile, int traced)
{
  FileOffset fo = htonl(HatCounter);
  HIDE(fprintf(stderr,"\tprimModule %s (%s) -> 0x%x\n",modname,srcfile,fo);)
  fprintf(HatFile,"%c%s%c%s%c", 0x20 + (traced?0:1)
                 ,modname, 0x0, srcfile, 0x0);
  HatCounter = ftell(HatFile);
  return fo;
}

void
outputTrace (FileOffset trace, char *output)
{
  fprintf(HatOutput,"%s",output);  /* copy of output */
  trace &= hiddenMask;
  while (*output++) {
    fwrite(&trace, sizeof(FileOffset), 1, HatBridge);
				/* link trace to output */
  }
}

