#include <stdio.h>
#include "fileformat.h"
#include "cinterface.h"


#if defined(DEBUG)
#define HIDE(x) x
#else
#define HIDE(x)
#endif

extern int spSize;	/* program's runtime stack size */
#define	NUM_SATC	32000

static FileOffset HatCounter = 8 + 2*sizeof(FileOffset);
static FileOffset *SATstack;
static int        *SATstackSort;
static int SATp = 0;
static FileOffset SATqueueA[NUM_SATC];
static FileOffset SATqueueC[NUM_SATC];
static int        SATqueueSort[NUM_SATC];
static int SATq = 0;

void
initialiseSATstack (void)
{
    SATstack = (FileOffset*)malloc(spSize * sizeof(FileOffset));
    if (SATstack==(FileOffset*)0) {
        fprintf(stderr,"Couldn't allocate %d words for SAT stack.\n",spSize);
        exit(10);
    }
    SATstackSort = (int*)malloc(spSize * sizeof(int));
    if (SATstackSort==(int*)0) {
        fprintf(stderr,"Couldn't allocate %d words for SAT stack.\n",spSize);
        exit(10);
    }
}

/* Remaining problems include (at least) the following:
 *   . All Rational values are currently dummy.
 *   . Floats and Doubles are written to the file without regard for endianness.
 */


FileOffset
primModInfo (ModInfo *m)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimModInfo %s (%s) -> 0x%x\n",m->modname,m->srcfile,fo);)
    fprintf(HatFile,"%c%s%c%s%c", (m->trusted ? 0x21 : 0x20)
                    ,m->modname, 0x0, m->srcfile, 0x0);
    m->fileoffset = fo;
    HatCounter = ftell(HatFile);
    return fo;
}


/* Function to build a CTrace triple from components.
 */

CTrace*
mkTrace (FileOffset fo, int trustedness, int hiddenness)
{
    CTrace* t;
    C_CHECK(1+EXTRA+3);
    t = (CTrace*) C_ALLOC(1+EXTRA+3);
    t->constr = CONSTR(0,3,3);
    INIT_PROFINFO((void*)t,&dummyProfInfo)
    t->ptr = fo;
    t->trust = trustedness;
    t->hidden = hiddenness;
    HIDE(fprintf(stderr,"\tmkTrace 0x%x %s %s -> 0x%x\n",fo
                                                   ,(t->trust?"True":"False")
                                                   ,(t->hidden?"True":"False")
                                                   ,t);)
    return t;
}


CTrace*
primTRoot (void)
{
    HIDE(fprintf(stderr,"primTRoot\n");)
    return mkTrace((FileOffset)0,True,False); /* test: trustedness was False */
}

CTrace*
primTAp1 (CTrace* tap, CTrace* tfn
                        , CTrace* targ1
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp1 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x01,HatFile);
    fwrite(&(tap->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (4*sizeof(FileOffset));
    return mkTrace(fo,tfn->trust,False);
}

CTrace*
primTAp2 (CTrace* tap, CTrace* tfn
                        , CTrace* targ1
                        , CTrace* targ2
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp2 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x02,HatFile);
    fwrite(&(tap->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (5*sizeof(FileOffset));
    return mkTrace(fo,tfn->trust,False);
}

CTrace*
primTAp3 (CTrace* tap, CTrace* tfn
                        , CTrace* targ1
                        , CTrace* targ2
                        , CTrace* targ3
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp3 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x03,HatFile);
    fwrite(&(tap->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (6*sizeof(FileOffset));
    return mkTrace(fo,tfn->trust,False);
}

CTrace*
primTAp4 (CTrace* tap, CTrace* tfn
                        , CTrace* targ1
                        , CTrace* targ2
                        , CTrace* targ3
                        , CTrace* targ4
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp4 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x04,HatFile);
    fwrite(&(tap->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (7*sizeof(FileOffset));
    return mkTrace(fo,tfn->trust,False);
}

CTrace*
primTAp5 (CTrace* tap, CTrace* tfn
                        , CTrace* targ1
                        , CTrace* targ2
                        , CTrace* targ3
                        , CTrace* targ4
                        , CTrace* targ5
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp5 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x05,HatFile);
    fwrite(&(tap->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (8*sizeof(FileOffset));
    return mkTrace(fo,tfn->trust,False);
}

CTrace*
primTAp6 (CTrace* tap, CTrace* tfn
                        , CTrace* targ1
                        , CTrace* targ2
                        , CTrace* targ3
                        , CTrace* targ4
                        , CTrace* targ5
                        , CTrace* targ6
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp6 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x06,HatFile);
    fwrite(&(tap->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ6->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (9*sizeof(FileOffset));
    return mkTrace(fo,tfn->trust,False);
}

CTrace*
primTAp7 (CTrace* tap, CTrace* tfn
                        , CTrace* targ1
                        , CTrace* targ2
                        , CTrace* targ3
                        , CTrace* targ4
                        , CTrace* targ5
                        , CTrace* targ6
                        , CTrace* targ7
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp7 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x07,HatFile);
    fwrite(&(tap->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ6->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ7->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (10*sizeof(FileOffset));
    return mkTrace(fo,tfn->trust,False);
}

CTrace*
primTAp8 (CTrace* tap, CTrace* tfn
                        , CTrace* targ1
                        , CTrace* targ2
                        , CTrace* targ3
                        , CTrace* targ4
                        , CTrace* targ5
                        , CTrace* targ6
                        , CTrace* targ7
                        , CTrace* targ8
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp8 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x08,HatFile);
    fwrite(&(tap->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ6->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ7->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ8->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (11*sizeof(FileOffset));
    return mkTrace(fo,tfn->trust,False);
}

CTrace*
primTAp9 (CTrace* tap, CTrace* tfn
                        , CTrace* targ1
                        , CTrace* targ2
                        , CTrace* targ3
                        , CTrace* targ4
                        , CTrace* targ5
                        , CTrace* targ6
                        , CTrace* targ7
                        , CTrace* targ8
                        , CTrace* targ9
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp9 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x09,HatFile);
    fwrite(&(tap->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ6->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ7->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ8->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ9->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (12*sizeof(FileOffset));
    return mkTrace(fo,tfn->trust,False);
}

CTrace*
primTAp10 (CTrace* tap, CTrace* tfn
                        , CTrace* targ1
                        , CTrace* targ2
                        , CTrace* targ3
                        , CTrace* targ4
                        , CTrace* targ5
                        , CTrace* targ6
                        , CTrace* targ7
                        , CTrace* targ8
                        , CTrace* targ9
                        , CTrace* targ10
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp10 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x0a,HatFile);
    fwrite(&(tap->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ6->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ7->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ8->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ9->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ10->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (13*sizeof(FileOffset));
    return mkTrace(fo,tfn->trust,False);
}

CTrace*
primTAp11 (CTrace* tap, CTrace* tfn
                        , CTrace* targ1
                        , CTrace* targ2
                        , CTrace* targ3
                        , CTrace* targ4
                        , CTrace* targ5
                        , CTrace* targ6
                        , CTrace* targ7
                        , CTrace* targ8
                        , CTrace* targ9
                        , CTrace* targ10
                        , CTrace* targ11
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp11 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x0b,HatFile);
    fwrite(&(tap->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ6->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ7->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ8->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ9->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ10->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ11->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (14*sizeof(FileOffset));
    return mkTrace(fo,tfn->trust,False);
}

CTrace*
primTAp12 (CTrace* tap, CTrace* tfn
                        , CTrace* targ1
                        , CTrace* targ2
                        , CTrace* targ3
                        , CTrace* targ4
                        , CTrace* targ5
                        , CTrace* targ6
                        , CTrace* targ7
                        , CTrace* targ8
                        , CTrace* targ9
                        , CTrace* targ10
                        , CTrace* targ11
                        , CTrace* targ12
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTAp12 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x0c,HatFile);
    fwrite(&(tap->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(tfn->ptr),   sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ1->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ2->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ3->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ4->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ5->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ6->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ7->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ8->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ9->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ10->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ11->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(targ12->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (15*sizeof(FileOffset));
    return mkTrace(fo,tfn->trust,False);
}






CTrace*
primTNm (CTrace* tnm, CNmType* nm, FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTNm 0x%x 0x%x 0x%x -> 0x%x\n",tnm,nm,sr,fo);)
    fputc(((Trace<<5) | TNm),HatFile);
    fwrite(&(tnm->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(nm->ptr),  sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,         sizeof(FileOffset), 1, HatFile);
    /*fflush(HatFile);*/
    HatCounter += 1 + (3*sizeof(FileOffset));
    return mkTrace(fo,nm->trust,False);
}

CTrace*
primTInd (CTrace* t1, CTrace* t2)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTInd 0x%x 0x%x -> 0x%x\n",t1,t2,fo);)
    fputc(((Trace<<5) | TInd),HatFile);
    fwrite(&(t1->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&(t2->ptr), sizeof(FileOffset), 1, HatFile);
    HatCounter += 1 + (2*sizeof(FileOffset));
    return mkTrace(fo,t1->trust,False);
}

CTrace*
primTHidden (CTrace* t1)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTHidden 0x%x -> 0x%x\n",t1,fo);)
    if (t1->hidden) return t1; /* collapse hidden chains */
    fputc(((Trace<<5) | THidden),HatFile);
    fwrite(&(t1->ptr), sizeof(FileOffset), 1, HatFile);
    HatCounter += 1 + (sizeof(FileOffset));
    return mkTrace(fo,t1->trust,True);  /* hiddeness t1->trust = True? */
}

CTrace*
primTSatA (CTrace* t1)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTSatA 0x%x -> 0x%x\n",t1,fo);)
    fputc(((Trace<<5) | TSatA),HatFile);
    fwrite(&(t1->ptr), sizeof(FileOffset), 1, HatFile);
    HatCounter += 1 + (sizeof(FileOffset));
    return mkTrace(fo,t1->trust,False);
}


CTrace*
primTSatALonely (CTrace* t1)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimTSatALonely 0x%x -> 0x%x\n",t1,fo);)
    fputc(((Trace<<5) | TSatAL),HatFile);
    fwrite(&(t1->ptr), sizeof(FileOffset), 1, HatFile);
    HatCounter += 1 + (sizeof(FileOffset));
    return mkTrace(fo,t1->trust,False);
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

CTrace*
primTSatB (CTrace* t1)
{
    SATstackSort[SATp] = False;
    SATstack[SATp++] = t1->ptr;
    if (SATp >= spSize) {
        fprintf(stderr,"Exceeded size of SAT stack\n");
        exit(1);
    }
    return t1;
}


CTrace*
primTSatBLonely (CTrace* t1)
{
    SATstackSort[SATp] = True;
    SATstack[SATp++] = t1->ptr;
    if (SATp >= spSize) {
        fprintf(stderr,"Exceeded size of SAT stack\n");
        exit(1);
    }
    return t1;
}

/* This implementation of SatC saves up a bunch of updates to be done
 * all at once.  We make no attempt yet to place the updates into order,
 * which could potentially make the combined update even faster.
 */

CTrace*
primTSatC (CTrace* torig,CTrace* teval)
{
    if (SATstack[--SATp] != torig->ptr) {
        fprintf(stderr,"SAT stack is corrupt.\n");
        exit(1);
    } else {
	/* save updates until we have a bunch of them */
        SATqueueSort[SATq] = False;
        SATqueueA[SATq] = torig->ptr;
        SATqueueC[SATq] = teval->ptr;
        SATq++;
        if (SATq >= NUM_SATC) {
            updateSatCs();
        }
        return torig;
/*
	-- do each update individually
        fseek(HatFile,ntohl(torig->ptr),SEEK_SET);
        fputc(((Trace<<5) | TSatC),HatFile);
        fwrite(&teval, sizeof(FileOffset), 1, HatFile);
        fseek(HatFile,HatCounter,SEEK_SET);
        return torig;
*/
    }
}


CTrace*
primTSatCLonely (CTrace* torig,CTrace* teval)
{
    if (SATstack[--SATp] != torig->ptr) {
        fprintf(stderr,"SAT stack is corrupt.\n");
        exit(1);
    } else {
	/* save updates until we have a bunch of them */
        SATqueueSort[SATq] = True;
        SATqueueA[SATq] = torig->ptr;
        SATqueueC[SATq] = teval->ptr;
        SATq++;
        if (SATq >= NUM_SATC) {
            updateSatCs();
        }
        return torig;
/*
	-- do each update individually
        fseek(HatFile,ntohl(torig->ptr),SEEK_SET);
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



/* Auxiliary function to turn a simple FileOffset into a struct CNmType.
 * We store the kind of the Nm object (currently not needed),
 * and also its trustedness (needed).
 */

CNmType*
mkCNmType (int type, FileOffset fo, int trustedness)
{
    CNmType* nm;
    C_CHECK(1+EXTRA+2);
    nm = (CNmType*) C_ALLOC(1+EXTRA+2);
    nm->constr = CONSTRW(2,type);
    INIT_PROFINFO((void*)nm,&dummyProfInfo)
    nm->ptr = fo;
    nm->trust = trustedness;
    return nm;
}






CNmType*
primNTInt (int i)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTInt %d -> 0x%x\n",i,fo);)
    fputc(((NmType<<5) | NTInt),HatFile);
    i = htonl(i);
    fwrite(&i, sizeof(int), 1, HatFile);
    HatCounter += 1 + (sizeof(int));
    return mkCNmType(NTInt,fo,False);
}

CNmType*
primNTChar (char c)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTChar '%c' -> 0x%x\n",c,fo);)
    fputc(((NmType<<5) | NTChar),HatFile);
    fwrite(&c, sizeof(char), 1, HatFile);
    HatCounter += 1 + (sizeof(char));
    return mkCNmType(NTChar,fo,False);
}

CNmType*
primNTInteger (NodePtr i)
{
    FileOffset fo;  char size;  int n,count;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTInteger -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTInteger),HatFile);
#if 0
    fputc(0x00,HatFile);	/* fake all Integers as zero for now */
    HatCounter += 1 + (sizeof(char));
#else
    size = (char)CONINFO_LARGESIZEU(i[0]);
    HIDE(fprintf(stderr,"primNTInteger size=%d ",size);)
    HIDE(if (size==1) fprintf(stderr,"value=%d\n",i[1]); \
         else         fprintf(stderr,"value=0\n");)
    if(CONINFO_LARGEEXTRA(i[0]))
         fputc(-size,HatFile);
    else fputc(size,HatFile);
    for (count=1;count<=size;count++) {
      n = htonl(i[count]);
      fwrite(&n, sizeof(long), 1, HatFile);
    }
    HatCounter += 1+(sizeof(char))+(size*sizeof(int));
#endif
    return mkCNmType(NTInteger,fo,False);
}

CNmType*
primNTRational (NodePtr i, NodePtr j)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTRational -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTRational),HatFile);
    fputc(0x00,HatFile);	/* fake all Integers as zero for now */
    fputc(0x00,HatFile);	/* fake all Integers as zero for now */
    HatCounter += 1 + (2*sizeof(char));
    return mkCNmType(NTRational,fo,False);
}

CNmType*
primNTFloat (float f)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTFloat -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTFloat),HatFile);
    fwrite(&f, sizeof(float), 1, HatFile);	/* ignore endian problems */
    HatCounter += 1 + (sizeof(float));
    return mkCNmType(NTFloat,fo,False);
}

CNmType*
primNTDouble (double d)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTDouble -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTDouble),HatFile);
    fwrite(&d, sizeof(double), 1, HatFile);	/* ignore endian problems */
    HatCounter += 1 + (sizeof(double));
    return mkCNmType(NTDouble,fo,False);
}

CNmType*
primNTId (IdEntry *id)
{
    if (id->fileoffset) {
        HIDE(fprintf(stderr,"\tprimNTId \"%s\" -> (cached)\n",id->name);)
        return mkCNmType(NTId,id->fileoffset,id->srcmod->trusted);
    } else {
        FileOffset fo;
        int i;
        if (!(id->srcmod->fileoffset)) (void)primModInfo(id->srcmod);
        fo = htonl(HatCounter);
        HIDE(fprintf(stderr,"\tprimNTId \"%s\" -> 0x%x\n",id->name,fo);)
        fputc(((NmType<<5) | NTId),HatFile);
        fprintf(HatFile,"%s",id->name);
        fputc(0x0,HatFile);
        i = 0;
        fwrite(&(id->srcmod->fileoffset), sizeof(FileOffset), 1, HatFile);
        fputc(id->pri,HatFile);
        i = htonl(id->srcpos);
        fwrite(&i, sizeof(int), 1, HatFile);
        id->fileoffset = fo;
        HatCounter = ftell(HatFile);
        return mkCNmType(NTId,fo,id->srcmod->trusted);
    }
}

CNmType*
primNTConstr (IdEntry *id)
{
    if (id->fileoffset) {
        HIDE(fprintf(stderr,"\tprimNTConstr \"%s\" -> (cached)\n",id->name);)
	return mkCNmType(NTConstr,id->fileoffset,False); /* id->srcmod->trusted */
    } else {
        FileOffset fo;
        int i;
        if (!(id->srcmod->fileoffset)) (void)primModInfo(id->srcmod);
        fo = htonl(HatCounter);
        HIDE(fprintf(stderr,"\tprimNTConstr \"%s\" -> 0x%x\n",id->name,fo);)
        fputc(((NmType<<5) | NTConstr),HatFile);
        fprintf(HatFile,"%s",id->name);
        fputc(0x0,HatFile);
        i = 0;
        fwrite(&(id->srcmod->fileoffset), sizeof(FileOffset), 1, HatFile);
        fputc(id->pri,HatFile);
        i = htonl(id->srcpos);
        fwrite(&i, sizeof(int), 1, HatFile);
        id->fileoffset = fo;
        HatCounter = ftell(HatFile);
        return mkCNmType(NTConstr,fo,False);  /* id->srcmod->trusted);  */
    }
}

CNmType*
primNTTuple ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTTuple -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTTuple),HatFile);
    HatCounter += 1;
    return mkCNmType(NTTuple,fo,False);
}

CNmType*
primNTFun ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTFun -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTFun),HatFile);
    HatCounter += 1;
    return mkCNmType(NTFun,fo,False);
}

CNmType*
primNTCase ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTCase -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTCase),HatFile);
    HatCounter += 1;
    return mkCNmType(NTCase,fo,False);
}

CNmType*
primNTLambda ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTLambda -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTLambda),HatFile);
    HatCounter += 1;
    return mkCNmType(NTLambda,fo,False);
}

CNmType*
primNTDummy ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTDummy -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTDummy),HatFile);
    HatCounter += 1;
    return mkCNmType(NTDummy,fo,False);
}

CNmType*
primNTCString (char *s)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTCString \"%s\" -> 0x%x\n",s,fo);)
    fputc(((NmType<<5) | NTCString),HatFile);
    fprintf(HatFile,"%s",s);
    fputc(0x0,HatFile);
    HatCounter = ftell(HatFile);
    return mkCNmType(NTCString,fo,False);
}

CNmType*
primNTIf ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTIf -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTIf),HatFile);
    HatCounter += 1;
    return mkCNmType(NTIf,fo,False);
}

CNmType*
primNTGuard ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTGuard -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTGuard),HatFile);
    HatCounter += 1;
    return mkCNmType(NTGuard,fo,False);
}

CNmType*
primNTContainer ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    HIDE(fprintf(stderr,"\tprimNTContainer -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTContainer),HatFile);
    HatCounter += 1;
    return mkCNmType(NTContainer,fo,False);
}


int
primTrustedNm (CNmType* nm)
{
    HIDE(fprintf(stderr,"\tprimTrustedNm %s\n",(nm->trust?"trust":"suspect"));)
    return nm->trust;
}

int
primSameTrace (CTrace* t1, CTrace* t2)
{
    HIDE(fprintf(stderr,"\tprimSameTrace (%s)\n",(t1==t2 ?"yes":"no"));)
    return ((t1->ptr) == (t2->ptr));
}


FileOffset
primSR0 ()
{
    HIDE(fprintf(stderr,"\tprimSR0\n");)
    return (FileOffset)0;
}

FileOffset
primSR3 (SrcRef *sr)
{
    if (sr->fileoffset) {
        HIDE(fprintf(stderr,"\tprimSR3 -> (cached)\n");)
        return sr->fileoffset;
    } else {
        FileOffset fo;
        int i = 0;
        if (!(sr->modinfo->fileoffset)) (void)primModInfo(sr->modinfo);
        fo = htonl(HatCounter);
        HIDE(fprintf(stderr,"\tprimSR3 -> 0x%x\n",fo);)
        fputc((SR<<5),HatFile);
        if (sr->modinfo)
          fwrite(&(sr->modinfo->fileoffset), sizeof(FileOffset), 1, HatFile);
        else
          fwrite(&i                        , sizeof(FileOffset), 1, HatFile);
        i = htonl(sr->posn);
        fwrite(&i, sizeof(int), 1, HatFile);
        sr->fileoffset = fo;
        HatCounter += 1 + sizeof(FileOffset) + sizeof(int);
        return fo;
    }
}


FileOffset
primTracePtr (CTrace* t)
{
    fprintf(stderr,"\tprimTracePtr 0x%x -> 0x%x\n",t,t->ptr);
    return t->ptr;
}

int
primTrustedFun (CTrace* t)
{
    HIDE(fprintf(stderr,"\tprimTrustedFun 0x%x -> %s\n",t,(t->trust?"yes":"no"));)
    return t->trust;
}

int
primHidden (CTrace* t)
{
    HIDE(fprintf(stderr,"\tprimHidden 0x%x -> %s\n",t,(t->trust?"yes":"no"));)
    return t->hidden;
}

