#include <stdio.h>
#include "fileformat.h"
#include "cinterface.h"

#if defined(DEBUG)
#define SHOW(x) x
#else
#define SHOW(x)
#endif

#define	NUM_SATB	8192
#define	NUM_SATC	32000

static FileOffset HatCounter = 8 + 2*sizeof(FileOffset);
static FileOffset SATstack[NUM_SATB];
static int SATp = 0;
static FileOffset SATqueueA[NUM_SATC];
static FileOffset SATqueueC[NUM_SATC];
static int SATq = 0;


/* Remaining problems include (at least) the following:
 *   . All Integer values are faked to zero for now.
 *   . Hence all Rational values are also dummy.
 *   . Floats and Doubles are written to the file without regard for endianness.
 *   . SATs are never overwritten - the B and C variants create new things
 *     in the file.
 */

FileOffset
primModInfo (ModInfo *m)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    SHOW(fprintf(stderr,"\tprimModInfo %s (%s) -> 0x%x\n",m->modname,m->srcfile,fo);)
    fprintf(HatFile,"%c%s%c%s%c", (m->trusted ? 0x21 : 0x20)
                    ,m->modname, 0x0, m->srcfile, 0x0);
    m->fileoffset = fo;
    HatCounter = ftell(HatFile);
    return fo;
}

FileOffset
primTRoot (void)
{
    SHOW(fprintf(stderr,"primTRoot\n");)
    return (FileOffset)0;
}

FileOffset
primTAp1 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    SHOW(fprintf(stderr,"\tprimTAp1 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x01,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
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
    SHOW(fprintf(stderr,"\tprimTAp2 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x02,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
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
    SHOW(fprintf(stderr,"\tprimTAp3 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x03,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ3, sizeof(FileOffset), 1, HatFile);
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
    SHOW(fprintf(stderr,"\tprimTAp4 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x04,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ3, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ4, sizeof(FileOffset), 1, HatFile);
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
    SHOW(fprintf(stderr,"\tprimTAp5 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x05,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ3, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ4, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ5, sizeof(FileOffset), 1, HatFile);
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
    SHOW(fprintf(stderr,"\tprimTAp6 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x05,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ3, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ4, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ5, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ6, sizeof(FileOffset), 1, HatFile);
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
    SHOW(fprintf(stderr,"\tprimTAp7 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x05,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ3, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ4, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ5, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ6, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ7, sizeof(FileOffset), 1, HatFile);
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
    SHOW(fprintf(stderr,"\tprimTAp8 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x05,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ3, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ4, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ5, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ6, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ7, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ8, sizeof(FileOffset), 1, HatFile);
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
    SHOW(fprintf(stderr,"\tprimTAp9 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x05,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ3, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ4, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ5, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ6, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ7, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ8, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ9, sizeof(FileOffset), 1, HatFile);
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
    SHOW(fprintf(stderr,"\tprimTAp10 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x05,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ3, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ4, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ5, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ6, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ7, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ8, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ9, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ10, sizeof(FileOffset), 1, HatFile);
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
    SHOW(fprintf(stderr,"\tprimTAp11 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x05,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ3, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ4, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ5, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ6, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ7, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ8, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ9, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ10, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ11, sizeof(FileOffset), 1, HatFile);
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
    SHOW(fprintf(stderr,"\tprimTAp12 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);)
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x05,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ3, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ4, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ5, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ6, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ7, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ8, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ9, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ10, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ11, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ12, sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    HatCounter += 2 + (15*sizeof(FileOffset));
    return fo;
}






FileOffset
primTNm (FileOffset tnm, CNmType* nm, FileOffset sr)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    SHOW(fprintf(stderr,"\tprimTNm 0x%x 0x%x 0x%x -> 0x%x\n",tnm,nm,sr,fo);)
    fputc(((Trace<<5) | TNm),HatFile);
    fwrite(&tnm,       sizeof(FileOffset), 1, HatFile);
    fwrite(&(nm->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,        sizeof(FileOffset), 1, HatFile);
    /*fflush(HatFile);*/
    HatCounter += 1 + (3*sizeof(FileOffset));
    return fo;
}

FileOffset
primTInd (FileOffset t1, FileOffset t2)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    SHOW(fprintf(stderr,"\tprimTInd 0x%x 0x%x -> 0x%x\n",t1,t2,fo);)
    fputc(((Trace<<5) | TInd),HatFile);
    fwrite(&t1, sizeof(FileOffset), 1, HatFile);
    fwrite(&t2, sizeof(FileOffset), 1, HatFile);
    HatCounter += 1 + (2*sizeof(FileOffset));
    return fo;
}

FileOffset
primTHidden (FileOffset t1)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    SHOW(fprintf(stderr,"\tprimTHidden 0x%x -> 0x%x\n",t1,fo);)
    fputc(((Trace<<5) | THidden),HatFile);
    fwrite(&t1, sizeof(FileOffset), 1, HatFile);
    HatCounter += 1 + (sizeof(FileOffset));
    return fo;
}

FileOffset
primTSatA (FileOffset t1)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    SHOW(fprintf(stderr,"\tprimTSatA 0x%x -> 0x%x\n",t1,fo);)
    fputc(((Trace<<5) | TSatA),HatFile);
    fwrite(&t1, sizeof(FileOffset), 1, HatFile);
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
    SATstack[SATp++] = t1;
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
    if (SATstack[--SATp] != torig) {
        fprintf(stderr,"SAT stack is corrupt.\n");
        exit(1);
    } else {
	/* save updates until we have a bunch of them */
        SATqueueA[SATq] = torig;
        SATqueueC[SATq] = teval;
        SATq++;
        if (SATq >= NUM_SATC) {
            updateSatCs();
        }
/*
	-- do each update individually
        fseek(HatFile,ntohl(torig),SEEK_SET);
        fputc(((Trace<<5) | TSatC),HatFile);
        fwrite(&teval, sizeof(FileOffset), 1, HatFile);
        fseek(HatFile,HatCounter,SEEK_SET);
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
    SHOW(fprintf(stderr,"\tupdateSatCs (%d SatCs) (%d SatBs)\n",SATq,SATp);)
    for (i=0; i<SATq; i++) {
        fseek(HatFile,ntohl(SATqueueA[i]),SEEK_SET);
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
    SHOW(fprintf(stderr,"\tupdateSatBs (%d SatBs)\n",SATp);)
    for (i=0; i<SATp; i++) {
        fseek(HatFile,ntohl(SATstack[i]),SEEK_SET);
        fputc(((Trace<<5) | TSatB),HatFile);
    }
    fseek(HatFile,HatCounter,SEEK_SET);
}



/* Auxiliary function to turn a simple FileOffset into a struct CNmType.
 * We store the kind of the Nm object (currently not needed),
 * and also its trustedness (needed).
 */

CNmType*
mkCNmType (int type, FileOffset fo, int trustedness)
{
    CNmType* nm;
    nm = (CNmType*) C_ALLOC(1+EXTRA+2);
    nm->constr = CONSTR(type,2,2);
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
    SHOW(fprintf(stderr,"\tprimNTInt %d -> 0x%x\n",i,fo);)
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
    SHOW(fprintf(stderr,"\tprimNTChar '%c' -> 0x%x\n",c,fo);)
    fputc(((NmType<<5) | NTChar),HatFile);
    fwrite(&c, sizeof(char), 1, HatFile);
    HatCounter += 1 + (sizeof(char));
    return mkCNmType(NTChar,fo,False);
}

CNmType*
primNTInteger (NodePtr i)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    SHOW(fprintf(stderr,"\tprimNTInteger -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTInteger),HatFile);
    fputc(0x00,HatFile);	/* fake all Integers as zero for now */
    HatCounter += 1 + (sizeof(char));
    return mkCNmType(NTInteger,fo,False);
}

CNmType*
primNTRational (NodePtr i, NodePtr j)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    SHOW(fprintf(stderr,"\tprimNTRational -> 0x%x\n",fo);)
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
    SHOW(fprintf(stderr,"\tprimNTFloat -> 0x%x\n",fo);)
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
    SHOW(fprintf(stderr,"\tprimNTDouble -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTDouble),HatFile);
    fwrite(&d, sizeof(double), 1, HatFile);	/* ignore endian problems */
    HatCounter += 1 + (sizeof(double));
    return mkCNmType(NTDouble,fo,False);
}

CNmType*
primNTId (IdEntry *id)
{
    if (id->fileoffset) {
        SHOW(fprintf(stderr,"\tprimNTId \"%s\" -> (cached)\n",id->name);)
        return mkCNmType(NTId,id->fileoffset,id->srcmod->trusted);
    } else {
        FileOffset fo;
        int i;
        if (!(id->srcmod->fileoffset)) (void)primModInfo(id->srcmod);
        fo = htonl(HatCounter);
        SHOW(fprintf(stderr,"\tprimNTId \"%s\" -> 0x%x\n",id->name,fo);)
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
        SHOW(fprintf(stderr,"\tprimNTConstr \"%s\" -> (cached)\n",id->name);)
        return mkCNmType(NTConstr,id->fileoffset,id->srcmod->trusted);
    } else {
        FileOffset fo;
        int i;
        if (!(id->srcmod->fileoffset)) (void)primModInfo(id->srcmod);
        fo = htonl(HatCounter);
        SHOW(fprintf(stderr,"\tprimNTConstr \"%s\" -> 0x%x\n",id->name,fo);)
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
        return mkCNmType(NTConstr,fo,id->srcmod->trusted);
    }
}

CNmType*
primNTTuple ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    SHOW(fprintf(stderr,"\tprimNTTuple -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTTuple),HatFile);
    HatCounter += 1;
    return mkCNmType(NTTuple,fo,False);
}

CNmType*
primNTFun ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    SHOW(fprintf(stderr,"\tprimNTFun -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTFun),HatFile);
    HatCounter += 1;
    return mkCNmType(NTFun,fo,False);
}

CNmType*
primNTCase ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    SHOW(fprintf(stderr,"\tprimNTCase -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTCase),HatFile);
    HatCounter += 1;
    return mkCNmType(NTCase,fo,False);
}

CNmType*
primNTLambda ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    SHOW(fprintf(stderr,"\tprimNTLambda -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTLambda),HatFile);
    HatCounter += 1;
    return mkCNmType(NTLambda,fo,False);
}

CNmType*
primNTDummy ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    SHOW(fprintf(stderr,"\tprimNTDummy -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTDummy),HatFile);
    HatCounter += 1;
    return mkCNmType(NTDummy,fo,False);
}

CNmType*
primNTCString (char *s)
{
    FileOffset fo;
    fo = htonl(HatCounter);
    SHOW(fprintf(stderr,"\tprimNTCString \"%s\" -> 0x%x\n",s,fo);)
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
    SHOW(fprintf(stderr,"\tprimNTIf -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTIf),HatFile);
    HatCounter += 1;
    return mkCNmType(NTIf,fo,False);
}

CNmType*
primNTGuard ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    SHOW(fprintf(stderr,"\tprimNTGuard -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTGuard),HatFile);
    HatCounter += 1;
    return mkCNmType(NTGuard,fo,False);
}

CNmType*
primNTContainer ()
{
    FileOffset fo;
    fo = htonl(HatCounter);
    SHOW(fprintf(stderr,"\tprimNTContainer -> 0x%x\n",fo);)
    fputc(((NmType<<5) | NTContainer),HatFile);
    HatCounter += 1;
    return mkCNmType(NTContainer,fo,False);
}


int
primTrustedNm (CNmType* nm)
{
    SHOW(fprintf(stderr,"\tprimTrustedNm %s\n",(nm->trust?"trust":"suspect"));)
    return nm->trust;
}

int
primSameTrace (FileOffset t1, FileOffset t2)
{
    SHOW(fprintf(stderr,"\tprimSameTrace (%s)\n",(t1==t2 ?"yes":"no"));)
    return (t1==t2);
}


FileOffset
primSR0 ()
{
    SHOW(fprintf(stderr,"\tprimSR0\n");)
    return (FileOffset)0;
}

FileOffset
primSR3 (SrcRef *sr)
{
    if (sr->fileoffset) {
        SHOW(fprintf(stderr,"\tprimSR3 -> (cached)\n");)
        return sr->fileoffset;
    } else {
        FileOffset fo;
        int i = 0;
        fo = htonl(HatCounter);
        SHOW(fprintf(stderr,"\tprimSR3 -> 0x%x\n",fo);)
        fputc((SR<<5),HatFile);
        fwrite(&(sr->modinfo->fileoffset), sizeof(FileOffset), 1, HatFile);
        i = htonl(sr->posn);
        fwrite(&i, sizeof(int), 1, HatFile);
        sr->fileoffset = fo;
        HatCounter += 1 + sizeof(FileOffset) + sizeof(int);
        return fo;
    }
}


/* Function to build a CTrace triple from components.
 */

CTrace*
mkTrace (FileOffset fo, int trustedness, int hiddenness)
{
    CTrace* t;
    t = (CTrace*) C_ALLOC(1+EXTRA+3);
    t->constr = CONSTR(0,3,3);
    INIT_PROFINFO((void*)t,&dummyProfInfo)
    t->ptr = fo;
    t->trust = trustedness;
    t->hidden = hiddenness;
    SHOW(fprintf(stderr,"\tmkTrace 0x%x %s %s -> 0x%x\n",fo
                                                   ,(t->trust?"True":"False")
                                                   ,(t->hidden?"True":"False")
                                                   ,t);)
    return t;
}

FileOffset
primTracePtr (CTrace* t)
{
    SHOW(fprintf(stderr,"\tprimTracePtr 0x%x -> 0x%x\n",t,t->ptr);)
    return t->ptr;
}

int
primTrustedFun (CTrace* t)
{
    SHOW(fprintf(stderr,"\tprimTrustedFun 0x%x -> %s\n",t,(t->trust?"yes":"no"));)
    return t->trust;
}

int
primHidden (CTrace* t)
{
    SHOW(fprintf(stderr,"\tprimHidden 0x%x -> %s\n",t,(t->trust?"yes":"no"));)
    return t->hidden;
}

