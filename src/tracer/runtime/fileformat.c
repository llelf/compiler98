#include <stdio.h>
#include "fileformat.h"
#include "cinterface.h"

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
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimModInfo %s (%s) -> 0x%x\n",m->modname,m->srcfile,fo);
    fprintf(HatFile,"%c%s%c%s%c", (m->trusted ? 0x21 : 0x20)
                    ,m->modname, 0x0, m->srcfile, 0x0);
    m->fileoffset = fo;
    return fo;
}

FileOffset
primTRoot (void)
{
    fprintf(stderr,"primTRoot\n");
    return (FileOffset)0;
}

FileOffset
primTAp1 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset sr)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimTAp1 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x01,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    return fo;
}

FileOffset
primTAp2 (FileOffset tap, FileOffset tfn
                        , FileOffset targ1
                        , FileOffset targ2
                        , FileOffset sr)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimTAp2 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x02,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
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
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimTAp3 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x03,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ3, sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
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
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimTAp4 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);
    fputc(((Trace<<5) | TAp),HatFile);
    fputc(0x04,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ3, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ4, sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
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
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimTAp5 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);
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
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimTAp6 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);
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
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimTAp7 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);
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
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimTAp8 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);
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
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimTAp9 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);
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
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimTAp10 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);
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
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimTAp11 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);
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
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimTAp12 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",tap,tfn,targ1,sr,fo);
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
    return fo;
}






FileOffset
primTNm (FileOffset tnm, CNmType nm, FileOffset sr)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimTNm 0x%x 0x%x 0x%x -> 0x%x\n",tnm,nm,sr,fo);
    fputc(((Trace<<5) | TNm),HatFile);
    fwrite(&tnm,       sizeof(FileOffset), 1, HatFile);
    fwrite(&(nm->ptr), sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,        sizeof(FileOffset), 1, HatFile);
    fflush(HatFile);
    return fo;
}

FileOffset
primTInd (FileOffset t1, FileOffset t2)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimTInd 0x%x 0x%x -> 0x%x\n",t1,t2,fo);
    fputc(((Trace<<5) | TInd),HatFile);
    fwrite(&t1, sizeof(FileOffset), 1, HatFile);
    fwrite(&t2, sizeof(FileOffset), 1, HatFile);
    return fo;
}

FileOffset
primTHidden (FileOffset t1)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimTHidden 0x%x -> 0x%x\n",t1,fo);
    fputc(((Trace<<5) | THidden),HatFile);
    fwrite(&t1, sizeof(FileOffset), 1, HatFile);
    return fo;
}

FileOffset
primTSatA (FileOffset t1)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimTSatA 0x%x -> 0x%x\n",t1,fo);
    fputc(((Trace<<5) | TSatA),HatFile);
    fwrite(&t1, sizeof(FileOffset), 1, HatFile);
    return fo;
}


/* This implementation of SatB is wrong - it creates a new node rather than
 * overwriting the old one.
 */

FileOffset
primTSatB (FileOffset t1)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimTSatB 0x%x -> 0x%x\n",t1,fo);
    fputc(((Trace<<5) | TSatB),HatFile);
    fwrite(&t1, sizeof(FileOffset), 1, HatFile);
    return fo;
}

/* This implementation of SatC is wrong - it creates a new node rather than
 * overwriting the old one.
 */

FileOffset
primTSatC (FileOffset torig,FileOffset teval)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimTSatC 0x%x 0x%x -> 0x%x\n",torig,teval,fo);
    fputc(((Trace<<5) | TSatC),HatFile);
    fwrite(&teval, sizeof(FileOffset), 1, HatFile);
    return fo;
}



/* Auxiliary function to turn a simple FileOffset into a struct CNmType.
 * We store the kind of the Nm object (currently not needed),
 * and also its trustedness (needed).
 */

CNmType
mkCNmType (int type, FileOffset fo, int trustedness)
{
    CNmType nm;
    nm = (CNmType) C_ALLOC(1+EXTRA+2);
    nm->constr = CONSTR(type,2,2);
    INIT_PROFINFO((void*)nm,&dummyProfInfo)
    nm->ptr = fo;
    nm->trust = trustedness;
    return nm;
}






CNmType
primNTInt (int i)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimNTInt %d -> 0x%x\n",i,fo);
    fputc(((NmType<<5) | NTInt),HatFile);
    i = htonl(i);
    fwrite(&i, sizeof(int), 1, HatFile);
    return mkCNmType(NTInt,fo,False);
}

CNmType
primNTChar (char c)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimNTChar '%c' -> 0x%x\n",c,fo);
    fputc(((NmType<<5) | NTChar),HatFile);
    fwrite(&c, sizeof(char), 1, HatFile);
    return mkCNmType(NTChar,fo,False);
}

CNmType
primNTInteger (NodePtr i)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimNTInteger -> 0x%x\n",fo);
    fputc(((NmType<<5) | NTInteger),HatFile);
    fputc(0x00,HatFile);	/* fake all Integers as zero for now */
    return mkCNmType(NTInteger,fo,False);
}

CNmType
primNTRational (NodePtr i, NodePtr j)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimNTRational -> 0x%x\n",fo);
    fputc(((NmType<<5) | NTRational),HatFile);
    fputc(0x00,HatFile);	/* fake all Integers as zero for now */
    fputc(0x00,HatFile);	/* fake all Integers as zero for now */
    return mkCNmType(NTRational,fo,False);
}

CNmType
primNTFloat (float f)
{
    FileOffset fo;
    CNmType nm;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimNTFloat -> 0x%x\n",fo);
    fputc(((NmType<<5) | NTFloat),HatFile);
    fwrite(&f, sizeof(float), 1, HatFile);	/* ignore endian problems */
    return mkCNmType(NTFloat,fo,False);
}

CNmType
primNTDouble (double d)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimNTDouble -> 0x%x\n",fo);
    fputc(((NmType<<5) | NTDouble),HatFile);
    fwrite(&d, sizeof(double), 1, HatFile);	/* ignore endian problems */
    return mkCNmType(NTDouble,fo,False);
}

CNmType
primNTId (IdEntry *id)
{
    if (id->fileoffset) {
        fprintf(stderr,"\tprimNTId \"%s\" -> (cached)\n",id->name);
        return mkCNmType(NTId,id->fileoffset,id->srcmod->trusted);
    } else {
        FileOffset fo;
        int i;
        if (!(id->srcmod->fileoffset)) (void)primModInfo(id->srcmod);
        fo = ftell(HatFile);
        fo = htonl(fo);
        fprintf(stderr,"\tprimNTId \"%s\" -> 0x%x\n",id->name,fo);
        fputc(((NmType<<5) | NTId),HatFile);
        fprintf(HatFile,"%s",id->name);
        fputc(0x0,HatFile);
        i = 0;
        fwrite(&(id->srcmod->fileoffset), sizeof(FileOffset), 1, HatFile);
        fputc(id->pri,HatFile);
        i = htonl(id->srcpos);
        fwrite(&i, sizeof(int), 1, HatFile);
        id->fileoffset = fo;
        return mkCNmType(NTId,fo,id->srcmod->trusted);
    }
}

/* probably never used? */
CNmType
primNTConstr (IdEntry *id)
{
    if (id->fileoffset) {
        fprintf(stderr,"\tprimNTConstr \"%s\" -> (cached)\n",id->name);
        return mkCNmType(NTConstr,id->fileoffset,id->srcmod->trusted);
    } else {
        FileOffset fo;
        int i;
        if (!(id->srcmod->fileoffset)) (void)primModInfo(id->srcmod);
        fo = ftell(HatFile);
        fo = htonl(fo);
        fprintf(stderr,"\tprimNTConstr \"%s\" -> 0x%x\n",id->name,fo);
        fputc(((NmType<<5) | NTConstr),HatFile);
        fprintf(HatFile,"%s",id->name);
        fputc(0x0,HatFile);
        i = 0;
        fwrite(&(id->srcmod->fileoffset), sizeof(FileOffset), 1, HatFile);
        fputc(id->pri,HatFile);
        i = htonl(id->srcpos);
        fwrite(&i, sizeof(int), 1, HatFile);
        id->fileoffset = fo;
        return mkCNmType(NTConstr,fo,id->srcmod->trusted);
    }
}

CNmType
primNTTuple ()
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimNTTuple -> 0x%x\n",fo);
    fputc(((NmType<<5) | NTTuple),HatFile);
    return mkCNmType(NTTuple,fo,False);
}

CNmType
primNTFun ()
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimNTFun -> 0x%x\n",fo);
    fputc(((NmType<<5) | NTFun),HatFile);
    return mkCNmType(NTFun,fo,False);
}

CNmType
primNTCase ()
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimNTCase -> 0x%x\n",fo);
    fputc(((NmType<<5) | NTCase),HatFile);
    return mkCNmType(NTCase,fo,False);
}

CNmType
primNTLambda ()
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimNTLambda -> 0x%x\n",fo);
    fputc(((NmType<<5) | NTLambda),HatFile);
    return mkCNmType(NTLambda,fo,False);
}

CNmType
primNTDummy ()
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimNTDummy -> 0x%x\n",fo);
    fputc(((NmType<<5) | NTDummy),HatFile);
    return mkCNmType(NTDummy,fo,False);
}

CNmType
primNTCString (char *s)
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimNTCString \"%s\" -> 0x%x\n",s,fo);
    fputc(((NmType<<5) | NTCString),HatFile);
    fprintf(HatFile,"%s",s);
    fputc(0x0,HatFile);
    return mkCNmType(NTCString,fo,False);
}

CNmType
primNTIf ()
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimNTIf -> 0x%x\n",fo);
    fputc(((NmType<<5) | NTIf),HatFile);
    return mkCNmType(NTIf,fo,False);
}

CNmType
primNTGuard ()
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimNTGuard -> 0x%x\n",fo);
    fputc(((NmType<<5) | NTGuard),HatFile);
    return mkCNmType(NTGuard,fo,False);
}

CNmType
primNTContainer ()
{
    FileOffset fo;
    fo = ftell(HatFile);
    fo = htonl(fo);
    fprintf(stderr,"\tprimNTContainer -> 0x%x\n",fo);
    fputc(((NmType<<5) | NTContainer),HatFile);
    return mkCNmType(NTContainer,fo,False);
}


int
primTrustedNm (CNmType nm)
{
    fprintf(stderr,"\tprimTrustedNm %s\n",(nm->trust?"trust":"suspect"));
    return nm->trust;
}

int
primSameTrace (FileOffset t1, FileOffset t2)
{
    fprintf(stderr,"\tprimSameTrace (%s)\n",(t1==t2 ?"yes":"no"));
    return (t1==t2);
}


FileOffset
primSR0 ()
{
    fprintf(stderr,"\tprimSR0\n");
    return (FileOffset)0;
}

FileOffset
primSR3 (SrcRef *sr)
{
    if (sr->fileoffset) {
        fprintf(stderr,"\tprimSR3 -> (cached)\n");
        return sr->fileoffset;
    } else {
        FileOffset fo;
        int i = 0;
        fo = ftell(HatFile);
        fo = htonl(fo);
        fprintf(stderr,"\tprimSR3 -> 0x%x\n",fo);
        fputc((SR<<5),HatFile);
        fwrite(&(sr->modinfo->fileoffset), sizeof(FileOffset), 1, HatFile);
        i = htonl(sr->posn);
        fwrite(&i, sizeof(int), 1, HatFile);
        sr->fileoffset = fo;
        return fo;
    }
}

NodePtr
primShowTrace (NodePtr n)
{
    fprintf(stderr,"\t Trace %d size=%d\n",CONINFO_NUMBER(*n),CONINFO_SIZE(*n));
    fprintf(stderr,"\t       arg1=0x%x\n",GET_POINTER_ARG1(n,1));
    fprintf(stderr,"\t       arg2=0x%x\n",GET_POINTER_ARG1(n,2));
    fprintf(stderr,"\t       arg3=0x%x\n",GET_POINTER_ARG1(n,3));
    return n;
}

/* Function to build a CTrace triple from components.
 */

CTrace
mkTrace (FileOffset fo, int trustedness, int hiddenness)
{
    CTrace t;
    t = (CTrace) C_ALLOC(1+EXTRA+3);
    t->constr = CONSTR(0,3,3);
    INIT_PROFINFO((void*)t,&dummyProfInfo)
    t->ptr = fo;
    t->trust = trustedness;
    t->hidden = hiddenness;
    fprintf(stderr,"\tmkTrace 0x%x %s %s -> 0x%x\n",fo
                                                   ,(t->trust?"True":"False")
                                                   ,(t->hidden?"True":"False")
                                                   ,t);
    return t;
}

FileOffset
primTracePtr (CTrace t)
{
    fprintf(stderr,"\tprimTracePtr 0x%x -> 0x%x\n",t,t->ptr);
    return t->ptr;
}

int
primTrustedFun (CTrace t)
{
    fprintf(stderr,"\tprimTrustedFun 0x%x -> %s\n",t,(t->trust?"yes":"no"));
    return t->trust;
}

int
primHidden (CTrace t)
{
    fprintf(stderr,"\tprimHidden 0x%x -> %s\n",t,(t->trust?"yes":"no"));
    return t->hidden;
}
