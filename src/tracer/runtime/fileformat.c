#include "fileformat.h"

FileOffset
mkTAp1 (FileOffset tap, FileOffset tfn
                      , FileOffset targ1
                      , FileOffset sr)
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((Trace<<5) | TAp)),HatFile);
    fputc(0x01,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    return fo;
}

FileOffset
mkTAp2 (FileOffset tap, FileOffset tfn
                      , FileOffset targ1
                      , FileOffset targ2
                      , FileOffset sr)
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((Trace<<5) | TAp)),HatFile);
    fputc(0x02,HatFile);
    fwrite(&tap,   sizeof(FileOffset), 1, HatFile);
    fwrite(&tfn,   sizeof(FileOffset), 1, HatFile);
    fwrite(&targ1, sizeof(FileOffset), 1, HatFile);
    fwrite(&targ2, sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,    sizeof(FileOffset), 1, HatFile);
    return fo;
}

FileOffset
mkTAp3 (FileOffset tap, FileOffset tfn
                      , FileOffset targ1
                      , FileOffset targ2
                      , FileOffset targ3
                      , FileOffset sr)
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((Trace<<5) | TAp)),HatFile);
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
mkTAp4 (FileOffset tap, FileOffset tfn
                      , FileOffset targ1
                      , FileOffset targ2
                      , FileOffset targ3
                      , FileOffset targ4
                      , FileOffset sr)
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((Trace<<5) | TAp)),HatFile);
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
mkTAp5 (FileOffset tap, FileOffset tfn
                      , FileOffset targ1
                      , FileOffset targ2
                      , FileOffset targ3
                      , FileOffset targ4
                      , FileOffset targ5
                      , FileOffset sr)
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((Trace<<5) | TAp)),HatFile);
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
mkTNm (FileOffset tnm, FileOffset nm, FileOffset sr)
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((Trace<<5) | TNm)),HatFile);
    fwrite(&tnm, sizeof(FileOffset), 1, HatFile);
    fwrite(&nm,  sizeof(FileOffset), 1, HatFile);
    fwrite(&sr,  sizeof(FileOffset), 1, HatFile);
    return fo;
}

FileOffset
mkTInd (FileOffset t1, FileOffset t2)
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((Trace<<5) | TInd)),HatFile);
    fwrite(&t1,   sizeof(FileOffset), 1, HatFile);
    fwrite(&t2,   sizeof(FileOffset), 1, HatFile);
    return fo;
}

FileOffset
mkTHidden (FileOffset t1)
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((Trace<<5) | THidden)),HatFile);
    fwrite(&t1,   sizeof(FileOffset), 1, HatFile);
    return fo;
}

FileOffset
mkTSatA (FileOffset t1)
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((Trace<<5) | TSatA)),HatFile);
    fwrite(&t1,   sizeof(FileOffset), 1, HatFile);
    return fo;
}

FileOffset
mkTSatB (FileOffset t1)
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((Trace<<5) | TSatB)),HatFile);
    fwrite(&t1,   sizeof(FileOffset), 1, HatFile);
    return fo;
}

FileOffset
mkTSatC (FileOffset t1)
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((Trace<<5) | TSatC)),HatFile);
    fwrite(&t1,   sizeof(FileOffset), 1, HatFile);
    return fo;
}







FileOffset
mkNTInt (int i)
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTInt)),HatFile);
    i = htonl(i);
    fwrite(&i,   sizeof(int), 1, HatFile);
    return fo;
}

FileOffset
mkNTChar (char c)
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTChar)),HatFile);
    fwrite(&c,   sizeof(char), 1, HatFile);
    return fo;
}

FileOffset
mkNTInteger (Nodeptr i)
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTInteger)),HatFile);
    fputc(0x00,HatFile);	/* fake all Integers as zero for now */
    return fo;
}

FileOffset
mkNTRational (Nodeptr i, Nodeptr j)
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTRational)),HatFile);
    fputc(0x00,HatFile);	/* fake all Integers as zero for now */
    fputc(0x00,HatFile);	/* fake all Integers as zero for now */
    return fo;
}

FileOffset
mkNTFloat (float f)
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTFloat)),HatFile);
    fwrite(&f,   sizeof(float), 1, HatFile);	/* ignore endian problems */
    return fo;
}

FileOffset
mkNTDouble (double d)
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTDouble)),HatFile);
    fwrite(&d,   sizeof(double), 1, HatFile);	/* ignore endian problems */
    return fo;
}

FileOffset
mkNTId (IdEntry *id)
{
    if (id->fileoffset) {
        return id->fileoffset;
    } else {
        FileOffset fo;
        int i;
        fgetpos(HatFile,(fpos_t)&fo);
        fo = htonl(fo);
        fputc(((NmType<<5) | NTId)),HatFile);
        fprintf(HatFile,"%s",id->name);
        fputc(0x0,HatFile);
        i = 0;
        fwrite(&i,                       sizeof(FileOffset), 1, HatFile);
      /*fwrite(&id->modinfo->fileoffset, sizeof(FileOffset), 1, HatFile);*/
        fputc(id->pri,HatFile);
        i = htonl(id->srcpos);
        fwrite(&i, sizeof(int), 1, HatFile);
        id->fileoffset = fo;
        return fo;
    }
}

FileOffset
mkNTConstr (IdEntry *id)
{
    if (id->fileoffset) {
        return id->fileoffset;
    } else {
        FileOffset fo;
        int i;
        fgetpos(HatFile,(fpos_t)&fo);
        fo = htonl(fo);
        fputc(((NmType<<5) | NTConstr)),HatFile);
        fprintf(HatFile,"%s",id->name);
        fputc(0x0,HatFile);
        i = 0;
        fwrite(&i,                       sizeof(FileOffset), 1, HatFile);
      /*fwrite(&id->modinfo->fileoffset, sizeof(FileOffset), 1, HatFile);*/
        fputc(id->pri,HatFile);
        i = htonl(id->srcpos);
        fwrite(&i, sizeof(int), 1, HatFile);
        id->fileoffset = fo;
        return fo;
    }
}

FileOffset
mkNTTuple ()
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTTuple)),HatFile);
    return fo;
}

FileOffset
mkNTFun ()
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTFun)),HatFile);
    return fo;
}

FileOffset
mkNTCase ()
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTCase)),HatFile);
    return fo;
}

FileOffset
mkNTLambda ()
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTLambda)),HatFile);
    return fo;
}

FileOffset
mkNTDummy ()
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTDummy)),HatFile);
    return fo;
}

FileOffset
mkNTCString (char *s)
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTCString)),HatFile);
    fprintf(HatFile,"%s",s);
    fputc(0x0,HatFile);
    return fo;
}

FileOffset
mkNTIf ()
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTIf)),HatFile);
    return fo;
}

FileOffset
mkNTGuard ()
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTGuard)),HatFile);
    return fo;
}

FileOffset
mkNTContainer ()
{
    FileOffset fo;
    fgetpos(HatFile,(fpos_t)&fo);
    fo = htonl(fo);
    fputc(((NmType<<5) | NTContainer)),HatFile);
    return fo;
}





FileOffset
mkSR0 ()
{
    return (FileOffset)0;
}

FileOffset
mkSR3 (SrcRef *sr)
{
    if (sr->fileoffset) {
        return sr->fileoffset;
    } else {
        FileOffset fo;
        int i = 0;
        fgetpos(HatFile,(fpos_t)&fo);
        fo = htonl(fo);
        fputc((SR<<5),HatFile);
        fwrite(&i,                       sizeof(fileOffset), 1, HatFile);*/
      /*fwrite(&sr->modinfo->fileoffset, sizeof(fileOffset), 1, HatFile);*/
        i = htonl(sr->posn);
        fwrite(&i, sizeof(int), 1, HatFile);*/
        sr->fileoffset = fo;
        return fo;
    }
}

