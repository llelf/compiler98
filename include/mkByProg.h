extern NodePtr mkTuple2(NodePtr a1,NodePtr a2);
#define sizeTuple2 (1+EXTRA+2)
#define Tuple2 0 
extern NodePtr mkTuple3(NodePtr a1,NodePtr a2,NodePtr a3);
#define sizeTuple3 (1+EXTRA+3)
#define Tuple3 0 
extern NodePtr mkTuple4(NodePtr a1,NodePtr a2,NodePtr a3,NodePtr a4);
#define sizeTuple4 (1+EXTRA+4)
#define Tuple4 0 
extern NodePtr mkTuple5(NodePtr a1,NodePtr a2,NodePtr a3,NodePtr a4,NodePtr a5);
#define sizeTuple5 (1+EXTRA+5)
#define Tuple5 0 
extern NodePtr mkTuple6(NodePtr a1,NodePtr a2,NodePtr a3,NodePtr a4,NodePtr a5,NodePtr a6);
#define sizeTuple6 (1+EXTRA+6)
#define Tuple6 0 
extern NodePtr mkTuple7(NodePtr a1,NodePtr a2,NodePtr a3,NodePtr a4,NodePtr a5,NodePtr a6,NodePtr a7);
#define sizeTuple7 (1+EXTRA+7)
#define Tuple7 0 
extern NodePtr mkTuple8(NodePtr a1,NodePtr a2,NodePtr a3,NodePtr a4,NodePtr a5,NodePtr a6,NodePtr a7,NodePtr a8);
#define sizeTuple8 (1+EXTRA+8)
#define Tuple8 0 
extern NodePtr mkTuple9(NodePtr a1,NodePtr a2,NodePtr a3,NodePtr a4,NodePtr a5,NodePtr a6,NodePtr a7,NodePtr a8,NodePtr a9);
#define sizeTuple9 (1+EXTRA+9)
#define Tuple9 0 
extern NodePtr mkTuple10(NodePtr a1,NodePtr a2,NodePtr a3,NodePtr a4,NodePtr a5,NodePtr a6,NodePtr a7,NodePtr a8,NodePtr a9,NodePtr a10);
#define sizeTuple10 (1+EXTRA+10)
#define Tuple10 0 
extern NodePtr mkTuple11(NodePtr a1,NodePtr a2,NodePtr a3,NodePtr a4,NodePtr a5,NodePtr a6,NodePtr a7,NodePtr a8,NodePtr a9,NodePtr a10,NodePtr a11);
#define sizeTuple11 (1+EXTRA+11)
#define Tuple11 0 
extern NodePtr mkTuple12(NodePtr a1,NodePtr a2,NodePtr a3,NodePtr a4,NodePtr a5,NodePtr a6,NodePtr a7,NodePtr a8,NodePtr a9,NodePtr a10,NodePtr a11,NodePtr a12);
#define sizeTuple12 (1+EXTRA+12)
#define Tuple12 0 
extern Node C0_Prelude_46False[];
#define mkFalse() ((NodePtr)C0_Prelude_46False)
#define sizeFalse 0
#define False 0
extern Node C0_Prelude_46True[];
#define mkTrue() ((NodePtr)C0_Prelude_46True)
#define sizeTrue 0
#define True 1
extern NodePtr mkLeft(NodePtr a1);
#define sizeLeft (1+EXTRA+1)
#define Left 0
extern NodePtr mkRight(NodePtr a1);
#define sizeRight (1+EXTRA+1)
#define Right 1
extern Node C0_Prelude_46_91_93[];
#define mkNil() ((NodePtr)C0_Prelude_46_91_93)
#define sizeNil 0
#define Nil 0
extern NodePtr mkCons(NodePtr a1,NodePtr a2);
#define sizeCons (1+EXTRA+2)
#define Cons 1
extern Node C0_Prelude_46Nothing[];
#define mkNothing() ((NodePtr)C0_Prelude_46Nothing)
#define sizeNothing 0
#define Nothing 0
extern NodePtr mkJust(NodePtr a1);
#define sizeJust (1+EXTRA+1)
#define Just 1
extern Node C0_Prelude_46LT[];
#define mkLT() ((NodePtr)C0_Prelude_46LT)
#define sizeLT 0
#define LT 0
extern Node C0_Prelude_46EQ[];
#define mkEQ() ((NodePtr)C0_Prelude_46EQ)
#define sizeEQ 0
#define EQ 1
extern Node C0_Prelude_46GT[];
#define mkGT() ((NodePtr)C0_Prelude_46GT)
#define sizeGT 0
#define GT 2
extern NodePtr mkRatio(NodePtr a1,NodePtr a2);
#define sizeRatio (1+EXTRA+2)
#define Ratio 0
extern Node C0__40_41[];
#define mkUnit() ((NodePtr)C0__40_41)
#define sizeUnit 0
#define Unit 0 
extern Node C0_Prelude_46_95Void[];
#define mk_Void() ((NodePtr)C0_Prelude_46_95Void)
#define size_Void 0
#define _Void 0
extern NodePtr mkIO(NodePtr a1);
#define sizeIO (1+EXTRA+1)
#define IO 0
extern NodePtr mkIOErrorUser(NodePtr a1);
#define sizeIOErrorUser (1+EXTRA+1)
#define IOErrorUser 0
extern NodePtr mkIOErrorSystem(NodePtr a1,NodePtr a2);
#define sizeIOErrorSystem (1+EXTRA+2)
#define IOErrorSystem 1
extern NodePtr mkIOErrorOpen(NodePtr a1,NodePtr a2,NodePtr a3);
#define sizeIOErrorOpen (1+EXTRA+3)
#define IOErrorOpen 2
extern NodePtr mkIOErrorEOF(NodePtr a1,NodePtr a2);
#define sizeIOErrorEOF (1+EXTRA+2)
#define IOErrorEOF 3
extern NodePtr mkIOErrorHIsEOF(NodePtr a1,NodePtr a2);
#define sizeIOErrorHIsEOF (1+EXTRA+2)
#define IOErrorHIsEOF 4
extern NodePtr mkIOErrorHFileSize(NodePtr a1,NodePtr a2);
#define sizeIOErrorHFileSize (1+EXTRA+2)
#define IOErrorHFileSize 5
extern NodePtr mkIOErrorHFlush(NodePtr a1,NodePtr a2);
#define sizeIOErrorHFlush (1+EXTRA+2)
#define IOErrorHFlush 6
extern NodePtr mkIOErrorHSeek(NodePtr a1,NodePtr a2);
#define sizeIOErrorHSeek (1+EXTRA+2)
#define IOErrorHSeek 7
extern NodePtr mkIOErrorHGetPosn(NodePtr a1,NodePtr a2);
#define sizeIOErrorHGetPosn (1+EXTRA+2)
#define IOErrorHGetPosn 8
extern NodePtr mkIOErrorHSetPosn(NodePtr a1,NodePtr a2);
#define sizeIOErrorHSetPosn (1+EXTRA+2)
#define IOErrorHSetPosn 9
extern NodePtr mkIOErrorHGetBuffering(NodePtr a1,NodePtr a2);
#define sizeIOErrorHGetBuffering (1+EXTRA+2)
#define IOErrorHGetBuffering 10
extern NodePtr mkIOErrorHSetBuffering(NodePtr a1,NodePtr a2);
#define sizeIOErrorHSetBuffering (1+EXTRA+2)
#define IOErrorHSetBuffering 11
extern NodePtr mkMkArray(NodePtr a1,NodePtr a2);
#define sizeMkArray (1+EXTRA+2)
#define MkArray 0
extern NodePtr mkComplex(NodePtr a1,NodePtr a2);
#define sizeComplex (1+EXTRA+2)
#define Complex 0
extern Node C0_IO_46NoBuffering[];
#define mkNoBuffering() ((NodePtr)C0_IO_46NoBuffering)
#define sizeNoBuffering 0
#define NoBuffering 0
extern Node C0_IO_46LineBuffering[];
#define mkLineBuffering() ((NodePtr)C0_IO_46LineBuffering)
#define sizeLineBuffering 0
#define LineBuffering 1
extern NodePtr mkBlockBuffering(NodePtr a1);
#define sizeBlockBuffering (1+EXTRA+1)
#define BlockBuffering 2
extern Node C0_IO_46ReadMode[];
#define mkReadMode() ((NodePtr)C0_IO_46ReadMode)
#define sizeReadMode 0
#define ReadMode 0
extern Node C0_IO_46WriteMode[];
#define mkWriteMode() ((NodePtr)C0_IO_46WriteMode)
#define sizeWriteMode 0
#define WriteMode 1
extern Node C0_IO_46AppendMode[];
#define mkAppendMode() ((NodePtr)C0_IO_46AppendMode)
#define sizeAppendMode 0
#define AppendMode 2
extern Node C0_IO_46ReadWriteMode[];
#define mkReadWriteMode() ((NodePtr)C0_IO_46ReadWriteMode)
#define sizeReadWriteMode 0
#define ReadWriteMode 3
extern Node C0_IO_46AbsoluteSeek[];
#define mkAbsoluteSeek() ((NodePtr)C0_IO_46AbsoluteSeek)
#define sizeAbsoluteSeek 0
#define AbsoluteSeek 0
extern Node C0_IO_46RelativeSeek[];
#define mkRelativeSeek() ((NodePtr)C0_IO_46RelativeSeek)
#define sizeRelativeSeek 0
#define RelativeSeek 1
extern Node C0_IO_46SeekFromEnd[];
#define mkSeekFromEnd() ((NodePtr)C0_IO_46SeekFromEnd)
#define sizeSeekFromEnd 0
#define SeekFromEnd 2
extern Node C0_IO_46SocketStream[];
#define mkSocketStream() ((NodePtr)C0_IO_46SocketStream)
#define sizeSocketStream 0
#define SocketStream 0
extern Node C0_IO_46SocketDatagram[];
#define mkSocketDatagram() ((NodePtr)C0_IO_46SocketDatagram)
#define sizeSocketDatagram 0
#define SocketDatagram 1
extern Node C0_IO_46SocketRaw[];
#define mkSocketRaw() ((NodePtr)C0_IO_46SocketRaw)
#define sizeSocketRaw 0
#define SocketRaw 2
extern NodePtr mkSocket(NodePtr a1);
#define sizeSocket (1+EXTRA+1)
#define Socket 0
extern Node C0_System_46ExitSuccess[];
#define mkExitSuccess() ((NodePtr)C0_System_46ExitSuccess)
#define sizeExitSuccess 0
#define ExitSuccess 0
extern NodePtr mkExitFailure(NodePtr a1);
#define sizeExitFailure (1+EXTRA+1)
#define ExitFailure 1
