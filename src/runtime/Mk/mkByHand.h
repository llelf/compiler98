
NodePtr mkCInt(Int i);
#define sizeCInt 1

#define mkBool(b) (b?mkTrue():mkFalse())
#define sizeBool 0

#define mkChar(c) ((NodePtr)GET_CHAR(c))
#define sizeChar 0

extern NodePtr mkHandlePosn(fpos_t *);
extern fpos_t *getHandlePosn(NodePtr);
#define sizeHandlePosn (1+EXTRA+(sizeof(fpos_t)+3)/sizeof(Node))

extern NodePtr mkSmallIntegerU(Int i);
#define sizeSmallIntegerU (1+EXTRA+1)

extern NodePtr mkInt(Int i);
#define sizeInt (1+EXTRA+1)

extern NodePtr mkDouble(double d);
#define sizeDouble (1+EXTRA+2)

extern NodePtr mkFloat(float f);
#define sizeFloat (1+EXTRA+1)

int sizePackedString(int length);
NodePtr allocPackedString(int length);
void copyPackedString(int length, NodePtr dst, char *src);
NodePtr mkPackedString(int length, char *str);
char *getPackedString(NodePtr n);

#define mkString(s)   mkPackedString((s==(char*)0?1:strlen(s)+1),s)	/* MW */
extern NodePtr mkForeign(void *x, gccval f);		/* MW */
extern NodePtr mkStablePtr(NodePtr x);			/* MW */
extern NodePtr getStablePtr(NodePtr x);			/* MW */
