/* basic read/write from real memory locations, for primitive FFI */

void   writeDoubleAtAddr (void* a, double d)	{ *(double*)a = d; }
double readDoubleAtAddr  (void* a)		{ return *(double*)a; }
void   writeFloatAtAddr  (void* a, float f)	{ *(float*)a = f; }
float  readFloatAtAddr   (void* a)		{ return *(float*)a; }
void   writeAddrAtAddr   (void* a, void* v)	{ *(void**)a = v; }
void*  readAddrAtAddr    (void* a)		{ return *(void**)a; }
void   writeIntAtAddr    (void* a, int i)	{ *(int*)a = i; }
int    readIntAtAddr     (void* a)		{ return *(int*)a; }
void   writeCharAtAddr   (void* a, char c)	{ *(char*)a = c; }
char   readCharAtAddr    (void* a)		{ return *(char*)a; }

