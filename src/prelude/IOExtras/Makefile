include Makefile.inc

THISLIB	= IOExtras
SEARCH	= -P$(LOW) -P../PreludeIO -P../Array

SRCS	= \
	FixIO.hs PerformGC.hs UnsafePerformIO.hs UnsafePtrEq.hs _E.hs \
	DIOArray.hs BoundsIOArray.hs FreezeIOArray.hs NewIOArray.hs \
	ReadIOArray.hs WriteIOArray.hs \
	DIORef.hs NewIORef.hs ReadIORef.hs WriteIORef.hs
SRCS_T	= \
	$(SRCS)


# Here are the main rules.
include ../Makefile.common


# Here are the dependencies.

