LIBS        = ../main.o ../mutlib.o ../mutator.o $(ALIBS)

LIBDIR      = /Users/asloane/Workspace/nhc98/lib/powerpc-Darwin7

# We need to pick up many things from Prelude and Runtime so we repeat
# them a few times since there are dependences in both directions.
# We assume that the Kernel things have been removed from the Runtime.a.

ALIBS       = $(LIBDIR)/Prelude.a ../Kernel.a $(LIBDIR)/Runtime.a \
              $(LIBDIR)/Prelude.a ../Kernel.a $(LIBDIR)/Runtime.a \
              $(LIBDIR)/Prelude.a ../Kernel.a $(LIBDIR)/Runtime.a

all:	    Main

Main:	    Main.o $(LIBS)
	    $(CC) -o $@ Main.o $(LIBS)

%.o:   	    %.hs
	    nhc98-cvs -c $<
			
clean:
	    rm -f Main Main.o Main.hi
