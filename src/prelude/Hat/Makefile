include Makefile.inc

OBJDIR = $(BUILDDIR)/$(OBJ)/hatlib
TARGET = $(LIBDIR)/$(MACHINE)/libHSHat.a
CFLAGS = -DVERSION="\"$(VERSION)\"" -I$(INCDIR)
# must give package name whilst building, plus we need the FFI
HFLAGS = -package-name hat -package lang -fglasgow-exts
export HFLAGS


# C sources are just compiled normally
CSRCS = hat.c
COBJS = $(patsubst %.c,  $(OBJDIR)/%.o, $(CSRCS))
# plain Haskell sources are also compiled normally
PLAINSRCS = Hat.hs TPreludeBuiltinTypes.hs
PLAINOBJS = $(patsubst %.hs,  $(OBJDIR)/%.o, $(PLAINSRCS))
# these Haskell sources undergo hat-trans before compilation
TRANSSRCS = PreludeBuiltin.hs PreludeBasic.hs
TRANSOBJS = $(patsubst %.hs,  $(OBJDIR)/T%.o, $(TRANSSRCS))
# these Haskell sources undergo hat-trans, and the final obj is also renamed
MOVEDSRCS = PreludeTracing.hs CharTracing.hs
MOVEDOBJS = $(patsubst %Tracing.hs,  $(OBJDIR)/T%.o, $(MOVEDSRCS))


all: $(OBJDIR) $(TARGET)
clean:
	-rm -rf $(OBJDIR) TPreludeBuiltin.hs TPreludeBasic.hs \
			TPreludeTracing.hs  TCharTracing.hs
$(OBJDIR):
	mkdir -p $(OBJDIR)

$(PLAINOBJS): $(OBJDIR)/%.o: %.hs
	ghc $(HFLAGS) -cpp -c -o $@ $<

$(TRANSOBJS): $(OBJDIR)/T%.o: %.hs
	$(LOCAL)hat-trans -trusted -prelude $<
	ghc $(HFLAGS) -cpp -c -o $@ T$<

$(MOVEDOBJS): $(OBJDIR)/T%.o: %Tracing.hs
	$(LOCAL)hat-trans -trusted -prelude $<
	ghc $(HFLAGS) -cpp -c -o $@ T$<
	mv $(patsubst %.hs, T%.hi, $<) $(patsubst %Tracing.hs, T%.hi, $<) 
	mv $(patsubst %.hs, T%.hx, $<) $(patsubst %Tracing.hs, T%.hx, $<) 

#$(OBJDIR)/TPrelude.o: PreludeTracing.hs
#	$(LOCAL)hat-trans -trusted -prelude $<
#	ghc $(HFLAGS) -cpp -c -o $@ T$<
#	mv $(OBJDIR)/TPreludeTracing.o $(OBJDIR)/TPrelude.o
#	mv TPreludeTracing.hi TPrelude.hi

#$(OBJDIR)/TChar.o: CharTracing.hs
#	$(LOCAL)hat-trans -trusted $<
#	ghc $(HFLAGS) -cpp -c -o $@ T$<
#	mv $(OBJDIR)/TCharTracing.o $(OBJDIR)/TChar.o
#	mv TCharTracing.hi TChar.hi

$(COBJS): $(OBJDIR)/%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

# single archive for all object files
$(TARGET): $(COBJS) $(PLAINOBJS) $(TRANSOBJS) $(MOVEDOBJS)
	$(CC) -o $(TARGET) $(COBJS) $(PLAINOBJS) $(TRANSOBJS) $(MOVEDOBJS)
	# ghc-pkg --add-package <hat-package.conf


# Here are the extra dependencies.
$(OBJDIR)/TPreludeBuiltinTypes.o: $(OBJDIR)/Hat.o 
$(OBJDIR)/TPreludeBuiltin.o: $(OBJDIR)/TPreludeBuiltinTypes.o
$(OBJDIR)/TPreludeBasic.o: $(OBJDIR)/TPreludeBuiltin.o
$(OBJDIR)/TPrelude.o: $(OBJDIR)/TPreludeBasic.o
$(OBJDIR)/TChar.o: ${OBJDIR}/TPreludeBasic.o

# C-files dependencies.
$(OBJDIR)/hat.o: hat.c hat.h $(INCDIR)/art.h
