include Makefile.inc

OBJDIR = $(BUILDDIR)/$(OBJ)/hatlib
TARGET = $(LIBDIR)/$(MACHINE)/libHSHat.a
CFLAGS = -DVERSION="\"$(VERSION)\"" -I$(INCDIR)
# must give package name whilst building, plus we need the FFI
HFLAGS = -package-name hat -package lang -fglasgow-exts
export HFLAGS


# C sources are just compiled normally
CSRCS = hat.c
COBJS = $(patsubst %.c, $(OBJDIR)/%.o, $(CSRCS))
# plain Haskell sources are also compiled normally
PLAINSRCS = Hat.hs TPreludeBuiltinTypes.hs
PLAINOBJS = $(patsubst %.hs, $(OBJDIR)/%.o, $(PLAINSRCS))
# these Haskell sources undergo hat-trans before compilation
TRANSSRCS = PreludeBuiltin.hs PreludeBasic.hs
TRANSHATS = $(patsubst %, T%,                $(TRANSSRCS))
TRANSOBJS = $(patsubst %.hs, $(OBJDIR)/T%.o, $(TRANSSRCS))
# these Haskell sources undergo hat-trans, and the final obj/hx is also renamed
MOVEDSRCS = PreludeTracing.hs CharTracing.hs
MOVEDHATS = $(patsubst %, T%,                       $(MOVEDSRCS))
MOVEDOBJS = $(patsubst %Tracing.hs, $(OBJDIR)/T%.o, $(MOVEDSRCS))


all: $(OBJDIR) $(TARGET)
install: $(OBJDIR) $(TARGET)
	$(INSTALL) $(TARGET) $(GHCLIBDIR)
	$(INSTALL) TPrelude.hi TChar.hi Prelude.hx Char.hx $(GHCINCDIR)
	ghc-pkg --add-package <hat-package.conf
clean:
	-rm -rf $(OBJDIR) $(TRANSHATS) $(MOVEDHATS) *.hi Hat.hx \
		$(patsubst %.hs, %.hx, $(TRANSSRCS)) \
		$(patsubst %Tracing.hs, %.hx, $(MOVEDSRCS))
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
	mv $(patsubst %.hs, %.hx, $<) $(patsubst %Tracing.hs, %.hx, $<) 


$(COBJS): $(OBJDIR)/%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

# single archive for all object files
$(TARGET): $(COBJS) $(PLAINOBJS) $(TRANSOBJS) $(MOVEDOBJS)
	$(AR) cr $(TARGET) $(COBJS) $(PLAINOBJS) $(TRANSOBJS) $(MOVEDOBJS)
	ranlib $(TARGET)


# Here are the extra dependencies.
$(OBJDIR)/TPreludeBuiltinTypes.o: $(OBJDIR)/Hat.o 
$(OBJDIR)/TPreludeBuiltin.o: $(OBJDIR)/TPreludeBuiltinTypes.o
$(OBJDIR)/TPreludeBasic.o: $(OBJDIR)/TPreludeBuiltin.o
$(OBJDIR)/TPrelude.o: $(OBJDIR)/TPreludeBasic.o
$(OBJDIR)/TChar.o: ${OBJDIR}/TPreludeBasic.o

# C-files dependencies.
$(OBJDIR)/hat.o: hat.c hat.h $(INCDIR)/art.h

