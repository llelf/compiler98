include Makefile.inc

PROG =	$(MACHINE)/hi
SRCS =	hi.hs $(MACHINE)/HmakeConfig.hs

ifeq "${BUILDWITH}" "ghc"
HFLAGS = -fglasgow-exts -syslib lang
export HFLAGS
else
endif

all: $(PROG)
install: all
	mv $(PROG) $(DST)
clean:
	-rm -f $(PROG)
	cd $(MACHINE); rm -f *.hi *.o

$(PROG): $(SRCS)
	cp hi.hs $(MACHINE)
	cd $(MACHINE); hmake -$(BUILDWITH) $(shell echo $(BUILDOPTS)) hi
