# Makefile for the literate nhc98 runtime kernel. 

# To configure this file set the following variables:

# FWEXE: A path to the FunnelWeb executable

FWEXE       = $h/Applications/eli-4.4.0/lib/Eli/fw/1.0.1/fw.exe

# CFLAGS: Flags for compiling the C files (FIXME: hardcoded for now)

CFLAGS      = -I. -I../Mk -DHIGH_BYTE_FIRST

#
# Shouldn't need to change anything below here
#

# The FunnelWeb files.  All of the $(FWIS) should be included into $(FW)

FWS			= $(FW) $(FWIS)              
FW	 		= doc.fw
FWIS        = bytecode.fwi cdata.fwi cinterface.fwi collector.fwi graph.fwi \
              main.fwi mark.fwi mutator.fwi newbuiltin.fwi newtables.fwi \
              stableptr.fwi system.fwi unix.fwi

# The C files and headers that are generated from the FunnelWeb files.

SRCS        = $(CS) $(HS)             
CS          = cdata.c collector.c main.c mark.c mutator.c mutlib.c \
              newbuiltin.c newtables.c stableptr.c system.c timeUnix.c
HS          = bytecode.h cinterface.h mark.h mutlib.h newbytecode.h \
              newmacros.h node.h runtime.h stableptr.h

# Commands for running FunnelWeb

TANGLE	    = $(FWEXE)
WEAVE       = $(FWEXE) +T

# Command for formatting tex source

PDFTEX		= pdftex

# One object file for each C file, the library archive file

OS          = $(patsubst %.c,%.o,${CS})
LIB	        = Kernel.a

# The TeX source and formatted version of the document.  Also, FW and TeX
# execution by-products: listing, nop file, log file.

TEX	        = $(addsuffix .tex,$(basename $(FW)))
PDF	        = $(addsuffix .pdf,$(basename $(FW)))

AUX   		= $(LIS) $(NOP) $(LOG)
LIS	        = $(addsuffix .lis,$(basename $(FW)))
LOG	        = $(addsuffix .log,$(basename $(FW)))
NOP	        = $(addsuffix .nop,$(basename $(FW)))

all:        $(LIB) $(PDF)

$(LIB):     $(OS)
	ar -rs $@ $?
            
$(CS) $(HS):$(FWS)
	$(TANGLE) $(FW)
	    
$(PDF):		$(TEX)
	$(PDFTEX) $?
		
$(TEX):		$(FWS)
	$(WEAVE) $(FW)
		
clean:
	rm -f $(LIB) $(OS) $(PDF) $(TEX) $(SRCS) $(AUX)
