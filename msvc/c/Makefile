#
#			Makefile for ECL core library
#
top_srcdir = ..\..\src
srcdir	= ..\..\src\c

!if "$(ECL_THREADS)" != ""
THREADS_OBJ= threads_win32.obj
THREADS_FLAGS= -DECL_THREADS -DGC_WIN32_THREADS
!else
THREADS_OBJ=
THREADS_FLAGS=
!endif

# Programs used by "make":
#
TRUE_CC = cl
CC	= cl
CFLAGS	= -c $(ECL_CFLAGS) -I./ -I$(srcdir) -I$(HDIR) -I../h -I$(top_srcdir)/gc/include $(THREADS_FLAGS)
#	  -Wall -W -Wfloat-equal -Wundef -Wendif-labels -Wpointer-arith -Wcast-align \
#	  -Wwrite-strings -Wconversion -Wsign-compare -Wmissing-prototypes -Wredundant-decls \
#	  -Wunreachable-code -Winline
DEFS    = $(THREADS_FLAGS)

SHELL	= /bin/sh
RM	= del
CP      = copy /Y
MV      = move /Y
LINK    = link
EXE	= .exe
DPP	= .\dpp$(EXE)

# Data for installation
#
INSTALL = @INSTALL@
INSTALL_DATA = @INSTALL_DATA@
prefix=@prefix@
exec_prefix=$(prefix)
libdir=$(prefix)\lib\ecl

# Files

HDIR	= $(top_srcdir)\h
HFILES	= ..\h\config.h $(HDIR)\ecl.h $(HDIR)\ecl-cmp.h\
	$(HDIR)\object.h $(HDIR)\cs.h $(HDIR)\stacks.h\
	$(HDIR)\external.h $(HDIR)\eval.h\
	$(HDIR)\number.h $(HDIR)\page.h $(HDIR)\unify.h\
	$(HDIR)\lwp.h $(HDIR)\internal.h
OBJS	= main.obj symbol.obj package.obj list.obj\
	apply.obj eval.obj interpreter.obj compiler.obj disassembler.obj \
	instance.obj gfun.obj reference.obj character.obj\
	file.obj read.obj print.obj error.obj string.obj cfun.obj\
	typespec.obj assignment.obj \
	predicate.obj big.obj number.obj\
	num_pred.obj num_comp.obj num_arith.obj num_sfun.obj num_co.obj\
	num_log.obj num_rand.obj array.obj sequence.obj cmpaux.obj\
	macros.obj backq.obj stacks.obj \
	time.obj unixint.obj\
	mapfun.obj multival.obj hash.obj format.obj pathname.obj\
	structure.obj load.obj unixfsys.obj unixsys.obj \
	ffi.obj alloc_2.obj tcp.obj $(THREADS_OBJ) ffi_x86.obj

all:	$(DPP) external.h ..\eclmin.lib ..\cinit.obj

.SUFFIXES: .obj .c .d

{$(srcdir:\=/)}.d{}.c:
	$(DPP) $< $@

{$(srcdir:\=/)/arch}.d{}.c:
	$(DPP) $< $@

#.d.c:
#	$(DPP) $< $@
.c.obj:
	$(CC) $(CFLAGS) -o $@ $<

.PHONY:	all

#
# When compiling the library itself, we have to remove the dllimport
# declarations, because the variables that they mark are in the
# in the library and can be referenced without indirection.
#
external.h: $(top_srcdir)/h/external.h Makefile
	cut.exe removedecl < $(top_srcdir)/h/external.h > $@

cut.exe: $(top_srcdir)/util/cut.c
	$(CC) $(LDFLAGS) /Fecut.exe $(top_srcdir)/util/cut.c

install: $(HFILES)
	for %i in ($(HFILES)) do $(CP) %i $(libdir)\h
	cut.exe < ..\h\config.h > $(libdir)\h\config-install.h
	$(RM) $(libdir)\h\config.h
	$(MV) $(libdir)\h\config-install.h $(libdir)\h\config.h

flatinstall: $(HFILES)
	for %i in ($(HFILES)) do $(CP) %i $(prefix)\h
	cut.exe < ..\h\config.h > $(prefix)\h\config-install.h
	for %h in (..\h\*.h) do $(CP) %h $(prefix)\h
	$(RM) $(prefix)\h\config.h
	$(MV) $(prefix)\h\config-install.h $(prefix)\h\config.h

..\eclmin.lib: $(OBJS:.obj=.c) all_symbols.c $(OBJS) all_symbols.obj all_symbols2.obj
	-$(RM) $@
	$(LINK) -lib -out:$@ $(OBJS)

clean:
	-for %f in (..\h\config.h dpp dpp.obj $(DPP) external.h \
	            ..\eclmin.lib ..\cinit.obj cinit.c \
		    symbols_list2.h) \
	     do $(RM) %f
	-for %f in ($(OBJS:.obj=.c) all_symbols.c) do $(RM) %f
	-for %f in ($(OBJS) all_symbols.obj all_symbols2.obj) do $(RM) %f
	-$(RM) *.pdb

# Build rules

$(DPP): $(srcdir)/dpp.c $(srcdir)/symbols_list2.h ../h/config.h
	$(TRUE_CC) -I../h -I./ -I$(HDIR) $(DEFS) $(srcdir)/dpp.c  -o $@
../h/config.h: ../h/config.h.msvc6 Makefile
	cut.exe "@ECL_FPE_CODE@" "$(srcdir:\=/)/arch/fpe_x86.c" \
	        < ..\h\config.h.msvc6 > $@

#
# GCC might break this code
#
gbc.o: gbc.c $(HFILES)
	$(CC) $(CFLAGS) -O0 gbc.c -o $@
#
# This reduces the overhead of jumping to other functions
#
apply.o: apply.c $(HFILES) $(HDIR)/cs.h
	$(CC) $(CFLAGS) apply.c -o $@
#
# These files are interrelated
#
all_symbols.obj: all_symbols.c
	$(CC) $(CFLAGS) -Fo$@ -I./ all_symbols.c
all_symbols2.obj: all_symbols.c
	$(CC) $(CFLAGS) -DECL_FINAL -Fo$@ -I./ all_symbols.c
#
# This is in another directory
#
../cinit.obj: cinit.c
	$(CC) $(CFLAGS) -I./ -o cinit.obj cinit.c
	$(MV) cinit.obj ..\
