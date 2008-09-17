# Makefile for Windows NT.  Assumes Microsoft compiler, and a single thread.
# DLLs are included in the root set under NT, but not under win32S.
# Use "nmake nodebug=1 all" for optimized versions of library, gctest and editor.
#

MY_CPU=X86
CPU=$(MY_CPU)
!include <ntwin32.mak>

srcdir = ..\..\src\gc

!if "$(ECL_THREADS)" != ""
THREADS_OBJ= win32_threads.obj
THREADS_FLAGS= -DGC_DLL -DGC_BUILD -DGC_WIN32_THREADS
!else
THREADS_OBJ=
THREADS_FLAGS= -DGC_DLL -DGC_BUILD
!endif

OBJS= alloc.obj reclaim.obj allchblk.obj misc.obj mach_dep.obj os_dep.obj mark_rts.obj headers.obj mark.obj obj_map.obj blacklst.obj finalize.obj new_hblk.obj dbg_mlc.obj malloc.obj stubborn.obj dyn_load.obj typd_mlc.obj ptr_chck.obj gc_cpp.obj mallocx.obj $(THREADS_OBJ)

all: gc.lib

{$(srcdir)}.c{}.obj:
	$(cc) $(cflags) $(CFLAGS_CONFIG) -I$(srcdir)\include -DSILENT -DALL_INTERIOR_POINTERS -D__STDC__ -DGC_DLL -DGC_BUILD -DLARGE_CONFIG $(THREADS_FLAGS) $< /Fo$*.obj

{$(srcdir)\tests}.c{tests}.obj:
	$(cc) $(cflags) $(CFLAGS_CONFIG) -I$(srcdir)\include -DSILENT -DALL_INTERIOR_POINTERS -D__STDC__ -DGC_DLL -DGC_BUILD -DLARGE_CONFIG $(THREADS_FLAGS) $< /Fo$*.obj

#.c.obj:
#	$(cc) $(cdebug) $(cflags) $(cvars) -Iinclude -DSILENT -DALL_INTERIOR_POINTERS -D__STDC__ -DGC_NOT_DLL -DGC_BUILD $*.c /Fo$*.obj
#
.cpp.obj:
	$(cc) $(cdebug) $(cflags) $(cvars) -I$(srcdir)\include -DSILENT -DALL_INTERIOR_POINTERS -DGC_NOT_DLL -DGC_BUILD $*.CPP /Fo$*.obj

$(OBJS) tests\test.obj: $(srcdir)\include\private\gc_priv.h $(srcdir)\include\private\gc_hdrs.h $(srcdir)\include\gc.h $(srcdir)\include\private\gcconfig.h $(srcdir)\include\private\gc_locks.h $(srcdir)\include\private\gc_pmark.h $(srcdir)\include\gc_mark.h

gc.lib: $(OBJS)
	lib /MACHINE:i386 /out:gc.lib $(OBJS)
# The original NT SDK used lib32 instead of lib

gctest.exe: tests\test.obj gc.lib
#	The following works for win32 debugging.  For win32s debugging use debugtype:coff
#	and add mapsympe line.
#  This produces a "GUI" applications that opens no windows and writes to the log file
#  "gc.log".  This is done to make the result runnable under win32s.
	$(link) -debug:full -debugtype:cv $(guiflags) -stack:131072 -out:$*.exe tests\test.obj $(guilibs) gc.lib
#	mapsympe -n -o gctest.sym gctest.exe

cord\de_win.rbj: cord\de_win.res
	cvtres /MACHINE:$(MY_CPU) /OUT:cord\de_win.rbj cord\de_win.res

cord\de.obj cord\de_win.obj: $(srcdir)\include\cord.h $(srcdir)\include\private\cord_pos.h cord\de_win.h cord\de_cmds.h

cord\de_win.res: cord\de_win.rc cord\de_win.h cord\de_cmds.h
	$(rc) $(rcvars) -r -fo cord\de_win.res $(cvars) cord\de_win.rc

# Cord/de is a real win32 gui application.
cord\de.exe: cord\cordbscs.obj cord\cordxtra.obj cord\de.obj cord\de_win.obj cord\de_win.rbj gc.lib
	$(link) -debug:full -debugtype:cv $(guiflags) -stack:16384 -out:cord\de.exe  cord\cordbscs.obj cord\cordxtra.obj cord\de.obj cord\de_win.obj cord\de_win.rbj gc.lib $(guilibs)

gc_cpp.obj: $(srcdir)\include\gc_cpp.h $(srcdir)\include\gc.h

gc_cpp.cpp: $(srcdir)\gc_cpp.cc
	copy $? $@

test_cpp.cpp: $(srcdir)\tests\test_cpp.cc
	copy $? $@

# This generates the C++ test executable.  The executable expects
# a single numeric argument, which is the number of iterations.
# The output appears in the file "gc.log".
test_cpp.exe: test_cpp.obj $(srcdir)\include\gc_cpp.h $(srcdir)\include\gc.h gc.lib
	$(link) -debug:full -debugtype:cv $(guiflags) -stack:16384 -out:test_cpp.exe test_cpp.obj gc.lib $(guilibs)

clean:
	-del gc.lib
	-for %i in ($(OBJS) gc_cpp.cpp) do erase %i
