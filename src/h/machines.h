/*
    machines.h -- Machine descriptions.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#if defined(sparc) || defined(i386) || defined(mips)
#  define	stack_align(n)	(((n) + 0x7) & ~0x7)
#else
#  define	stack_align(n)	(((n) + 03) & ~03)
#endif

#if defined(linux)
#  define ecl_setjmp	setjmp
#  define ecl_longjmp	longjmp
#else
#  define ecl_setjmp	_setjmp
#  define ecl_longjmp	_longjmp
#endif

#ifdef __linux__
#  define FILE_CNT(fp)	((fp)->_IO_read_end - (fp)->_IO_read_ptr)
#elif defined(__FreeBSD__) || defined(__NetBSD__) || defined(cygwin) || defined(darwin)
#  define FILE_CNT(fp)	((fp)->_r)
#else
#  define FILE_CNT(fp)	(fp)->_cnt
#endif

#if defined(MSDOS) || defined(cygwin)
#  define IS_DIR_SEPARATOR(x) ((x=='/')||(x=='\\'))
#  define DIR_SEPARATOR	'\\'
#  define PATH_SEPARATOR	';'
#else
#  define IS_DIR_SEPARATOR(x) (x=='/')
#  define DIR_SEPARATOR	'/'
#  define PATH_SEPARATOR	':'
#endif /* MSDOS */

#if defined(MSDOS) || defined(cygwin) || defined(darwin)
#  define OPEN_R	"rb"
#  define OPEN_W	"wb"
#  define OPEN_RW	"w+b"
#  define OPEN_A	"ab"
#  define OPEN_RA	"a+b"
#  define CRLF
#else
#  define OPEN_R	"r"
#  define OPEN_W	"w"
#  define OPEN_RW	"w+"
#  define OPEN_A	"a"
#  define OPEN_RA	"a+"
#endif /* MSDOS */


/***********************************************************************

  Architectural features:

 ***********************************************************************/
#ifdef	vax
#  define ARCHITECTURE	"VAX"
#elif	defined(mc68000) || defined(__mc68000)
#  define ARCHITECTURE	"MC68000"
#elif	defined(mc68010) || defined(__mc68010)
#  define ARCHITECTURE	"MC68010"
#elif	defined(mc68020) || defined(__mc68020)
#  define ARCHITECTURE	"MC68020"
#elif	defined(mc68030) || defined(__mc68030)
#  define ARCHITECTURE	"MC68030"
#elif	defined(mc68040) || defined(__mc68040)
#  define ARCHITECTURE	"MC68040"
#elif	defined(sparc) || defined(__sparc) || defined(__sparc__)
#  define ARCHITECTURE	"SPARC"
#elif	defined(i386) || defined(__i386)
#  define ARCHITECTURE	"I386"
#elif	defined(ns32000) || defined(__ns32000)
#  define ARCHITECTURE	"NS32000"
#elif	__mips
#  define ARCHITECTURE	"MIPS"
#elif	defined(arm2) || defined(__arm2)
#  define ARCHITECTURE	"ARM2"
#elif	defined(arm) || defined(__arm)
#  define ARCHITECTURE	"ARM"
#elif	defined(PPC) || defined(__PPC__) || defined(__powerpc) || defined(__ppc__)
#  define ARCHITECTURE	"POWERPC"
#endif

#ifdef	MSDOS
#  define SOFTWARE_TYPE	MSDOS
#elif	defined(unix)
#  define SOFTWARE_TYPE	UNIX
#elif	defined(__WIN32__) || defined(cygwin)
#  define SOFTWARE_TYPE WIN32
#else
#  define SOFTWARE_TYPE	UNKNOWN
#endif	MSDOS

#ifdef  aix
#  define SOFTWARE_VERSION	AIX
#elif	defined(__FreeBSD__)
#  define SOFTWARE_VERSION	FreeBSD
#elif	defined(__NetBSD__)
#  define SOFTWARE_VERSION	NetBSD
#elif	defined(__linux__)
#  define SOFTWARE_VERSION	Linux
#elif	defined(__NeXT)
#  define SOFTWARE_VERSION	MACH
#elif	defined(ultrix)
#  define SOFTWARE_VERSION	ULTRIX
#elif	defined(hpux)
#  define SOFTWARE_VERSION	HPUX
#elif	defined(domain)
#  define SOFTWARE_VERSION	DOMAIN
#elif	defined(sgi)
#  define SOFTWARE_VERSION	IRIX
#elif	defined(bsd4_3)
#  define SOFTWARE_VERSION	BSD4.3
#elif	defined(bsd4_2)
#  define SOFTWARE_VERSION	BSD4.2
#elif	defined(BSD)
#  define SOFTWARE_VERSION	BSD
#elif	defined(sysv)
#  define SOFTWARE_VERSION	SYSTEM-V
#elif	defined(cygwin)
#  define SOFTWARE_VERSION	CYGWIN
#else
#  define SOFTWARE_VERSION	UNKNOWN
#endif

/***********************************************************************/

#ifdef darwin
#define IEEEFLOAT
#define BRAND "APPLE"
#define CLIBS
#define LDFLAGS
#define SHARED_LDFLAGS
#define USE_DLOPEN
#ifndef unix
# define unix
#endif
#endif /* darwin */

#ifdef	__FreeBSD__
#include <dlfcn.h>
#define	IEEEFLOAT
#define	JB_SP 4
#define	BRAND "IBM-PC"
#define	CLIBS -lcompat
#define	LDFLAGS -Wl,--export-dynamic
#define SHARED_LDFLAGS -shared
#define USE_DLOPEN
#define HAVE_ISOC99
#ifndef unix
#  define unix
#endif
#endif	__FreeBSD__

#ifdef	__linux__
#include <dlfcn.h>
#define	BRAND "IBM-PC"
#define	IEEEFLOAT
#define	BSD
#define CLIBS -ldl
#define LDFLAGS -Wl,--export-dynamic
#define SHARED_LDFLAGS -shared
#define USE_DLOPEN
#define _ISOC99_SOURCE
#define HAVE_ISOC99
#define HAVE_POSIX
#ifndef unix
#  define unix
#endif
#endif	linux

#ifdef	__NetBSD__
#include <dlfcn.h>
#define	IEEEFLOAT
#define	JB_SP 4
#define LDFLAGS -Wl,--export-dynamic
#define SHARED_LDFLAGS -shared
#define USE_DLOPEN
#ifndef BSD
#  define BSD
#endif
#ifndef unix
#  define unix
#endif
#define	BRAND "IBM-PC"
#define	CLIBS -lcompat
#define	LDFLAGS
#endif	__NetBSD__

#ifdef	sun
#define	IEEEFLOAT
#define	BSD
#define	BRAND "SUN"
#define LDFLAGS
#define SHARED_LDFLAGS -dy -G
#ifdef	sun4sol2
#  include <dlfcn.h>
#  include <link.h>
#  define USE_DLOPEN
#ifdef TCP
#  define CLIBS -lsocket -lnsl -lintl -ldl
#else
#  define CLIBS -ldl
#endif
#endif sun4sol2
#endif	sun

#ifdef	cygwin
#define	IEEEFLOAT
#define	BSD
#define	BRAND "REDHAT"
#define LDFLAGS
#define SHARED_LDFLAGS
#define HAVE_ISOC99
#define HAVE_POSIX
#undef USE_DLOPEN
#ifndef unix
#  define unix
#endif
#endif	cygwin

/**********************************************************************/
