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

#if defined(linux) \
 && !defined(__FreeBSD__) \
 && !defined(__NetBSD__) \
 && !(defined(sun) && !defined(sun4sol2))
#  define ecls_setjmp	setjmp
#  define ecls_longjmp	longjmp
#else
#  define ecls_setjmp	_setjmp
#  define ecls_longjmp	_longjmp
#endif

#ifdef __linux__
#  define FILE_CNT(fp)	((fp)->_IO_read_end - (fp)->_IO_read_ptr)
#elif defined(__FreeBSD__) || defined(__NetBSD__)
#  define FILE_CNT(fp)	((fp)->_r)
#  define _IO_buf_base	_bf._base /* watch out */
#elif defined(__EMX__)
#  define FILE_CNT(fp)	((fp)->rcount)
#  define _IO_buf_base	buffer
#else
#  define FILE_CNT(fp)	(fp)->_cnt
#  define _IO_buf_base	_base
#endif

#ifdef	MSDOS
#  define IS_DIR_SEPARATOR(x) ((x=='/')||(x=='\\'))
#  define DIR_SEPARATOR	'\\'
#  define PATH_SEPARATOR	';'
#  define OPEN_R	"rb"
#  define OPEN_W	"wb"
#  define OPEN_RW	"w+b"
#  define OPEN_A	"ab"
#  define OPEN_RA	"a+b"
#else
#  define IS_DIR_SEPARATOR(x) (x=='/')
#  define DIR_SEPARATOR	'/'
#  define PATH_SEPARATOR	':'
#  define OPEN_R	"r"
#  define OPEN_W	"w"
#  define OPEN_RW	"w+"
#  define OPEN_A	"a"
#  define OPEN_RA	"a+"
#endif	MSDOS

#ifdef	MSDOS
#  define CRLF
#endif	MSDOS

#if defined(__EMX__) || defined(__NetBSD__)
#  define unix
#endif

#if defined(unix) && !defined(__MACH__)
#  define NEED_MALLOC
#endif

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
#elif	defined(sparc) || defined(__sparc)
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
#endif

#ifdef	MSDOS
#  define SOFTWARE_TYPE	MSDOS
#elif	defined(unix)
#  define SOFTWARE_TYPE	UNIX
#elif	defined(__WIN32__)
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
#else
#  define SOFTWARE_VERSION	UNKNOWN
#endif

/***********************************************************************/

#ifdef	__FreeBSD__
#include <dlfcn.h>
#define	IEEEFLOAT
#define	DOWN_STACK
#define	JB_SP 4
#define	BRAND "IBM-PC"
#define	CLIBS -lcompat
#define	LDFLAGS -Wl,--export-dynamic
#define HAVE_ISOC99
#ifdef __ELF__
# define DATA_START 0x8000000
#else
# define DATA_START 0
#endif
#ifndef unix
#define unix
#endif
#endif	__FreeBSD__

#ifdef	__NetBSD__
#include <dlfcn.h>
#define	IEEEFLOAT
#define	DOWN_STACK
#define	JB_SP 4
#ifndef BSD
# define BSD
#endif
#ifndef unix
# define unix
#endif
#ifdef __ELF__
# define DATA_START 0x8000000
#else
#error "A.out not yet supported in NetBSD"
# define DATA_START 0
#endif
#define	BRAND "IBM-PC"
#define	CLIBS -lcompat
#define	LDFLAGS
#endif	__NetBSD__

#ifdef	sun
#define	IEEEFLOAT
#define	DATA_START 0
#define	DOWN_STACK
#define	BSD
#define	BRAND "SUN"
#define LDFLAGS

#ifdef	sun4sol2
#  include <dlfcn.h>
#  include <link.h>
#ifdef TCP
#  define CLIBS -lsocket -lnsl -lintl -ldl
#else
#  define CLIBS -ldl
#endif
#endif sun4sol2
#endif	sun

/**********************************************************************/
