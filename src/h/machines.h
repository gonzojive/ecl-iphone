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

#ifdef MSDOS
#  define RSYM rsym.exe
#elif !defined(__NeXT) && !defined(NeXT) /* cpp (configure) only defines NeXT */
#  define RSYM rsym
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

#ifdef	apollo
#define	IEEEFLOAT
#define	DATA_START 0
#define	DOWN_STACK
#define JB_SP 3
#define	BSD
#define COFF
#define	BRAND "HP"
#endif	apollo

#ifdef	__FreeBSD__
#define	IEEEFLOAT
#define	DOWN_STACK
#define	JB_SP 4
#ifdef __ELF__
# define ELF
# define UNEXEC unexelf
# define DATA_START 0x8000000
# define LDFLAGS -static
#else
# define DATA_START 0
# define AOUT <a.out.h>
#endif
#define	BRAND "IBM-PC"
#define	CLIBS -lcompat
#define	LDFLAGS -static
#define HAVE_ISOC99
#ifndef unix
#define unix
#endif
#endif	__FreeBSD__

#ifdef	__NetBSD__
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
# define ELF
# define DATA_START 0x8000000
#else
#error "A.out not yet supported in NetBSD"
# define AOUT <a.out.h>
# define DATA_START 0
#endif
#define	BRAND "IBM-PC"
#define	CLIBS -lcompat
#define	LDFLAGS -static
#endif	__NetBSD__

#ifdef	hp9000s300
#define	IEEEFLOAT
#define	DATA_START 0
#define	DOWN_STACK
#define JB_SP 14
#define	SYSV
#define	AOUT <a.out.h>
#define	BRAND "HP"
#define CFLAGS +Ns2000 +Nd2000
#endif	hp9000s300
 
#ifdef	hp9000s800
#define	IEEEFLOAT
#define	DATA_START 0x40000000
#undef	DOWN_STACK
#define JB_SP 14
#define	SYSV
#define COFF
#define UNEXEC unexhp9k800
#define	BRAND "HP"
#endif	hp9000s800

#ifdef	IBMRT
#define	IEEEFLOAT
#define	DATA_START 0x20000000
#define	DOWN_STACK
#define JB_SP 0
#define	BSD
#define	AOUT <a.out.h>
#define	BRAND "IBM"
#endif	IBMRT

#ifdef	__linux__
#define	IEEEFLOAT
#define	DOWN_STACK
#define	BSD
#ifdef __ELF__
# define ELF
# define UNEXEC unexelf
# define DATA_START 0x8000000
# define LDFLAGS -static
#else
# define DATA_START 0
# define AOUT <a.out.h>
#endif
#define	BRAND "IBM-PC"
#define HAVE_ISOC99
#define HAVE_POSIX
#ifndef unix
#define unix
#endif
#endif	linux

#ifdef	mips_dec
#define	IEEEFLOAT
#define	DATA_START 0xA00000	/* normally 0x10000000 */
#define	DOWN_STACK
/* #define JB_SP 32 */
#define	BSD
#define COFF
#define ECOFF
#define UNEXEC unexelf
#define	BRAND "DEC"
#define	LDFLAGS -Wl,-D -Wl,A00000
#define ILDFLAGS -T 0 -d -N
#define LSPCFLAGS -G 0
#endif	mips_dec

#ifdef	NEWS
#define	IEEEFLOAT
#define	DATA_START 0
#define	DOWN_STACK
#define	BSD
#define	AOUT <a.out.h>
#define	BRAND "SONY"
#endif	NEWS

#ifdef	__NeXT
#define	IEEEFLOAT
#define	DATA_START 0
#define	DOWN_STACK
/* #define JB_SP 2 in <setjmp.h> */
/* #define	BSD is in <param.h>	*/
#define UNEXEC unexnext
#define DLD dldNeXT
#define	BRAND "NeXT"
#define LDFLAGS -seglinkedit -segprot __TEXT rwx rwx
#define	CLIBS -lsys_s
#undef	AOUT
#endif

#ifdef	MSDOS
#include "dos.h"
#define	IEEEFLOAT
#define	DOWN_STACK
#define JB_SP 3
#define	BSD
#ifdef  __GO32__
# define DATA_START 0
# define COFF
#else	/* __EMX__ */
# define DATA_START 0x20000	/* 2 * SEGMENT_SIZE */
# define AOUT <a_out.h>
# define LDFLAGS -Zbsd-signals
#endif  __GO32__
#define	BRAND "IBM-PC"
#endif	MSDOS

#ifdef	OMRON
#define	IEEEFLOAT
#define	DATA_START 0
#define	DOWN_STACK
#define	SYSV
#define COFF
#define	BRAND "OMRON"
#endif	OMRON

#ifdef	SEQ
#define	IEEEFLOAT
#define	DATA_START 0
#define	DOWN_STACK
#define	BSD
#define	AOUT <a.out.h>
#define	BRAND "SEQUENT"
#endif	SEQ

#ifdef	sgi
#define	IEEEFLOAT
#define	DATA_START 0xA00000	/* normally 0x10000000 */
#define	DOWN_STACK
/* #define JB_SP 32 */
#define	BSD
#define	BRAND "SGI"
#define _BSD_SIGNALS
#define	LDFLAGS -Wl,-D -Wl,A00000
#define ILDFLAGS -T 0 -d -N
#define LSPCFLAGS -G 0
# ifdef SVR3			/* Irix Release 4 */
#define COFF
#define ECOFF
#define UNEXEC unexmips
# else
#define ELF
#define UNEXEC unexelfsgi
# endif
#endif	sgi

#ifdef	sun
#define	IEEEFLOAT
#define	DATA_START 0
#define	DOWN_STACK
#define	BSD
#define	BRAND "SUN"
#define LDFLAGS -Wl,-Bstatic

#ifdef	sun4sol2
#  define ELF
#  define UNEXEC unexelf
#elif	defined(sun386)
#  define COFF
#else
#  define AOUT <a.out.h>
#endif

#ifdef	mc68000
#  define JB_SP 14
#elif	defined(sparc)
#  define JB_SP 1
#  define JB_FP 3
#elif	defined(i386)
#  define JB_SP 2
#endif

#ifdef	sun4
#  define SETJMP	setjmpsparc.s
#  define sigsetjmp(x,y) _setjmp(x)
#  define siglongjmp(x,y) _longjmp(x,y)
#endif

#if defined(TCP) && defined(sun4sol2)
#  define CLIBS -lsocket -lnsl -lintl -Wl,-Bdynamic -ldl -Wl,-Bstatic
#endif

#endif	sun

#ifdef	TAHOE
#define	DATA_START 0
#define	DOWN_STACK
#define BSD
#define	AOUT <a.out.h>
#define	BRAND "TAHOE"
#endif	TAHOE

#ifdef	vax
#define	DATA_START 0
#define	DOWN_STACK
#define JB_SP 13
#define	BSD
#define AOUT <a.out.h>
#define	BRAND "DEC"
#endif	vax

#ifdef	__WIN32__
#define	IEEEFLOAT
#define	DATA_START 0
#define	DOWN_STACK
#define	BSD
# define PFI
# define UNEXEC unexec
#define	BRAND "IBM-PC"
#endif	__WIN32__

/**********************************************************************/
