dnl --------------------------------------------------------------
dnl Set up a configuration file for the case when we are cross-
dnl compiling
dnl
AC_DEFUN(ECL_CROSS_CONFIG,[
if test "x${cross_compiling}" = "xyes"; then
  if test -n "${cross_config}" -a -f "${cross_config}"; then
    . ${cross_config}
  elif test -f ./cross_config; then
    . ./cross_config
  elif test -n "${srcdir}" -a -f ${srcdir}/cross_config; then
    . ${srcdir}/cross_config
  else
    test -z ${cross_config} && cross_config=`pwd`/cross_config
    cat > ${cross_config} <<EOF
###
### YOU ARE TRYING TO CROSS COMPILE ECL.
### PLEASE FOLLOW THESE INSTRUCTIONS:
###
### 1) Vital information cannot be determined at configuration time
### because we are not able to run test programs. A file called
###		${cross_config}
### has been created, that you will have to fill out. Please do
### it before invoking "configure" again.

### 1.1) Direction of growth of the stack
ECL_STACK_DIR=up

### 1.2) Choose an integer datatype which is large enough to host a pointer
CL_FIXNUM_TYPE=int
CL_FIXNUM_BITS=32
CL_FIXNUM_MAX=536870911L
CL_FIXNUM_MIN=-536870912L

### 1.3) Order of bytes within a word
ECL_BIGENDIAN=no

### 1.4) What characters signal an end of line. May be LF (Linefeed or \\n)
###      CR (Carriage return or \\r), and CRLF (CR followed by LF).
ECL_NEWLINE=LF

### 2) To cross-compile ECL so that it runs on the system
###		${host}
### you need to first compile ECL on the system in which you are building
### the cross-compiled files, that is
###		${build}
### By default we assume that ECL can be accessed from some directory in
### the path.
ECL_TO_RUN=`which ecl`
EOF
    cat ${cross_config}
    AC_MSG_ERROR(Configuration aborted)
  fi
  if test "${ECL_TO_RUN}" = "failed"; then
    AC_MSG_ERROR(The program ECL is not installed in your system)
  fi
  DPP_TO_RUN=`${ECL_TO_RUN} -eval '(progn (print (truename "sys:dpp")) (quit))' \
	| grep '\#\P' | sed 's,#P"\(.*\)",\1,'`
  if test -z "${DPP_TO_RUN}" -o "${DPP_TO_RUN}" = "failed"  ; then
    AC_MSG_ERROR(The program DPP is not installed in your system)
  fi
  (echo '#!/bin/sh'; echo ${ECL_TO_RUN} -eval "'"'(push :cross *features*)'"'" '$''*') > CROSS-COMPILER
  (echo '#!/bin/sh'; echo ${DPP_TO_RUN} '$''*') > CROSS-DPP
  chmod +x CROSS-COMPILER CROSS-DPP
fi
])

dnl --------------------------------------------------------------
dnl Make srcdir absolute, if it isn't already.  It's important to
dnl avoid running the path through pwd unnecessarily, since pwd can
dnl give you automounter prefixes, which can go away.
dnl
AC_DEFUN(ECL_MAKE_ABSOLUTE_SRCDIR,[
if uname -a | grep -i 'mingw32' > /dev/null; then
  PWDCMD="pwd -W";
else
  PWDCMD="pwd";
fi
case "${srcdir}" in
  /* | ?:/* ) ;;
  *  ) srcdir="`(cd ${srcdir}; ${PWDCMD})`";
esac
])

dnl
dnl --------------------------------------------------------------
dnl Define a name for this operating system and set some defaults
dnl
AC_DEFUN(ECL_GUESS_HOST_OS,[
#### Some command variations:
AC_SUBST(CP)
AC_SUBST(RM)
AC_SUBST(MV)
AC_SUBST(EXE_SUFFIX)
RM="rm -f"
CP="cp"
MV="mv"

### Guess the operating system
AC_SUBST(ARCHITECTURE)dnl	Type of processor for which this is compiled
AC_SUBST(SOFTWARE_TYPE)dnl	Type of operating system
AC_SUBST(SOFTWARE_VERSION)dnl	Version number of operating system
AC_SUBST(MACHINE_INSTANCE)dnl	Identifier of the machine
AC_SUBST(MACHINE_VERSION)dnl	Version of the machine
SOFTWARE_TYPE="${host_os}"
SOFTWARE_VERSION="unknown"
MACHINE_INSTANCE="${host_cpu}"
MACHINE_VERSION="unknown"
ARCHITECTURE=`echo "${host_cpu}" | tr a-z A-Z` # i386 -> I386


AC_SUBST(LDRPATH)dnl	Sometimes the path for finding DLLs must be hardcoded.
AC_SUBST(LIBPREFIX)dnl	Name components of a statically linked library
AC_SUBST(LIBEXT)
AC_SUBST(SHAREDEXT)dnl	Name components of a dynamically linked library
AC_SUBST(SHAREDPREFIX)
AC_SUBST(OBJEXT)dnl	These are set by autoconf
AC_SUBST(EXEEXT)
LDRPATH='~*'
SHAREDEXT='so'
SHAREDPREFIX='lib'
LIBPREFIX='lib'
LIBEXT='a'
case "${host_os}" in
	# libdir may have a dollar expression inside
	linux*)
		thehost="linux"
		SHARED_LDFLAGS="-shared"
		LDRPATH="-Wl,--rpath,~A"
		CLIBS="-ldl"
		# Maybe CFLAGS="-D_ISOC99_SOURCE ${CFLAGS}" ???
		;;
	freebsd*)
		thehost="freebsd"
		CLIBS="-lcompat"
		SHARED_LDFLAGS="-shared"
		LDRPATH="-Wl,--rpath,~A"
		;;
	netbsd*)
		thehost="netbsd"
		SHARED_LDFLAGS="-shared"
		LDRPATH="-Wl,--rpath,~A"
		CLIBS="-lcompat"
		;;
	solaris*)
		thehost="sun4sol2"
		SHARED_LDFLAGS="-dy -G"
		LDRPATH="-Wl,--rpath,~A"
		TCPLIBS="-lsocket -lnsl -lintl"
		CLIBS="-ldl"
		;;
	cygwin*)
		thehost="cygwin"
		shared="no"
		SHAREDEXT='dll'
		;;
	darwin*)
		thehost="darwin"
		shared="no"
		;;
	*)
		thehost="$host_os"
		shared="no"
		;;
esac
CFLAGS="${CFLAGS} -D${thehost}"
AC_MSG_CHECKING(for ld flags when building shared libraries)
if test "${shared}" = "yes"; then
AC_MSG_RESULT([${SHARED_LDFLAGS}])
else
shared="no";
AC_MSG_RESULT(cannot build)
fi
AC_MSG_CHECKING(for required libraries)
AC_MSG_RESULT([${CLIBS}])
AC_MSG_CHECKING(for architecture)
AC_MSG_RESULT([${ARCHITECTURE}])
AC_MSG_CHECKING(for software type)
AC_MSG_RESULT([${SOFTWARE_TYPE}])
])

dnl
dnl --------------------------------------------------------------
dnl Check the direction to which the stack grows (for garbage
dnl collection).
dnl
AC_DEFUN(ECL_STACK_DIRECTION,[
  AC_MSG_CHECKING(whether stack growns downwards)
if test -z "${ECL_STACK_DIR}" ; then
  AC_TRY_RUN([
char *f2() {
  char c;
  return &c;
}

int f1() {
  char d;
  return f2() - &d;
}

int main() {
  if (f1() > 0)
    return 1;
  else
    return 0;
}
],
ECL_STACK_DIR=down,
ECL_STACK_DIR=up,[])
fi
case "${ECL_STACK_DIR}" in
  down|DOWN) AC_MSG_RESULT(yes); AC_DEFINE(DOWN_STACK) ;;
  up|UP) AC_MSG_RESULT(no) ;;
  *) AC_MSG_ERROR(Unable to determine stack growth direction)
esac])
dnl
dnl ------------------------------------------------------------
dnl Find out a setjmp() that does not save signals. It is called
dnl in several architectures.
AC_DEFUN(ECL_FIND_SETJMP,[
AC_SUBST(ECL_SETJMP)
AC_SUBST(ECL_LONGJMP)
AC_CHECK_FUNC(_setjmp,
ECL_SETJMP="setjmp";ECL_LONGJMP="longjmp",
ECL_SETJMP="_setjmp";ECL_LONGJMP="_longjmp")])

dnl
dnl --------------------------------------------------------------
dnl Guess the right type and size for cl_fixnum. It must be large
dnl enough that convertion back and forth to pointer implies no
dnl loss of information.
AC_DEFUN(ECL_FIXNUM_TYPE,[
AC_SUBST(CL_FIXNUM_TYPE)
AC_SUBST(CL_FIXNUM_BITS)
AC_SUBST(CL_FIXNUM_MAX)
AC_SUBST(CL_FIXNUM_MIN)
AC_MSG_CHECKING(appropiate type for fixnums)
if test -z "${CL_FIXNUM_TYPE}" ; then
  AC_TRY_RUN([#include <stdio.h>
int main() {
  const char *int_type;
  int bits;
  FILE *f=fopen("conftestval", "w");
  if (!f) exit(1);
  if (sizeof(int) >= sizeof(void*)) {
    unsigned int t = 1;
    signed int l = 0;
    int_type="int";
    for (bits=1; ((t << 1) >> 1) == t; bits++, t <<= 1);
    l = (~l) << (bits - 3);
    fprintf(f,"CL_FIXNUM_MIN='%d';",l);
    fprintf(f,"CL_FIXNUM_MAX='%d';",-(l+1));
  } else if (sizeof(long) >= sizeof(void*)) {
    unsigned long int t = 1;
    signed long int l = 0;
    int_type="long int";
    for (bits=1; ((t << 1) >> 1) == t; bits++, t <<= 1);
    l = (~l) << (bits - 3);
    fprintf(f,"CL_FIXNUM_MIN='%ld';",l);
    fprintf(f,"CL_FIXNUM_MAX='%ld';",-(l+1));
  } else
    exit(1);
  fprintf(f,"CL_FIXNUM_TYPE='%s';",int_type);
  fprintf(f,"CL_FIXNUM_BITS='%d'",bits);
  exit(0);
}],
eval "`cat conftestval`",[])
fi
if test -z "${CL_FIXNUM_TYPE}" ; then
AC_MSG_ERROR(There is no appropiate integer type for the cl_fixnum type)
fi
AC_MSG_RESULT([${CL_FIXNUM_TYPE}])])

dnl
dnl ------------------------------------------------------------
dnl Find out what is written for every '\n' character, when
dnl opening a text file.
dnl
AC_DEFUN(ECL_LINEFEED_MODE,[
AC_MSG_CHECKING(character sequence for end of line)
if test -z "${ECL_NEWLINE}" ; then
AC_TRY_RUN([#include <stdio.h>
int main() {
  FILE *f = fopen("conftestval","w");
  int c1, c2;
  char *output;
  if (f == NULL) exit(1);
  fprintf(f, "\n");
  fclose(f);
  f = fopen("conftestval","rb");
  if (f == NULL) exit(1);
  c1 = fgetc(f);
  c2 = fgetc(f);
  fclose(f);
  f = fopen("conftestval","w");
  if (f == NULL) exit(1);
  if (c1 == '\r')
    if (c2 == EOF)
      output="CR";
    else
      output="CRLF";
  else
    output="LF";
  fclose(f);
  f = fopen("conftestval","w");
  if (f == NULL) exit(1);
  fprintf(f, output);
  fclose(f);
  exit(0);
}
],
ECL_NEWLINE=`cat conftestval`,[],[])
fi
case "${ECL_NEWLINE}" in
  LF) AC_MSG_RESULT(lf) ;;
  CR) AC_MSG_RESULT(cr); AC_DEFINE(ECL_NEWLINE_IS_CR) ;;
  CRLF) AC_MSG_RESULT(cr+lf); AC_DEFINE(ECL_NEWLINE_IS_CRLF) ;;
  *) AC_MSG_ERROR(Unable to determine linefeed mode) ;;
esac
])

