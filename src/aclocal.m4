dnl --------------------------------------------------------------
dnl Make srcdir absolute, if it isn't already.  It's important to
dnl avoid running the path through pwd unnecessarily, since pwd can
dnl give you automounter prefixes, which can go away.
dnl
AC_DEFUN(ECL_MAKE_ABSOLUTE_SRCDIR,[
case "${srcdir}" in
  /* ) ;;
  . )
    ## We may be able to use the $PWD environment variable to make this
    ## absolute.  But sometimes PWD is inaccurate.
    if [ "${PWD}" != "" ] && [ "`(cd ${PWD} ; sh -c pwd)`" = "`pwd`" ] ; then
      srcdir="$PWD"
    else
      srcdir="`(cd ${srcdir}; pwd)`"
    fi
  ;;
  *  ) srcdir="`(cd ${srcdir}; pwd)`" ;;
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
AC_SUBST(MACHINE_INSTANCE)
AC_SUBST(MACHINE_VERSION)
AC_SUBST(OS_TYPE)
AC_SUBST(OS_RELEASE)
MACHINE_INSTANCE=`uname -m`
MACHINE_VERSION=`uname -r`
OS_TYPE=`uname -s`
OS_RELEASE=`uname -r`
case $host_os in
	linux*)
		host="linux"
		;;
	freebsd*)
		host="freebsd"
		;;
	netbsd*)
		host="netbsd"
		;;
	solaris*)
		host="sun4sol2"
		;;
	cygwin*)
		host="cygwin"
		;;
	darwin*)
		host="darwin"
		;;
	*)
		host="$host_os"
		;;
esac
])
dnl
dnl --------------------------------------------------------------
dnl Extract some information from the machine description file.
dnl WARNING: file confdefs.h may depend on version of Autoconf
dnl
AC_DEFUN(ECL_PROCESS_MACHINES_H,[
[
echo "Extracting parameters from the machine description file"

tempcname="conftest.c"

echo '
#include "confdefs.h"
#include "'${srcdir}'/h/machines.h"
#ifdef CFLAGS
configure___CFLAGS=CFLAGS
#endif

#ifdef LSPCFLAGS
configure___LSPCFLAGS=LSPCFLAGS
#endif

#ifdef CLIBS
configure___CLIBS=CLIBS
#endif

#ifdef LDFLAGS
configure___LDFLAGS=LDFLAGS
#endif

#ifdef SHARED_LDFLAGS
configure___SHARED_LDFLAGS=SHARED_LDFLAGS
#endif

configure___architecture=ARCHITECTURE
configure___software_type=SOFTWARE_TYPE
configure___software_version=SOFTWARE_VERSION
' > ${tempcname}

# The value of CPP is a quoted variable reference, so we need to do this
# to get its actual value...
CPP=`eval "echo $CPP"`
eval `${CPP} -D${host} ${tempcname} \
       | grep 'configure___' \
       | sed -e 's/^configure___\([^=]*=\)[ ]*\(.*[^ ]\) */\1"\2"/'`
rm ${tempcname}
]
AC_MSG_CHECKING(for ld flags when building shared libraries)
if test "${shared}" = "yes" -a "${SHARED_LDFLAGS}" ; then
AC_MSG_RESULT([${SHARED_LDFLAGS}])
else
shared="no";
AC_MSG_RESULT(cannot build)
fi
AC_MSG_CHECKING(for required libraries)
AC_MSG_RESULT([${CLIBS}])
AC_MSG_CHECKING(for architecture)
AC_MSG_RESULT([${architecture}])
AC_MSG_CHECKING(for software type)
AC_MSG_RESULT([${software_type}])
AC_MSG_CHECKING(for software version)
AC_MSG_RESULT([${software_version}])
])
dnl
dnl --------------------------------------------------------------
dnl Check the direction to which the stack grows (for garbage
dnl collection).
dnl
AC_DEFUN(ECL_STACK_DIRECTION,[
  AC_MSG_CHECKING(whether stack growns downwards)
  AC_SUBST(DOWN_STACK)
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
AC_MSG_RESULT(yes)
AC_DEFINE(DOWN_STACK),
AC_MSG_RESULT(no))])
dnl
dnl --------------------------------------------------------------
dnl Check whether we can access the values in va_list() as an
dnl ordinary C vector.
dnl
AC_DEFUN(ECL_ARGS_ARRAY,[
  AC_MSG_CHECKING(if arguments can be accessed through vector)
  AC_TRY_RUN([
#include <stdarg.h>
#include <stdlib.h>
int f(int narg, ...) {
  va_list args;
  int *vector;
  va_start(args,narg);
  vector = NULL;
  while (narg--) {
    if (vector == NULL) {
      vector = &va_arg(args, int);
    } else if (*(++vector) != va_arg(args,int)) {
      return 1;
    }
  }
  return 0;
}

int main() {
  exit(f(10,1,2,3,4,5,6,7,8,9,10));
}
],
AC_MSG_RESULT(yes),
AC_MSG_RESULT(no)
AC_DEFINE(NO_ARGS_ARRAY))])
dnl
dnl --------------------------------------------------------------
dnl Guess the right type and size for cl_fixnum. It must be large
dnl enough that convertion back and forth to pointer implies no
dnl loss of information.
AC_DEFUN(ECL_FIXNUM_TYPE,[
AC_MSG_CHECKING(appropiate type for fixnums)
dnl
dnl 1.- Guess the type of a fixnum
dnl
AC_TRY_RUN([#include <stdio.h>
int main() {
  FILE *f=fopen("conftestval", "w");
  if (!f) exit(1);
  if (sizeof(int) >= sizeof(void*)) {
    fprintf(f, "int\n");
  } else if (sizeof(long) >= sizeof(void*)) {
    fprintf(f, "long\n");
  } else
    exit(1);
  exit(0);
}],
cl_fixnum=`cat conftestval`
AC_MSG_RESULT([${cl_fixnum}])
AC_DEFINE_UNQUOTED(CL_FIXNUM_TYPE,${cl_fixnum}),
AC_MSG_ERROR(There is no appropiate integer type for the cl_fixnum type))
dnl
dnl 2.- Guess the size of a fixnum
dnl
AC_MSG_CHECKING(most positive fixnum)
AC_TRY_RUN([#include <stdio.h>
int main() {
  ${cl_fixnum} i=0,j,k;
  FILE *f=fopen("conftestval", "w");
  if (!f) exit(1);
  do {
    k = i * 2 + 1;
    if (k == i) exit(1);
    i = k;
    j = (i << 2) >> 2;
    fprintf(stderr,"%d\n",i);
  } while (i == j);
  fprintf(f, "0x%xL\n", (long)i >> 1);
  exit(0);
}],
cl_fixnum_limit=`cat conftestval`
AC_MSG_RESULT([${cl_fixnum_limit}])
AC_DEFINE_UNQUOTED(MOST_POSITIVE_FIXNUM, ${cl_fixnum_limit}),
AC_MSG_ERROR(There is no appropiate integer type for the cl_fixnum type))
])
