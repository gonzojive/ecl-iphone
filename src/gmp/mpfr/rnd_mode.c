/* mpfr_set_machine_rnd_mode -- set the rounding mode for machine floats

Copyright 1999, 2001, 2002, 2004 Free Software Foundation, Inc.

This file is part of the MPFR Library.

The MPFR Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The MPFR Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the MPFR Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA. */

#include <stdio.h>
#include <stdlib.h>
#include "gmp.h"
#include "gmp-impl.h"
#include "mpfr.h"

#ifdef MPFR_HAVE_FESETROUND
#include <fenv.h>

/* sets the machine rounding mode to the value rnd_mode

   Doing an exit(0) when an FE mode is not available is a nasty hack.  It's
   done to let the test programs stop gracefully when they attempt an
   unsupported mode.  This happens for instance on ARM systems, which lack
   FE_TOWARDZERO.

   Applications which have been using this function are not harmed by this
   hack.  In the past the code didn't even compile on systems with an
   incomplete set of FE choices.  Applications won't want to be using this
   anyway, it's been moved to the test suite in newer mpfr.  */

void 
mpfr_set_machine_rnd_mode (mp_rnd_t rnd_mode)
{
  switch (rnd_mode) {

  case GMP_RNDN:
#ifdef FE_TONEAREST
    fesetround(FE_TONEAREST);
    return;
#else
    break;
#endif

  case GMP_RNDZ:
#ifdef FE_TOWARDZERO
    fesetround(FE_TOWARDZERO);
    return;
#else
    break;
#endif

  case GMP_RNDU:
#ifdef FE_UPWARD
    fesetround(FE_UPWARD);
    return;
#else
    break;
#endif

  case GMP_RNDD:
#ifdef FE_DOWNWARD
    fesetround(FE_DOWNWARD);
    return;
#else
    break;
#endif

  default:
    fprintf(stderr, "invalid rounding mode\n");
    exit(1);
  }

  printf ("mpfr_set_machine_rnd_mode(): rounding mode %d not available, exiting\n", rnd_mode);
  exit (0);
}
#endif
