/*  profile.c -- profiling tool						*/
/*
    Copyright (c) 1996, Giuseppe Attardi.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include "ecl.h"

/*
 *----------------------------------------------------------------------
 * Profiling tool
 * ----------------------------------------------------------------------
 */

extern caddr_t	*function_entry_table;
extern int	function_entries;

static object sSAprofile_arrayA;
static caddr_t profile_start;
static unsigned int profile_scale;

/*
 *----------------------------------------------------------------------
 * profile resolution start-address
 *   scale is a value between 0 and 16384:
 *	0 means stop profiling, other values represent the size of
 *	consecutive groups of instructions to which each tick is attributed
 *   Only (length *profile-array*) / (2 * scale) intructions from
 *   start-address are profiled.
 *----------------------------------------------------------------------
 */

extern int siLmake_vector (int narg, object etype, object dim, object adj, object fillp, object displ, object disploff, object staticp);
extern void profil (short unsigned int *, size_t, int, unsigned int);

siLprofile(int narg, object scale, object start_address)
{
  int size;
  object ar = sSAprofile_arrayA->symbol.dbind;
  if ((narg > 2) || (narg == 0))
    check_arg_failed(narg, 2);
  if (narg == 1)
    profile_start = (caddr_t)function_entry_table[0];
  else
    profile_start = (caddr_t)fixnnint(start_address);
  profile_scale = fixnnint(scale);
  if (type_of(ar) != t_vector) {
    size = (int)function_entry_table[(function_entries-1)*2]
      - (int)profile_start;
    size = size / (sizeof(int) * 2 * profile_scale);
    ar = siLmake_vector(7, Sfixnum, MAKE_FIXNUM(size),
		   Cnil, Cnil, Cnil, Cnil,
		   Ct);		/* static: must not be moved by GC */
    sSAprofile_arrayA->symbol.dbind = ar;
  }
  else
#   define	inheap(pp)	((unsigned long)(pp) < (unsigned long)heap_end)
    if (!inheap(ar->array.self.fix))
      FEerror("SI:*PROFILE-ARRAY* must be a static array", 0);
  if (profile_scale > 0)
    profile_scale = 65536 / ( 2 * profile_scale);
  profil((unsigned short *)ar->array.self.fix, ar->array.dim * sizeof(int),
	 profile_start, profile_scale);
#error "Not fixed"
  VALUES(0) = MAKE_FIXNUM(profile_start);
  RETURN(1);
}

siLclear_profile(int narg)
{
  int i;
  object ar = sSAprofile_arrayA->symbol.dbind;
  check_arg(0);
  if (type_of(ar) != t_vector)
    FEerror("SI:*PROFILE-ARRAY* must be an array of FIXNUM", 0);
  for (i = 0;  i < ar->array.dim;  i++)
      ar->array.self.fix[i] = 0;
}

total_ticks(unsigned short *aar, unsigned int dim)
{
  register unsigned short *endar = aar+dim;
  register int count = 0;
  for ( ; aar < endar; aar++)
    count += *aar;
  return count;
}

siLdisplay_profile(int narg)
{
  caddr_t prev, next;
  unsigned upto, dim;
  int i, j, scale, count, total;
  unsigned short *ar;
  object obj_ar = sSAprofile_arrayA->symbol.dbind;
  int groupSize = 0x20000 / profile_scale;

  check_arg(0);
  if (type_of(obj_ar) != t_vector)
    FEerror("si:*profile-array* not a vector", 0);
  ar = (unsigned short *)obj_ar->array.self.fix;
  dim = obj_ar->array.dim * (sizeof(int) / sizeof(short));

  total = total_ticks(ar, dim);

  for (i = 0; i < 2*function_entries; i += 2, prev = next) {
    prev = function_entry_table[i];
    if (prev < profile_start) continue;

    if (i+2 == 2*function_entries)
      upto = dim;
    else {
    next = function_entry_table[i+2];
      upto = (next > profile_start)
	? (next - profile_start) / groupSize : 0;
      if (upto >= dim) upto = dim;
    }
    count = 0;
    for (j = (prev - profile_start) / groupSize; j < upto; j++)
      count += ar[j];
    if (count > 0) {
      object name = (object)function_entry_table[i+1];
      printf("\n%6.2f%% (%5d): ", 100.0 * count/total, count);
      prin1(name, Cnil);
      fflush(stdout);
    }
    if (upto == dim) break;
  }
  printf("\nTotal ticks: %d\n", total); fflush(stdout);
  RETURN(0);
}
