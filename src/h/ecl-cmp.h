/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    ecl-cmp.h  -- Include file for compiled code.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or modify it under
    the terms of the GNU General Library Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <math.h> /* for inline mathematics */

#undef cs_check
#define	cs_check \
	if ((int *)(&narg) < cs_limit) \
		cs_overflow()

#define TRAMPOLINK(narg, vv, lk, cblock) \
	cl_va_list args; cl_va_start(args, narg, narg, 0); \
	return(_ecl_link_call(vv, (cl_objectfn *)lk, cblock, narg, args))
