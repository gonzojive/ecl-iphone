/*
    bind.c -- Lambda bindings.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECLS is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#include "ecls.h"
#include "ecls-inl.h"

#define NOT_YET         10
#define FOUND           11
#define NOT_KEYWORD     1

void
parse_key(
     int narg,			/* number of actual args */
     cl_object *args,		/* actual args */
     int nkey,			/* number of keywords */
     cl_object *keys,		/* keywords for the function */
     cl_object *vars,		/* where to put values (vars[0..nkey-1])
				   and suppliedp (vars[nkey..2*nkey-1]) */
     cl_object rest,		/* rest variable or NULL */
     bool allow_other_keys)	/* whether other key are allowed */
{ cl_object *p;
  int i;
  cl_object k;

  /* fill in the rest arg list */
  if (rest != OBJNULL)
    for (i = narg, p = args; i > 0; i--) {
      CAR(rest) = *p++;
      rest = CDR(rest);
    }

  for (i = 0; i < 2*nkey; i++)
    vars[i] = Cnil;             /* default values: NIL, supplied: NIL */
  if (narg <= 0) return;

  /* scan backwards, so that if a keyword is duplicated, first one is used */
  args = args + narg;
  top:
    while (narg >= 2) {
     args = args - 2;
     k = args[0];
     for (i = 0; i < nkey; i++) {
       if (keys[i] == k) {
	 vars[i] = args[1];
	 vars[nkey+i] = Ct;
	 narg = narg-2;
	 goto top;
       }
     }
     /* the key is a new one */
     if (allow_other_keys)
	 narg = narg-2;
     else {
       /* look for :allow-other-keys t */
       for ( i = narg-2, p = args; i >= 0; i -= 2, p -=2)
	 if (*p == Kallow_other_keys) {
	   allow_other_keys = (p[1] != Cnil); break;
	 }
       if (allow_other_keys) narg = narg-2;
       else FEerror("Unrecognized key ~a", 1, k);
     }
   }
  if (narg != 0) FEerror("Odd number of keys", 0);
}

/* Used in compiled macros */
void
check_other_key(cl_object l, int n, ...)
{
	cl_object other_key = OBJNULL;
	cl_object k;
	int i;
	bool allow_other_keys = FALSE;
	va_list ap;
	va_start(ap, n);                /* extracting arguments */

	for (;  !endp(l);  l = CDDR(l)) {
		k = CAR(l);
		if (!keywordp(k))
			FEerror("~S is not a keyword.", 1, k);
		if (endp(CDR(l)))
			FEerror("Odd number of arguments for keywords.", 0);
		if (k == Kallow_other_keys && CADR(l) != Cnil) {
			allow_other_keys = TRUE;
		} else {
#ifndef NO_ARG_ARRAY
		        cl_object *ktab = (cl_object *)ap;
			for (i = 0;  i < n;  i++)
				if (ktab[i] == k) {
				  ktab[i] = Cnil; /* remember seen */
				  break;
				}
			if (i >= n) other_key = k;
#else
			Rewrite this!
#endif NO_ARG_ARRAY
		}
	}
	if (other_key != OBJNULL && !allow_other_keys)
		FEerror("The keyword ~S is not allowed or is duplicated.",
			1, other_key);
}

void
init_bind(void)
{
	make_constant("LAMBDA-LIST-KEYWORDS",
	list(8, SAoptional, SArest, SAkey, SAallow_other_keys, SAaux,
		make_ordinary("&WHOLE"), make_ordinary("&ENVIRONMENT"), make_ordinary("&BODY")));

	make_constant("LAMBDA-PARAMETERS-LIMIT", MAKE_FIXNUM(64));
}
