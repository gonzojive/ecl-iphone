/*
    cmpaux.c -- Auxiliaries used in compiled Lisp code.
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

cl_object @'&optional';
cl_object @'&rest';
cl_object @'&key';
cl_object @'&allow-other-keys';
cl_object @'&aux';
cl_object @':allow-other-keys';

cl_object
make_list(int i)
{
  cl_object x = Cnil;
  while (i-- > 0)
    x = CONS(Cnil, x);
  return x;
}

@(defun si::specialp (sym)
@
	@(return ((SYMBOLP(sym) && sym->symbol.stype == stp_special) ?
		   Ct : Cnil))
@)

int
ifloor(int x, int y)
{
	if (y == 0)
		FEerror("Zero divizor", 0);
	else if (y > 0)
		if (x >= 0)
			return(x/y);
		else
			return(-((-x+y-1))/y);
	else
		if (x >= 0)
			return(-((x-y-1)/(-y)));
		else
			return((-x)/(-y));
}

int
imod(int x, int y)
{
	return(x - ifloor(x, y)*y);
}

/*
 * ----------------------------------------------------------------------
 *	Conversions to C
 * ----------------------------------------------------------------------
 */

char
object_to_char(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
		return fix(x);
	case t_bignum:
		return big_to_long(x) & (CHCODELIM - 1);
	case t_character:
		return CHAR_CODE(x);
	default:
		FEerror("~S cannot be coerced to a C char.", 1, x);
	}
}

int
object_to_int(cl_object x)
{
	switch (type_of(x)) {
	case t_fixnum:
		return fix(x);
	case t_character:
		return CHAR_CODE(x);
	case t_bignum:
		return big_to_long(x);
	case t_ratio:
		return number_to_double(x);
	case t_shortfloat:
		return sf(x);
	case t_longfloat:
		return lf(x);
	default:
		FEerror("~S cannot be coerced to a C int.", 1, x);
	}
}

char *
object_to_string(cl_object x)
{
  extern VOID *malloc(size_t size);
  switch (type_of(x)) {
  case t_string:
  case t_symbol:
    return(x->string.self);
  case t_fixnum: {
    char *num = malloc(12);
    sprintf(num, "%ld", (long)fix(x));
    return(num);
  }
  case t_character: {
    char *c = malloc(2);
    c[0] = CHAR_CODE(x);
    c[1] = '\0';
    return c;
  }
  case t_pathname:
    return namestring(x)->string.self;
  default:
    FEerror("~S cannot be coerced to a C string.", 1, x);
  }
}

float
object_to_float(cl_object x)
{
	if (FIXNUMP(x)) return(fix(x));	/* Immediate fixnum */

	switch (type_of(x)) {
	  /*	case t_fixnum: return fix(x);	*/
	case t_character:
		return CHAR_CODE(x);
	case t_bignum:
	case t_ratio:
		return number_to_double(x);
	case t_shortfloat:
		return sf(x);
	case t_longfloat:
		return lf(x);
	default:
		FEerror("~S cannot be coerced to a C float.", 1, x);
	}
}

double
object_to_double(cl_object x)
{

	if (FIXNUMP(x)) return(fix(x));	/* Immediate fixnum */

	switch (type_of(x)) {
	  /*	case t_fixnum: return fix(x);	*/
	case t_character:
		return CHAR_CODE(x);
	case t_bignum:
	case t_ratio:
		return number_to_double(x);
	case t_shortfloat:
		return sf(x);
	case t_longfloat:
		return lf(x);
	default:
		FEerror("~S cannot be coerced to a C double.", 1, x);
	}
}

int
aref_bv(cl_object x, cl_index index)
{
  index += x->vector.offset;
  return ((x->vector.self.bit[index/CHAR_BIT] & (0200>>index%CHAR_BIT)) != 0);
}

int
aset_bv(cl_object x, cl_index index, int value)
{
  index += x->vector.offset;
  if (value == 0)
    x->vector.self.bit[index/CHAR_BIT] &= ~(0200>>index%CHAR_BIT);
  else
    x->vector.self.bit[index/CHAR_BIT] |= 0200>>index%CHAR_BIT;
  return value;
}

void
throw(cl_object tag)
{
  frame_ptr fr = frs_sch_catch(tag);
  if (fr == NULL)
    FEcontrol_error("THROW: The catch ~S is undefined.", 1, tag);
  unwind(fr, tag);
}

void
return_from(cl_object block_id, cl_object block_name)
{
  frame_ptr fr = frs_sch(block_id);
  if (fr == NULL)
    FEcontrol_error("RETURN-FROM: The block ~S with id ~S is missing.",
		    2, block_name, block_id);
  unwind(fr, block_name);
}

void
go(cl_object tag_id, cl_object label)
{
  frame_ptr fr = frs_sch(tag_id);
  if (fr == NULL)
    FEcontrol_error("GO: The tagbody ~S is missing.", 1, tag_id);
  unwind(fr, label);
}

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
{
  int i;

  /* fill in the rest arg list */
  if (rest != OBJNULL) {
    cl_object *p = args;
    for (i = narg; i > 0; i--) {
      CAR(rest) = *(p++);
      rest = CDR(rest);
    }
  }

  for (i = 0; i < 2*nkey; i++)
    vars[i] = Cnil;             /* default values: NIL, supplied: NIL */
  if (narg <= 0) return;

  for (; narg>=2; narg-= 2) {
    cl_object keyword = *(args++);
    cl_object value = *(args++);
    for (i = 0; i < nkey; i++) {
      if (keys[i] == keyword) {
	if (vars[nkey+i] == Cnil) {
	  vars[i] = value;
	  vars[nkey+i] = Ct;
	}
	goto go_on;
      }
    }
    /* the key is a new one */
    if (!allow_other_keys) {
      if (keyword == @':allow-other-keys')
	allow_other_keys = (value != Cnil);
      else {
	cl_object *p = args;
	/* look for :allow-other-keys t */
	for (i = narg-2; i >= 0; i -= 2, p -=2)
	  if (*(p++) == @':allow-other-keys') {
	    allow_other_keys = (*(p++) != Cnil);
	    break;
	  }
	if (!allow_other_keys)
	  FEprogram_error("Unrecognized key ~a", 1, keyword);
      }
    }
  go_on:
  }
  if (narg != 0) FEprogram_error("Odd number of keys", 0);
}

#ifdef NO_ARGS_ARRAY
void
va_parse_key(
     int narg,			/* number of actual args */
     va_list args,		/* actual args */
     int nkey,			/* number of keywords */
     cl_object *keys,		/* keywords for the function */
     cl_object *vars,		/* where to put values (vars[0..nkey-1])
				   and suppliedp (vars[nkey..2*nkey-1]) */
     cl_object rest,		/* rest variable or NULL */
     bool allow_other_keys)	/* whether other key are allowed */
{
  int i;

  /* fill in the rest arg list */
  if (rest != OBJNULL) {
    va_list p = args;
    for (i = narg; i > 0; i--) {
      CAR(rest) = cl_nextarg(p);
      rest = CDR(rest);
    }
  }

  for (i = 0; i < 2*nkey; i++)
    vars[i] = Cnil;             /* default values: NIL, supplied: NIL */
  if (narg <= 0) return;

  for (; narg>=2; narg-= 2) {
    cl_object keyword = cl_nextarg(args);
    cl_object value = cl_nextarg(args);
    for (i = 0; i < nkey; i++) {
      if (keys[i] == keyword) {
	if (vars[nkey+i] == Cnil) {
	  vars[i] = value;
	  vars[nkey+i] = Ct;
	}
	goto go_on;
      }
    }
    /* the key is a new one */
    if (!allow_other_keys) {
      if (keyword == @':allow-other-keys')
	allow_other_keys = (value != Cnil);
      else {
	va_list p = args;
	/* look for :allow-other-keys t */
	for (i = narg-2; i >= 0; i -= 2, p -=2)
	  if (cl_nextarg(p) == @':allow-other-keys') {
	    allow_other_keys = (cl_nextarg(p) != Cnil);
	    break;
	  }
	if (!allow_other_keys)
	  FEprogram_error("Unrecognized key ~a", 1, keyword);
      }
    }
  go_on:
  }
  if (narg != 0) FEprogram_error("Odd number of keys", 0);
}
#endif NO_ARGS_ARRAY

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
			FEprogram_error("~S is not a keyword.", 1, k);
		if (endp(CDR(l)))
			FEprogram_error("Odd number of arguments for keywords.",0);
		if (k == @':allow-other-keys' && CADR(l) != Cnil) {
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
		FEprogram_error("The keyword ~S is not allowed or is duplicated.",
				1, other_key);
}

void
init_cmpaux(void)
{
	make_constant("LAMBDA-LIST-KEYWORDS",
	list(8, @'&optional', @'&rest', @'&key', @'&allow-other-keys', @'&aux',
		make_ordinary("&WHOLE"), make_ordinary("&ENVIRONMENT"), make_ordinary("&BODY")));

	make_constant("LAMBDA-PARAMETERS-LIMIT", MAKE_FIXNUM(64));
}
