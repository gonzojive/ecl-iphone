/*
    array.c --  Array routines
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

#ifndef CHAR_BIT
#define CHAR_BIT (sizeof(char)*8)
#endif

static void displace (cl_object from, cl_object to, cl_object offset);
static void check_displaced (cl_object dlist, cl_object orig, cl_index newdim);
extern enum aelttype get_aelttype (cl_object x);

cl_index
object_to_index(cl_object n)
{
	switch (type_of(n)) {
	case t_fixnum: {
		cl_fixnum out = fix(n);
		if (out < 0 || out >= ADIMLIM)
			FEtype_error_index(n);
		return out;
	}
	case t_bignum:
		FEtype_error_index(n);
	default:
		FEtype_error_integer(n);
	}
}

@(defun row-major-aref (x indx)
  cl_index j;
@
  j = fixnnint(indx);
  @(return aref(x, j))
@)

@(defun si::row-major-aset (x indx val)
  cl_index j;
@
  j = fixnnint(indx);
  @(return aset(x, j, val))
@)

@(defun aref (x &rest indx)
  cl_index r, s, i, j;
  cl_object index;
@
  r = narg - 1;
  switch (type_of(x)) {
  case t_array:
    if (r != x->array.rank)
      FEerror("Wrong number of indices.", 0);
    for (i = j = 0;  i < r;  i++) {
      index = cl_nextarg(indx);
      if ((s = fixnnint(index)) >= x->array.dims[i])
        FEerror("The ~:R index, ~S, to the array~%\
~S is too large.", 3, MAKE_FIXNUM(i+1), index, x);
      j = j*(x->array.dims[i]) + s;
    }
    break;

  case t_vector:
  case t_string:
  case t_bitvector:
    if (r != 1)
      FEerror("Wrong number of indices.", 0);
    index = cl_nextarg(indx);
    j = fixnnint(index);
    if (j >= x->vector.dim)
      FEerror("The first index, ~S, to the array ~S is too large.",
              2, index, x);
    break;

  default:
    FEwrong_type_argument(@'array', x);
  }
  @(return aref(x, j))
@)

cl_object
aref(cl_object x, cl_index index)
{
  if (index >= x->array.dim)
    FEerror("The index, ~D, is too large.", 1, MAKE_FIXNUM(index));
  switch ((enum aelttype)array_elttype(x)) {
  case aet_object:
    return(x->array.self.t[index]);

  case aet_ch:
    return(code_char(x->string.self[index]));

  case aet_bit:
    index += x->vector.offset;
    if (x->vector.self.bit[index/CHAR_BIT] & (0200>>index%CHAR_BIT))
      return(MAKE_FIXNUM(1));
    else
      return(MAKE_FIXNUM(0));
  case aet_fix:
    return(MAKE_FIXNUM(x->array.self.fix[index]));

  case aet_sf:
    return(make_shortfloat(x->array.self.sf[index]));

  case aet_lf:
    return(make_longfloat(x->array.self.lf[index]));

  default:
    internal_error("aref");
  }
}

cl_object
aref1(cl_object v, cl_index index)
{
  switch (type_of(v)) {
  case t_vector:
  case t_bitvector:
    return(aref(v, index));

  case t_string:
    if (index >= v->string.dim)
      FEerror("The index, ~D, is too large.", 1, MAKE_FIXNUM(index));
    return(code_char(v->string.self[index]));

  default:
    FEerror("~S is not a vector.", 1, v);
  }
}

/*
	Internal function for setting array elements:

		(si:aset value array dim0 ... dimN)
*/
@(defun si::aset (v x &rest dims)
  cl_index r, s, i, j;
  cl_object index;
@
  r = narg - 2;
  switch (type_of(x)) {
  case t_array:
    if (r != x->array.rank)
      FEerror("Wrong number of indices.", 0);
    for (i = j = 0;  i < r;  i++) {
      index = cl_nextarg(dims);
      if ((s = fixnnint(index)) >= x->array.dims[i])
	FEerror("The ~:R index, ~S, to the array ~S is too large.",
		3, MAKE_FIXNUM(i+1), index, x);
      j = j*(x->array.dims[i]) + s;
    }
    break;

  case t_vector:
  case t_string:
  case t_bitvector:
    if (r != 1)
      FEerror("Wrong number of indices.", 0);
    index = cl_nextarg(dims);
    j = fixnnint(index);
    if (j >= x->vector.dim)
      FEerror("The first index, ~S, to the array ~S is too large.",
	      2, index, x);
    break;

  default:
    FEwrong_type_argument(@'array', x);
  }
  @(return aset(x, j, v))
@)

cl_object
aset(cl_object x, cl_index index, cl_object value)
{
  if (index >= x->array.dim)
    FEerror("The index, ~D, too large.", 1, MAKE_FIXNUM(index));
  switch (array_elttype(x)) {
  case aet_object:
    x->array.self.t[index] = value;
    break;

  case aet_ch:
    /* INV: char_code() checks the type of `value' */
    x->string.self[index] = char_code(value);
    break;

  case aet_bit: {
    cl_fixnum i = fixint(value);
    if (i != 0 && i != 1)
      FEerror("~S is not a bit.", 1, value);
    index += x->vector.offset;
    if (i == 0)
      x->vector.self.bit[index/CHAR_BIT] &= ~(0200>>index%CHAR_BIT);
    else
      x->vector.self.bit[index/CHAR_BIT] |= 0200>>index%CHAR_BIT;
    break;
  }
  case aet_fix:
    x->array.self.fix[index] = fixint(value);
    break;

  case aet_sf:
    x->array.self.sf[index] = object_to_float(value);
    break;

  case aet_lf:
    x->array.self.lf[index] = object_to_double(value);
    break;
  }
  return(value);
}

cl_object
aset1(cl_object v, cl_index index, cl_object val)
{
  switch (type_of(v)) {
  case t_vector:
  case t_bitvector:
    return(aset(v, index, val));

  case t_string:
    if (index >= v->string.dim)
      FEerror("The index, ~D, is too large", 1, MAKE_FIXNUM(index));
    /* INV: char_code() checks the type of `val' */
    v->string.self[index] = char_code(val);
    return(val);

  default:
    FEerror("~S is not a vector.", 1, v);
  }

}

/*
	Internal function for making arrays of more than one dimension:

		(si:make-pure-array element-type adjustable
			            displaced-to displaced-index-offset
			            dim0 dim1 ... )
*/
@(defun si::make_pure_array (etype adj displ disploff &rest dims)
  cl_index r, s, i, j;
  cl_object x;
@
  r = narg - 4;
  x = alloc_object(t_array);
  x->array.displaced = Cnil;
  x->array.self.t = NULL;		/* for GC sake */
  x->array.rank = r;
  x->array.elttype = (short)get_aelttype(etype);
  x->array.dims = alloc_atomic_align(sizeof(int)*r, sizeof(int));
  if (r >= ARANKLIM)
    FEerror("The array rank, ~R, is too large.", 1, MAKE_FIXNUM(r));
  for (i = 0, s = 1;  i < r;  i++) {
    cl_object index = cl_nextarg(dims);
    if ((j = fixnnint(index)) > ADIMLIM)
      FEerror("The ~:R array dimension, ~D, is too large.",
	      2, MAKE_FIXNUM(i+1), index);
    s *= (x->array.dims[i] = j);
    if (s > ATOTLIM)
      FEerror("The array total size, ~D, is too large.", 1, MAKE_FIXNUM(s));
  }
  x->array.dim = s;
  x->array.adjustable = adj != Cnil;
  if (Null(displ))
    array_allocself(x);
  else
    displace(x, displ, disploff);
  @(return x)
@)

/*
	Internal function for making vectors:

		(si:make-vector element-type dimension adjustable fill-pointer
				displaced-to displaced-index-offset)
*/
@(defun si::make_vector (etype dim adj fillp displ disploff)
  cl_index d, f;
  cl_object x;
  enum aelttype aet;
@
  aet = get_aelttype(etype);
  if ((d = fixnnint(dim)) > ADIMLIM)
    FEerror("The vector dimension, ~D, is too large.", 1, dim);
  f = d;
  if (aet == aet_ch) {
    x = alloc_object(t_string);
    d++;			/* extra for null terminator */
  }
  else if (aet == aet_bit)
    x = alloc_object(t_bitvector);
  else {
    x = alloc_object(t_vector);
    x->vector.elttype = (short)aet;
  }
  x->vector.self.t = NULL;		/* for GC sake */
  x->vector.displaced = Cnil;
  x->vector.dim = d;
  x->vector.adjustable = adj != Cnil;

  if (Null(fillp))
    x->vector.hasfillp = FALSE;
  else if (fillp == Ct)
    x->vector.hasfillp = TRUE;
  else if ((f = fixnnint(fillp)) > d)
    FEerror("The fill-pointer ~S is too large.", 1, fillp);
  else
    x->vector.hasfillp = TRUE;
  x->vector.fillp = f;

  if (Null(displ))
    array_allocself(x);
  else
    displace(x, displ, disploff);
  @(return x)
@)

void
array_allocself(cl_object x)
{
	cl_index i, d;

	d = x->array.dim;
#ifdef THREADS
	start_critical_section(); /* avoid losing elts */
#endif THREADS
	switch (array_elttype(x)) {

	/* assign self field only after it has been filled, for GC sake  */
	case aet_object: {
		cl_object *elts;
		elts = alloc_align(sizeof(cl_object)*d, sizeof(cl_object));
		for (i = 0;  i < d;  i++)
			elts[i] = Cnil;
		x->array.self.t = elts;
		break;
	      }
	case aet_ch: {
		char *elts;
		elts = alloc_atomic(d);
		for (i = 0;  i < d;  i++)
			elts[i] = ' ';
		if (type_of(x) == t_string) elts[d-1] = '\0';
		x->string.self = elts;
		break;
	      }
	case aet_bit: {
		char *elts;
		d = (d+(CHAR_BIT-1))/CHAR_BIT;
		elts = alloc_atomic(d);
		for (i = 0;  i < d;  i++)
			elts[i] = '\0';
		x->vector.offset = 0;
		x->vector.self.bit = elts;
		break;
	      }
	case aet_fix: {
		cl_fixnum *elts;
		elts = alloc_atomic_align(sizeof(cl_fixnum)*d, sizeof(cl_fixnum));
		for (i = 0;  i < d;  i++)
			elts[i] = 0;
		x->array.self.fix = elts;
		break;
	      }
	case aet_sf: {
		float *elts;
		elts = alloc_atomic_align(sizeof(float)*d, sizeof(float));
		for (i = 0;  i < d;  i++)
			elts[i] = 0.0;
		x->array.self.sf = elts;
		break;
	      }
	case aet_lf: {
		double *elts;
		elts = alloc_atomic_align(sizeof(double)*d, sizeof(double));
		for (i = 0;  i < d;  i++)
			elts[i] = 0.0;
		x->array.self.lf = elts;
		break;
	      }
	}
#ifdef THREADS
	end_critical_section();
#endif THREADS
}

enum aelttype
get_aelttype(cl_object x)
{
	if (x == @'base-char')
		return(aet_ch);
	else if (x == @'bit')
		return(aet_bit);
	else if (x == @'fixnum')
		return(aet_fix);
	else if (x == @'single-float' || x == @'short-float')
		return(aet_sf);
	else if (x == @'long-float' || x == @'double-float')
		return(aet_lf);
/*	else if (x == @'signed-char')
		return(aet_char);
	else if (x == @'unsigned-char')
		return(aet_uchar);
	else if (x == @'signed-short')
		return(aet_short);
	else if (x == @'unsigned-short')
		return(aet_ushort);
*/	else
		return(aet_object);
}

void *
array_address(cl_object x, cl_index inc)
{
	switch(array_elttype(x)) {
	case aet_object:
		return x->array.self.t + inc;
	case aet_fix:
		return x->array.self.fix + inc;
	case aet_sf:
		return x->array.self.t + inc;
	case aet_ch:
		return x->string.self + inc;
	case aet_lf:
		return x->array.self.lf + inc;

	default:
		FEerror("Bad array type", 0);
	}
}

@(defun array_element_type (a)
	cl_object output;
@
	switch (array_elttype(a)) {
	case aet_object:	output = Ct; break;
	case aet_ch:		output = @'base-char'; break;
	case aet_bit:		output = @'bit'; break;
	case aet_fix:		output = @'fixnum'; break;
	case aet_sf:		output = @'short-float'; break;
	case aet_lf:		output = @'long-float'; break;
	}
	@(return output)
@)

/*
	Displace(from, to, offset) displaces the from-array
	to the to-array (the original array) by the specified offset.
	It changes the a_displaced field of both arrays.
	The field is a cons; the car of the from-array points to
	the to-array and the cdr of the to-array is a list of arrays
	displaced to the to-array, so the from-array is pushed to the
	cdr of the to-array's a_displaced.
*/
static void
displace(cl_object from, cl_object to, cl_object offset)
{
	cl_index j;
	enum aelttype totype, fromtype;

	j = fixnnint(offset);
	totype = array_elttype(to);
	fromtype = array_elttype(from);
	if (totype != fromtype)
		FEerror("Cannot displace the array,~%\
because the element types don't match.", 0);
	if (j + from->array.dim > to->array.dim)
		FEerror("Cannot displace the array,~%\
because the total size of the to-array is too small.", 0);
	from->array.displaced = CONS(to, Cnil);
	if (Null(to->array.displaced))
		to->array.displaced = CONS(Cnil, Cnil);
	CDR(to->array.displaced) =
	CONS(from, CDR(to->array.displaced));
	if (fromtype == aet_bit) {
		j += to->vector.offset;
		from->vector.self.bit = to->vector.self.bit + j/CHAR_BIT;
		from->vector.offset = j%CHAR_BIT;
	}
#ifndef BYTE_ADDRESS
	else if (fromtype != aet_ch)
		from->array.self.t = (cl_object *)(array_address(to, j));
#endif
	else
		from->string.self = array_address(to, j);
}

/*
	Check_displaced(dlist, orig, newdim) checks if the displaced
	arrays can keep the displacement when the original array is
	adjusted.
	Dlist is the list of displaced arrays, orig is the original array
	and newdim is the new dimension of the original array.
*/
static void
check_displaced(cl_object dlist, cl_object orig, cl_index newdim)
{
	cl_object x;

	for (;  dlist != Cnil;  dlist = CDR(dlist)) {
		x = CAR(dlist);
		if (x->array.self.t == NULL)
			continue;
		if (array_elttype(x) != aet_bit) {
			if (array_address(x, x->array.dim) >
			    array_address(orig, newdim))
				FEerror("Can't keep displacement.", 0);
		} else {
			if ((x->vector.self.bit - orig->vector.self.bit)*CHAR_BIT +
			    x->vector.dim - newdim +
			    x->vector.offset - orig->vector.offset > 0)
				FEerror("Can't keep displacement.", 0);
		}
		check_displaced(CDR(x->array.displaced), orig, newdim);
	}
}

/*
	Adjust_displaced(x, diff) adds the int value diff
	to the a_self field of the array x and all the arrays displaced to x.
	This function is used in @si::replace-array (ADJUST-ARRAY) and
	the garbage collector.
*/
void adjust_displaced(cl_object x, ptrdiff_t diff)
{
	if (x->array.self.t != NULL)
		x->array.self.t = (cl_object *)((char*)(x->array.self.t) + diff);
	for (x = CDR(x->array.displaced);  x != Cnil;  x = CDR(x))
		adjust_displaced(CAR(x), diff);
}

enum aelttype
array_elttype(cl_object x)
{
	switch(type_of(x)) {
	case t_array:
	case t_vector:
		return((enum aelttype)x->array.elttype);

	case t_string:
		return(aet_ch);

	case t_bitvector:
		return(aet_bit);

	default:
		FEwrong_type_argument(@'array', x);
	}
}

@(defun array_rank (a)
@
	assert_type_array(a);
	@(return ((type_of(a) == t_array) ? MAKE_FIXNUM(a->array.rank)
					  : MAKE_FIXNUM(1)))
@)

@(defun array_dimension (a index)
	cl_index i, dim;
@
	i = fixnnint(index);
	switch (type_of(a)) {
	case t_array:
		if (i >= a->array.rank)
			goto ILLEGAL;
		dim  = a->array.dims[i];
		break;
	case t_string:
		if (i != 0)
			goto ILLEGAL;
		dim = a->string.fillp;
		break;
	case t_vector:
	case t_bitvector:
		if (i != 0)
ILLEGAL:		FEerror("~S is an illegal axis-number to the array ~S.",
				2, index, a);
		dim = a->vector.dim;
		break;
	default:
		FEwrong_type_argument(@'array', a);
	}
	@(return MAKE_FIXNUM(dim))
@)

@(defun array_total_size (a)
@
	assert_type_array(a);
	@(return MAKE_FIXNUM(a->array.dim))
@)

@(defun adjustable_array_p (a)
@
	assert_type_array(a);
	@(return (a->array.adjustable ? Ct : Cnil))
@)

/*
	Internal function for checking if an array is displaced.
*/
@(defun si::displaced_array_p (a)
@
	assert_type_array(a);
	@(return ((CAR(a->array.displaced) != Cnil) ? Ct : Cnil))
@)

@(defun svref (x index)
  cl_index i;
@
  if (type_of(x) != t_vector ||
      x->vector.adjustable ||
      x->vector.hasfillp ||
      CAR(x->vector.displaced) != Cnil ||
      (enum aelttype)x->vector.elttype != aet_object)
    FEwrong_type_argument(@'simple-vector', x);
  if ((i = fixnnint(index)) >= x->vector.dim)
    illegal_index(x, index);
  @(return x->vector.self.t[i])
@)

@(defun si::svset (x index v)
  cl_index i;
@
  if (type_of(x) != t_vector ||
      x->vector.adjustable ||
      x->vector.hasfillp ||
      CAR(x->vector.displaced) != Cnil ||
      (enum aelttype)x->vector.elttype != aet_object)
    FEwrong_type_argument(@'simple-vector', x);
  if ((i = fixnnint(index)) >= x->vector.dim)
    illegal_index(x, index);
  @(return (x->vector.self.t[i] = v))
@)

@(defun array_has_fill_pointer_p (a)
	cl_object r;
@
	switch (type_of(a)) {
	case t_array:
		r = Cnil; break;
	case t_vector:
	case t_bitvector:
	case t_string:
		r = a->vector.hasfillp? Ct : Cnil;
		break;
	default:
		FEwrong_type_argument(@'array', a);
	}
	@(return r)
@)

@(defun fill_pointer (a)
@
  assert_type_vector(a);
  if (a->vector.hasfillp)
    @(return MAKE_FIXNUM(a->vector.fillp))
  FEerror("The vector ~S has no fill pointer.", 1, a);
@)

/*
	Internal function for setting fill pointer.
*/
@(defun si::fill_pointer_set (a fp)
  cl_index i;
@
  assert_type_vector(a);
  i = fixnnint(fp);
  if (a->vector.hasfillp)
    if (i > a->vector.dim)
      FEerror("The fill-pointer ~S is too large", 1, fp);
    else
      a->vector.fillp = i;
  else
    FEerror("The vector ~S has no fill pointer.", 1, a);
  @(return fp)
@)

/*
	Internal function for replacing the contents of arrays:

		(si:replace-array old-array new-array).

	Used in ADJUST-ARRAY.
*/
@(defun si::replace_array (old new)
  cl_object displaced, dlist;
  ptrdiff_t diff;
@
  if (type_of(old) != type_of(new)
      || (type_of(old) == t_array && old->array.rank != new->array.rank))
    goto CANNOT;
  if (!old->array.adjustable)
    FEerror("~S is not adjustable.", 1, old);
  diff = (char*)(new->array.self.t) - (char*)(old->array.self.t);
  dlist = CDR(old->array.displaced);
  displaced = CONS(CAR(new->array.displaced), dlist);
  check_displaced(dlist, old, new->array.dim);
  adjust_displaced(old, diff);
/*  undisplace(old); */
  switch (type_of(old)) {
  case t_array:
  case t_vector:
  case t_bitvector:
    old->array = new->array;
    break;

  case t_string:
    old->string = new->string;
    break;

  default:
    goto CANNOT;
  }
  old->array.displaced = displaced;
  @(return old)

 CANNOT:
  FEerror("Cannot replace the array ~S by the array ~S.", 2, old, new);
@)

void
init_array(void)
{
	make_constant("ARRAY-RANK-LIMIT", MAKE_FIXNUM(ARANKLIM));
	make_constant("ARRAY-DIMENSION-LIMIT", MAKE_FIXNUM(ADIMLIM));
	make_constant("ARRAY-TOTAL-SIZE-LIMIT", MAKE_FIXNUM(ATOTLIM));
}
