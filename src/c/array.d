/*
    array.c --  Array routines
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <limits.h>
#include <string.h>
#include <ecl/ecl.h>

static const cl_index ecl_aet_size[] = {
  sizeof(cl_object),          /* aet_object */
  sizeof(float),              /* aet_sf */
  sizeof(double),             /* aet_lf */
  0,                          /* aet_bit: cannot be handled with this code */
  sizeof(cl_fixnum),          /* aet_fix */
  sizeof(cl_index),           /* aet_index */
  sizeof(uint8_t),            /* aet_b8 */
  sizeof(int8_t),             /* aet_i8 */
#ifdef ECL_UNICODE
  sizeof(cl_object),          /* aet_ch */
#endif
  sizeof(unsigned char)       /* aet_bc */
};

static void displace (cl_object from, cl_object to, cl_object offset);
static void check_displaced (cl_object dlist, cl_object orig, cl_index newdim);

cl_index
object_to_index(cl_object n)
{
	switch (type_of(n)) {
	case t_fixnum: {
		cl_fixnum out = fix(n);
		if (out < 0 || out >= ADIMLIM)
			FEtype_error_index(Cnil, n);
		return out;
	}
	case t_bignum:
		FEtype_error_index(Cnil, n);
	default:
		FEtype_error_integer(n);
	}
}

cl_object
cl_row_major_aref(cl_object x, cl_object indx)
{
	cl_index j = fixnnint(indx);
	@(return aref(x, j))
}

cl_object
si_row_major_aset(cl_object x, cl_object indx, cl_object val)
{
	cl_index j = fixnnint(indx);
	@(return aset(x, j, val))
}

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
      index = cl_va_arg(indx);
      if ((s = fixnnint(index)) >= x->array.dims[i])
        FEerror("The ~:R index, ~S, to the array~%\
~S is too large.", 3, MAKE_FIXNUM(i+1), index, x);
      j = j*(x->array.dims[i]) + s;
    }
    break;

  case t_vector:
#ifdef ECL_UNICODE
  case t_string:
#endif
  case t_base_string:
  case t_bitvector:
    if (r != 1)
      FEerror("Wrong number of indices.", 0);
    index = cl_va_arg(indx);
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
  switch ((cl_elttype)array_elttype(x)) {
  case aet_object:
#ifdef ECL_UNICODE
  case aet_ch:
#endif
    return(x->array.self.t[index]);

  case aet_bc:
    return(CODE_CHAR(x->base_string.self[index]));

  case aet_bit:
    index += x->vector.offset;
    if (x->vector.self.bit[index/CHAR_BIT] & (0200>>index%CHAR_BIT))
      return(MAKE_FIXNUM(1));
    else
      return(MAKE_FIXNUM(0));
  case aet_fix:
    return make_integer(x->array.self.fix[index]);

  case aet_index:
    return make_unsigned_integer(x->array.self.index[index]);

  case aet_sf:
    return(make_shortfloat(x->array.self.sf[index]));

  case aet_lf:
    return(make_longfloat(x->array.self.lf[index]));

  case aet_b8:
    return(MAKE_FIXNUM(x->array.self.b8[index]));

  case aet_i8:
    return(MAKE_FIXNUM(x->array.self.i8[index]));

  default:
    internal_error("aref");
  }
}

cl_object
aref1(cl_object v, cl_index index)
{
  switch (type_of(v)) {
#ifdef ECL_UNICODE
  case t_string:
#endif
  case t_vector:
  case t_bitvector:
    return(aref(v, index));

  case t_base_string:
    if (index >= v->base_string.dim)
      FEerror("The index, ~D, is too large.", 1, MAKE_FIXNUM(index));
    return(CODE_CHAR(v->base_string.self[index]));

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
      index = cl_va_arg(dims);
      if ((s = fixnnint(index)) >= x->array.dims[i])
	FEerror("The ~:R index, ~S, to the array ~S is too large.",
		3, MAKE_FIXNUM(i+1), index, x);
      j = j*(x->array.dims[i]) + s;
    }
    break;

  case t_vector:
#ifdef ECL_UNICODE
  case t_string:
#endif
  case t_base_string:
  case t_bitvector:
    if (r != 1)
      FEerror("Wrong number of indices.", 0);
    index = cl_va_arg(dims);
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
#ifdef ECL_UNICODE
  case aet_ch:
#endif
    x->array.self.t[index] = value;
    break;

  case aet_bc:
    /* INV: char_code() checks the type of `value' */
    x->base_string.self[index] = char_code(value);
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

  case aet_index:
    x->array.self.index[index] = fixnnint(value);
    break;

  case aet_sf:
    x->array.self.sf[index] = object_to_float(value);
    break;

  case aet_lf:
    x->array.self.lf[index] = object_to_double(value);
    break;

  case aet_b8: {
    cl_index i = fixnnint(value);
    if (i > 0xFF) FEerror("~S is not a (INTEGER 0 255)",1,value);
    x->array.self.b8[index] = i;
    break;
  }
  case aet_i8: {
    cl_fixnum i = fixint(value);
    if (i > 127 || i < -128) FEerror("~S is not a (INTEGER -128 127)",1,value);
    x->array.self.i8[index] = i;
    break;
  }
  }
  return(value);
}

cl_object
aset1(cl_object v, cl_index index, cl_object val)
{
  switch (type_of(v)) {
#ifdef ECL_UNICODE
  case t_string:
#endif
  case t_vector:
  case t_bitvector:
    return(aset(v, index, val));

  case t_base_string:
    if (index >= v->base_string.dim)
      FEerror("The index, ~D, is too large", 1, MAKE_FIXNUM(index));
    /* INV: char_code() checks the type of `val' */
    v->base_string.self[index] = char_code(val);
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
  if (etype == Cnil) {
    FEerror("ECL does not support creating arrays with element type NIL", 0);
  }
  r = narg - 4;
  x = cl_alloc_object(t_array);
  x->array.displaced = Cnil;
  x->array.self.t = NULL;		/* for GC sake */
  x->array.rank = r;
  x->array.elttype = (short)ecl_symbol_to_elttype(etype);
  x->array.dims = (cl_index *)cl_alloc_atomic_align(sizeof(cl_index)*r, sizeof(cl_index));
  if (r >= ARANKLIM)
    FEerror("The array rank, ~R, is too large.", 1, MAKE_FIXNUM(r));
  for (i = 0, s = 1;  i < r;  i++) {
    cl_object index = cl_va_arg(dims);
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
cl_object
si_make_vector(cl_object etype, cl_object dim, cl_object adj,
	       cl_object fillp, cl_object displ, cl_object disploff)
{
  cl_index d, f;
  cl_object x;
  cl_elttype aet;

  if (etype == Cnil) {
    FEerror("ECL does not support creating arrays with element type NIL", 0);
  }
  aet = ecl_symbol_to_elttype(etype);
  if ((d = fixnnint(dim)) > ADIMLIM)
    FEerror("The vector dimension, ~D, is too large.", 1, dim);
  f = d;
  if (aet == aet_bc) {
    x = cl_alloc_object(t_base_string);
  } else if (aet == aet_bit) {
    x = cl_alloc_object(t_bitvector);
#ifdef ECL_UNICODE
  } else if (aet == aet_ch) {
    x = cl_alloc_object(t_string);
#endif
  } else {
    x = cl_alloc_object(t_vector);
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
}

void
array_allocself(cl_object x)
{
	cl_index i, d;

	d = x->array.dim;
	start_critical_section(); /* avoid losing elts */
	switch (array_elttype(x)) {
	/* assign self field only after it has been filled, for GC sake  */
	case aet_object: {
		cl_object *elts;
		elts = (cl_object *)cl_alloc_align(sizeof(cl_object)*d, sizeof(cl_object));
		for (i = 0;  i < d;  i++)
			elts[i] = Cnil;
		x->array.self.t = elts;
		break;
	      }
#ifdef ECL_UNICODE
	case aet_ch: {
		cl_object *elts;
		elts = (cl_object *)cl_alloc_align(sizeof(cl_object)*d, sizeof(cl_object));
		for (i = 0;  i < d;  i++)
			elts[i] = CODE_CHAR(' ');
		x->string.self = elts;
		break;
	      }
#endif
	case aet_bc: {
		char *elts;
		elts = (char *)cl_alloc_atomic(d+1);
		for (i = 0;  i < d;  i++)
			elts[i] = ' ';
		elts[d] = '\0';
		x->base_string.self = elts;
		break;
	      }
	case aet_bit: {
		byte *elts;
		d = (d+(CHAR_BIT-1))/CHAR_BIT;
		elts = (byte *)cl_alloc_atomic(d);
		for (i = 0;  i < d;  i++)
			elts[i] = '\0';
		x->vector.offset = 0;
		x->vector.self.bit = elts;
		break;
	      }
	case aet_fix: {
		cl_fixnum *elts;
		elts = (cl_fixnum *)cl_alloc_atomic_align(sizeof(*elts)*d, sizeof(*elts));
		for (i = 0;  i < d;  i++)
			elts[i] = 0;
		x->array.self.fix = elts;
		break;
	      }
	case aet_index: {
		cl_fixnum *elts;
		elts = (cl_fixnum *)cl_alloc_atomic_align(sizeof(*elts)*d, sizeof(*elts));
		for (i = 0;  i < d;  i++)
			elts[i] = 0;
		x->array.self.fix = elts;
		break;
	      }
	case aet_sf: {
		float *elts;
		elts = (float *)cl_alloc_atomic_align(sizeof(*elts)*d, sizeof(*elts));
		for (i = 0;  i < d;  i++)
			elts[i] = 0.0;
		x->array.self.sf = elts;
		break;
	      }
	case aet_lf: {
		double *elts;
		elts = (double *)cl_alloc_atomic_align(sizeof(*elts)*d, sizeof(*elts));
		for (i = 0;  i < d;  i++)
			elts[i] = 0.0;
		x->array.self.lf = elts;
		break;
	      }
	case aet_b8: {
		uint8_t *elts;
		elts = (uint8_t *)cl_alloc_atomic_align(sizeof(*elts)*d, sizeof(*elts));
		for (i = 0;  i < d;  i++)
			elts[i] = 0;
		x->array.self.b8 = elts;
		break;
	      }
	case aet_i8: {
		int8_t *elts;
		elts = (int8_t *)cl_alloc_atomic_align(sizeof(*elts)*d, sizeof(*elts));
		for (i = 0;  i < d;  i++)
			elts[i] = 0;
		x->array.self.i8 = elts;
		break;
	      }
	}
	end_critical_section();
}

cl_elttype
ecl_symbol_to_elttype(cl_object x)
{
 BEGIN:
	if (x == @'base-char')
		return(aet_bc);
#ifdef ECL_UNICODE
	if (x == @'character')
		return(aet_ch);
#endif
	else if (x == @'bit')
		return(aet_bit);
	else if (x == @'ext::cl-fixnum')
		return(aet_fix);
	else if (x == @'ext::cl-index')
		return(aet_index);
	else if (x == @'single-float' || x == @'short-float')
		return(aet_sf);
	else if (x == @'long-float' || x == @'double-float')
		return(aet_lf);
	else if (x == @'ext::byte8')
		return(aet_b8);
	else if (x == @'ext::integer8')
		return(aet_i8);
	else if (x == @'t')
		return(aet_object);
	x = cl_funcall(2, @'upgraded-array-element-type', x);
	goto BEGIN;
}

cl_object
ecl_elttype_to_symbol(cl_elttype aet)
{
	cl_object output;
	switch (aet) {
	case aet_object:	output = Ct; break;
#ifdef ECL_UNICODE
	case aet_ch:		output = @'character'; break;
#endif
	case aet_bc:		output = @'base-char'; break;
	case aet_bit:		output = @'bit'; break;
	case aet_fix:		output = @'ext::cl-fixnum'; break;
	case aet_index:		output = @'ext::cl-index'; break;
	case aet_sf:		output = @'short-float'; break;
	case aet_lf:		output = @'long-float'; break;
	case aet_b8:		output = @'ext::byte8'; break;
	case aet_i8:		output = @'ext::integer8'; break;
	}
	return output;
}

static void *
array_address(cl_object x, cl_index inc)
{
	switch(array_elttype(x)) {
#ifdef ECL_UNICODE
	case aet_ch:
#endif
	case aet_object:
		return x->array.self.t + inc;
	case aet_fix:
		return x->array.self.fix + inc;
	case aet_index:
		return x->array.self.fix + inc;
	case aet_sf:
		return x->array.self.t + inc;
	case aet_bc:
		return x->base_string.self + inc;
	case aet_lf:
		return x->array.self.lf + inc;
	case aet_b8:
		return x->array.self.b8 + inc;
	case aet_i8:
		return x->array.self.i8 + inc;
	default:
		FEerror("Bad array type", 0);
	}
}

cl_object
cl_array_element_type(cl_object a)
{
	@(return ecl_elttype_to_symbol(array_elttype(a)))
}

/*
	Displace(from, to, offset) displaces the from-array
	to the to-array (the original array) by the specified offset.
	It changes the a_displaced field of both arrays.
	The field is a cons; the car of the from-array points to
	the to-array and the cdr of the to-array is a list of arrays
	displaced to the to-array, so the from-array is pushed to the
	cdr of the to-array's array.displaced.
*/
static void
displace(cl_object from, cl_object to, cl_object offset)
{
	cl_index j;
	cl_elttype totype, fromtype;

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
	else if (fromtype != aet_bc)
		from->array.self.t = (cl_object *)(array_address(to, j));
#endif
	else
		from->base_string.self = (char *)array_address(to, j);
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
	if (x->array.self.t != NULL) {
		if (array_elttype(x) == aet_bit) {
			ptrdiff_t aux = diff + x->array.offset;
			x->array.offset = aux % CHAR_BIT;
			x->array.self.bit += aux / CHAR_BIT;
		} else {
			x->array.self.t = (cl_object *)((char*)(x->array.self.t) + diff);
		}
		for (x = CDR(x->array.displaced);  x != Cnil;  x = CDR(x))
			adjust_displaced(CAR(x), diff);
	}
}

cl_elttype
array_elttype(cl_object x)
{
	switch(type_of(x)) {
	case t_array:
	case t_vector:
		return((cl_elttype)x->array.elttype);

#ifdef ECL_UNICODE
	case t_string:
		return(aet_ch);
#endif

	case t_base_string:
		return(aet_bc);

	case t_bitvector:
		return(aet_bit);

	default:
		FEwrong_type_argument(@'array', x);
	}
}

cl_object
cl_array_rank(cl_object a)
{
	assert_type_array(a);
	@(return ((type_of(a) == t_array) ? MAKE_FIXNUM(a->array.rank)
					  : MAKE_FIXNUM(1)))
}

cl_object
cl_array_dimension(cl_object a, cl_object index)
{
	cl_index i, dim;

	i = fixnnint(index);
	switch (type_of(a)) {
	case t_array:
		if (i >= a->array.rank)
			goto ILLEGAL;
		dim  = a->array.dims[i];
		break;
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string:
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
}

cl_object
cl_array_total_size(cl_object a)
{
	assert_type_array(a);
	@(return MAKE_FIXNUM(a->array.dim))
}

cl_object
cl_adjustable_array_p(cl_object a)
{
	assert_type_array(a);
	@(return (a->array.adjustable ? Ct : Cnil))
}

/*
	Internal function for checking if an array is displaced.
*/
cl_object
cl_array_displacement(cl_object a)
{
	cl_object to_array;
	cl_index offset;

	assert_type_array(a);
	to_array = a->array.displaced;
	if (Null(to_array)) {
		offset = 0;
	} else if (Null(to_array = CAR(a->array.displaced))) {
		offset = 0;
	} else {
		switch (array_elttype(a)) {
#ifdef ECL_UNICODE
		case aet_ch:
#endif
		case aet_object:
			offset = a->array.self.t - to_array->array.self.t;
			break;
		case aet_bc:
			offset = a->array.self.ch - to_array->array.self.ch;
			break;
		case aet_bit:
			offset = a->array.self.bit - to_array->array.self.bit;
			offset = offset * CHAR_BIT + a->array.offset;
			break;
		case aet_fix:
			offset = a->array.self.fix - to_array->array.self.fix;
			break;
		case aet_index:
			offset = a->array.self.fix - to_array->array.self.fix;
			break;
		case aet_sf:
			offset = a->array.self.sf - to_array->array.self.sf;
			break;
		case aet_lf:
			offset = a->array.self.lf - to_array->array.self.lf;
			break;
		case aet_b8:
		case aet_i8:
		default:
			offset = a->array.self.b8 - to_array->array.self.b8;
			break;
		}
	}
	@(return to_array MAKE_FIXNUM(offset));
}

cl_object
cl_svref(cl_object x, cl_object index)
{
  cl_index i;

  if (type_of(x) != t_vector ||
      x->vector.adjustable ||
      x->vector.hasfillp ||
      CAR(x->vector.displaced) != Cnil ||
      (cl_elttype)x->vector.elttype != aet_object)
    FEwrong_type_argument(@'simple-vector', x);
  if ((i = fixnnint(index)) >= x->vector.dim)
    illegal_index(x, index);
  @(return x->vector.self.t[i])
}

cl_object
si_svset(cl_object x, cl_object index, cl_object v)
{
  cl_index i;

  if (type_of(x) != t_vector ||
      x->vector.adjustable ||
      x->vector.hasfillp ||
      CAR(x->vector.displaced) != Cnil ||
      (cl_elttype)x->vector.elttype != aet_object)
    FEwrong_type_argument(@'simple-vector', x);
  if ((i = fixnnint(index)) >= x->vector.dim)
    illegal_index(x, index);
  @(return (x->vector.self.t[i] = v))
}

cl_object
cl_array_has_fill_pointer_p(cl_object a)
{
	cl_object r;

	switch (type_of(a)) {
	case t_array:
		r = Cnil; break;
	case t_vector:
	case t_bitvector:
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string:
		r = a->vector.hasfillp? Ct : Cnil;
		break;
	default:
		FEwrong_type_argument(@'array', a);
	}
	@(return r)
}

cl_object
cl_fill_pointer(cl_object a)
{
	assert_type_vector(a);
	if (!a->vector.hasfillp)
		FEwrong_type_argument(c_string_to_object("(AND VECTOR (SATISFIES ARRAY-HAS-FILL-POINTER-P))"),
				      a);
	@(return MAKE_FIXNUM(a->vector.fillp))
}

/*
	Internal function for setting fill pointer.
*/
cl_object
si_fill_pointer_set(cl_object a, cl_object fp)
{
  cl_index i;

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
}

/*
	Internal function for replacing the contents of arrays:

		(si:replace-array old-array new-array).

	Used in ADJUST-ARRAY.
*/
cl_object
si_replace_array(cl_object olda, cl_object newa)
{
	cl_object displaced, dlist;
	ptrdiff_t diff;

	if (type_of(olda) != type_of(newa)
	    || (type_of(olda) == t_array && olda->array.rank != newa->array.rank))
		goto CANNOT;
	if (!olda->array.adjustable) {
		/* When an array is not adjustable, we simply output the new array */
		olda = newa;
		goto OUTPUT;
	}
	diff = (char*)(newa->array.self.t) - (char*)(olda->array.self.t);
	if (array_elttype(newa) == aet_bit) {
		diff = diff * CHAR_BIT + (newa->array.offset - olda->array.offset);
	}
	dlist = CDR(olda->array.displaced);
	displaced = CONS(CAR(newa->array.displaced), dlist);
	check_displaced(dlist, olda, newa->array.dim);
	adjust_displaced(olda, diff);
	switch (type_of(olda)) {
	case t_array:
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_vector:
	case t_bitvector:
		olda->array = newa->array;
		break;
	case t_base_string:
		olda->base_string = newa->base_string;
		break;
	default:
	CANNOT:
		FEerror("Cannot replace the array ~S by the array ~S.", 2, olda, newa);
	}
	olda->array.displaced = displaced;
 OUTPUT:
	@(return olda)
}

void
ecl_copy_subarray(cl_object dest, cl_index i0, cl_object orig, cl_index i1, cl_index l)
{
	cl_elttype t = array_elttype(dest);
	if (i0 + l > dest->array.dim) {
		l = dest->array.dim - i0;
	}
	if (i1 + l > orig->array.dim) {
		l = orig->array.dim - i1;
	}
	if (t != array_elttype(orig) || t == aet_bit) {
		while (l--) {
			aset(dest, i0++, aref(orig, i1++));
		}
	} else if (t >= 0 && t <= aet_last_type) {
		cl_index elt_size = ecl_aet_size[t];
		memcpy(dest->array.self.ch + i0 * elt_size,
		       orig->array.self.ch + i1 * elt_size,
		       l * elt_size);
	} else {
		FEerror("Bad array type", 0);
	}
}

void
ecl_reverse_subarray(cl_object x, cl_index i0, cl_index i1)
{
	cl_elttype t = array_elttype(x);
	cl_index i, j;
	if (x->array.dim == 0) {
		return;
	}
	if (i1 >= x->array.dim) {
		i1 = x->array.dim;
	}
	switch (t) {
#ifdef ECL_UNICODE
	case aet_ch:
#endif
	case aet_object:
	case aet_fix:
	case aet_index:
		for (i = i0, j = i1-1;  i < j;  i++, --j) {
			cl_object y = x->vector.self.t[i];
			x->vector.self.t[i] = x->vector.self.t[j];
			x->vector.self.t[j] = y;
		}
		break;
	case aet_sf:
		for (i = i0, j = i1-1;  i < j;  i++, --j) {
			float y = x->array.self.sf[i];
			x->array.self.sf[i] = x->array.self.sf[j];
			x->array.self.sf[j] = y;
		}
		break;
	case aet_lf:
		for (i = i0, j = i1-1;  i < j;  i++, --j) {
			double y = x->array.self.lf[i];
			x->array.self.lf[i] = x->array.self.lf[j];
			x->array.self.lf[j] = y;
		}
		break;
	case aet_b8:
		for (i = i0, j = i1-1;  i < j;  i++, --j) {
			uint8_t y = x->array.self.b8[i];
			x->array.self.b8[i] = x->array.self.b8[j];
			x->array.self.b8[j] = y;
		}
		break;
	case aet_i8:
		for (i = i0, j = i1-1;  i < j;  i++, --j) {
			int8_t y = x->array.self.i8[i];
			x->array.self.i8[i] = x->array.self.i8[j];
				x->array.self.i8[j] = y;
		}
		break;
	case aet_bc:
		for (i = i0, j = i1-1;  i < j;  i++, --j) {
			unsigned char y = x->array.self.ch[i];
			x->array.self.ch[i] = x->array.self.ch[j];
				x->array.self.ch[j] = y;
		}
		break;
	case aet_bit:
		for (i = i0 + x->vector.offset,
		     j = i1 + x->vector.offset - 1;
		     i < j;
		     i++, --j) {
			int k = x->array.self.bit[i/CHAR_BIT]&(0200>>i%CHAR_BIT);
			if (x->array.self.bit[j/CHAR_BIT]&(0200>>j%CHAR_BIT))
				x->array.self.bit[i/CHAR_BIT]
				|= 0200>>i%CHAR_BIT;
			else
				x->array.self.bit[i/CHAR_BIT]
				&= ~(0200>>i%CHAR_BIT);
			if (k)
				x->array.self.bit[j/CHAR_BIT]
				|= 0200>>j%CHAR_BIT;
			else
				x->array.self.bit[j/CHAR_BIT]
				&= ~(0200>>j%CHAR_BIT);
		}
		break;
	default:
		FEerror("Bad array type", 0);
	}
}
