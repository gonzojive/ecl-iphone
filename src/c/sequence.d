/*
    sequence.d -- Sequence routines.
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
#include "ecl.h"
#include "ecl-inl.h"

/*
	I know the following name is not good.
*/
cl_object
cl_alloc_simple_vector(cl_index l, cl_elttype aet)
{
	cl_object x;

	x = cl_alloc_object(t_vector);
	x->vector.hasfillp = FALSE;
	x->vector.adjustable = FALSE;
	x->vector.displaced = Cnil;
	x->vector.dim = x->vector.fillp = l;
	x->vector.self.t = NULL;
	x->vector.elttype = (short)aet;
	return(x);
}

cl_object
cl_alloc_simple_bitvector(cl_index l)
{
	cl_object x;

	x = cl_alloc_object(t_bitvector);
	x->vector.hasfillp = FALSE;
	x->vector.adjustable = FALSE;
	x->vector.displaced = Cnil;
	x->vector.dim = x->vector.fillp = l;
	x->vector.offset = 0;
	x->vector.self.bit = NULL;
	return(x);
}

cl_object
cl_elt(cl_object x, cl_object i)
{
	@(return elt(x, fixint(i)))
}

cl_object
elt(cl_object seq, cl_fixnum index)
{
	cl_fixnum i;
	cl_object l;

	if (index < 0)
		goto E;
	switch (type_of(seq)) {
	case t_cons:
		for (i = index, l = seq;  i > 0;  --i)
			if (endp(l))
				goto E;
			else
				l = CDR(l);
		if (endp(l))
			goto E;
		return(CAR(l));

	case t_vector:
	case t_bitvector:
		if (index >= seq->vector.fillp)
			goto E;
		return(aref(seq, index));

	case t_string:
		if (index >= seq->string.fillp)
			goto E;
		return(CODE_CHAR(seq->string.self[index]));

	case t_symbol:
		if (Null(seq))
			break;
	default:
		FEwrong_type_argument(@'sequence', seq);
	}
E:
	FEtype_error_index(seq, MAKE_FIXNUM(index));
}

cl_object
si_elt_set(cl_object seq, cl_object index, cl_object val)
{
	@(return elt_set(seq, fixint(index), val))
}

cl_object
elt_set(cl_object seq, cl_fixnum index, cl_object val)
{
	cl_fixnum i;
	cl_object l;

	if (index < 0)
		goto E;
	switch (type_of(seq)) {
	case t_cons:
		for (i = index, l = seq;  i > 0;  --i)
			if (endp(l))
				goto E;
			else
				l = CDR(l);
		if (endp(l))
			goto E;
		return(CAR(l) = val);

	case t_vector:
	case t_bitvector:
		if (index >= seq->vector.fillp)
			goto E;
		return(aset(seq, index, val));

	case t_string:
		if (index >= seq->string.fillp)
			goto E;
		/* INV: char_code() checks the type of `val' */
		seq->string.self[index] = char_code(val);
		return(val);

	default:
		FEwrong_type_argument(@'sequence', seq);
	}
E:
	FEtype_error_index(seq, MAKE_FIXNUM(index));
}

@(defun subseq (sequence start &optional end &aux x)
	cl_fixnum s, e;
	cl_fixnum i, j;
@
	s = fixnnint(start);
	if (Null(end))
		e = -1;
	else
		e = fixnnint(end);
	switch (type_of(sequence)) {
	case t_symbol:
		if (Null(sequence)) {
			if (s > 0)
				goto ILLEGAL_START_END;
			if (e > 0)
				goto ILLEGAL_START_END;
			@(return Cnil)
		}
		FEwrong_type_argument(@'sequence', sequence);

	case t_cons:
		if (e >= 0)
			if ((e -= s) < 0)
				goto ILLEGAL_START_END;
		while (s-- > 0) {
			if (ATOM(sequence))
				goto ILLEGAL_START_END;
			sequence = CDR(sequence);
		}
		if (e < 0)
			return cl_copy_list(sequence);
		{ cl_object *z = &x;
		  for (i = 0;  i < e;  i++) {
		    if (ATOM(sequence))
		      goto ILLEGAL_START_END;
		    z = &CDR(*z = CONS(CAR(sequence), Cnil));
		    sequence = CDR(sequence);
		  }
		}
		@(return x)

	case t_vector:
		if (s > sequence->vector.fillp)
			goto ILLEGAL_START_END;
		if (e < 0)
			e = sequence->vector.fillp;
		else if (e < s || e > sequence->vector.fillp)
			goto ILLEGAL_START_END;
		x = cl_alloc_simple_vector(e - s, (cl_elttype)sequence->vector.elttype);
		array_allocself(x);
		switch ((cl_elttype)sequence->vector.elttype) {
		case aet_object:
		case aet_fix:
		case aet_sf:
			for (i = s, j = 0;  i < e;  i++, j++)
				x->vector.self.t[j] = sequence->vector.self.t[i];
			break;

		case aet_lf:
			for (i = s, j = 0;  i < e;  i++, j++)
				x->array.self.lf[j] =
				sequence->array.self.lf[i];
			break;
		case aet_b8:
		case aet_i8:
			for (i = s, j = 0;  i < e;  i++, j++)
				x->vector.self.b8[j] = sequence->vector.self.b8[i];
			break;
		default:
			internal_error("subseq");
		}
		@(return x)

	case t_string:
		if (s > sequence->string.fillp)
			goto ILLEGAL_START_END;
		if (e < 0)
			e = sequence->string.fillp;
		else if (e < s || e > sequence->string.fillp)
			goto ILLEGAL_START_END;
		x = cl_alloc_simple_string(e - s);
		for (i = s, j = 0;  i < e;  i++, j++)
			x->string.self[j] = sequence->string.self[i];
		@(return x)

	case t_bitvector:
		if (s > sequence->vector.fillp)
			goto ILLEGAL_START_END;
		if (e < 0)
			e = sequence->vector.fillp;
		else if (e < s || e > sequence->vector.fillp)
			goto ILLEGAL_START_END;
		x = cl_alloc_simple_bitvector(e - s);
		x->vector.self.bit = (byte *)cl_alloc_atomic((e-s+CHAR_BIT-1)/CHAR_BIT);
		s += sequence->vector.offset;
		e += sequence->vector.offset;
		for (i = s, j = 0;  i < e;  i++, j++)
			if (sequence->vector.self.bit[i/CHAR_BIT]&(0200>>i%CHAR_BIT))
				x->vector.self.bit[j/CHAR_BIT]
				|= 0200>>j%CHAR_BIT;
			else
				x->vector.self.bit[j/CHAR_BIT]
				&= ~(0200>>j%CHAR_BIT);
		@(return x)

	default:
		FEwrong_type_argument(@'sequence', sequence);
	}

ILLEGAL_START_END:
	FEerror("~S and ~S are illegal as :START and :END~%\
for the sequence ~S.", 3, start, end, sequence);
@)

cl_object
cl_copy_seq(cl_object x)
{
	return @subseq(2, x, MAKE_FIXNUM(0));
}

cl_object
cl_length(cl_object x)
{
	@(return MAKE_FIXNUM(length(x)))
}

cl_fixnum
length(cl_object x)
{
	cl_fixnum i;

	switch (type_of(x)) {
	case t_symbol:
		if (Null(x))
			return(0);
		FEwrong_type_argument(@'sequence', x);

	case t_cons:
		/* INV: A list's length always fits in a fixnum */
		i = 0;
		loop_for_in(x) {
			i++;
		} end_loop_for_in;
		return(i);

	case t_vector:
	case t_string:
	case t_bitvector:
		return(x->vector.fillp);

	default:
		FEwrong_type_argument(@'sequence', x);
	}
}

cl_object
cl_reverse(cl_object seq)
{
	cl_object x, y;
	cl_fixnum i, j, k;

	switch (type_of(seq)) {
	case t_symbol:
		if (Null(seq))
			y = Cnil;
		else
			FEwrong_type_argument(@'sequence', seq);
		break;

	case t_cons:
		y = Cnil;
		for (x = seq;  !endp(x);  x = CDR(x))
			y = CONS(CAR(x), y);
		break;

	case t_vector:
		x = seq;
		k = x->vector.fillp;
		y = cl_alloc_simple_vector(k, (cl_elttype)x->vector.elttype);
		array_allocself(y);
		switch ((cl_elttype)x->vector.elttype) {
		case aet_object:
		case aet_fix:
		case aet_sf:
			for (j = k - 1, i = 0;  j >=0;  --j, i++)
				y->vector.self.t[j] = x->vector.self.t[i];
			break;
		case aet_lf:
			for (j = k - 1, i = 0;  j >=0;  --j, i++)
				y->array.self.lf[j] = x->array.self.lf[i];
			break;
		case aet_b8:
			for (j = k - 1, i = 0;  j >=0;  --j, i++)
				y->array.self.b8[j] = x->array.self.b8[i];
			break;
		case aet_i8:
			for (j = k - 1, i = 0;  j >=0;  --j, i++)
				y->array.self.i8[j] = x->array.self.i8[i];
			break;
		default:
			internal_error("reverse");
		}
		break;

	case t_string:
		x = seq;
		y = cl_alloc_simple_string(x->string.fillp);
		for (j = x->string.fillp - 1, i = 0;  j >=0;  --j, i++)
			y->string.self[j] = x->string.self[i];
		break;

	case t_bitvector:
		x = seq;
		y = cl_alloc_simple_bitvector(x->vector.fillp);
		y->vector.self.bit = (byte *)cl_alloc_atomic((x->vector.fillp+CHAR_BIT-1)/CHAR_BIT);
		for (j = x->vector.fillp - 1, i = x->vector.offset;
		     j >=0;
		     --j, i++)
			if (x->vector.self.bit[i/CHAR_BIT]&(0200>>i%CHAR_BIT))
				y->vector.self.bit[j/CHAR_BIT] |= 0200>>j%CHAR_BIT;
			else
				y->vector.self.bit[j/CHAR_BIT] &= ~(0200>>j%CHAR_BIT);
		break;

	default:
		FEwrong_type_argument(@'sequence', seq);
	}
	@(return y)
}

cl_object
cl_nreverse(cl_object seq)
{
	cl_object x, y, z;
	cl_fixnum i, j, k;

	switch (type_of(seq)) {
	case t_symbol:
		if (!Null(seq))
			FEwrong_type_argument(@'sequence', seq);
		break;

	case t_cons:
		for (x = Cnil, y = seq;  !endp(CDR(y));) {
			z = y;
			y = CDR(y);
			CDR(z) = x;
			x = z;
		}
		CDR(y) = x;
		seq = y;
		break;

	case t_vector:
		x = seq;
		k = x->vector.fillp;
		switch ((cl_elttype)x->vector.elttype) {
		case aet_object:
		case aet_fix:
			for (i = 0, j = k - 1;  i < j;  i++, --j) {
				y = x->vector.self.t[i];
				x->vector.self.t[i] = x->vector.self.t[j];
				x->vector.self.t[j] = y;
			}
			break;
		case aet_sf:
			for (i = 0, j = k - 1;  i < j;  i++, --j) {
				float y = x->array.self.sf[i];
				x->array.self.sf[i] = x->array.self.sf[j];
				x->array.self.sf[j] = y;
			}
			break;
		case aet_lf:
			for (i = 0, j = k - 1;  i < j;  i++, --j) {
				double y = x->array.self.lf[i];
				x->array.self.lf[i] = x->array.self.lf[j];
				x->array.self.lf[j] = y;
			}
			break;
		case aet_b8:
			for (i = 0, j = k - 1;  i < j;  i++, --j) {
				uint8_t y = x->array.self.b8[i];
				x->array.self.b8[i] = x->array.self.b8[j];
				x->array.self.b8[j] = y;
			}
			break;
		case aet_i8:
			for (i = 0, j = k - 1;  i < j;  i++, --j) {
				int8_t y = x->array.self.i8[i];
				x->array.self.i8[i] = x->array.self.i8[j];
				x->array.self.i8[j] = y;
			}
			break;
		default:
			internal_error("subseq");
		}
		break;

	case t_string:
		x = seq;
		for (i = 0, j = x->string.fillp - 1;  i < j;  i++, --j) {
			k = x->string.self[i];
			x->string.self[i] = x->string.self[j];
			x->string.self[j] = k;
		}
		break;

	case t_bitvector:
		x = seq;
		for (i = x->vector.offset,
		     j = x->vector.fillp + x->vector.offset - 1;
		     i < j;
		     i++, --j) {
			k = x->vector.self.bit[i/CHAR_BIT]&(0200>>i%CHAR_BIT);
			if (x->vector.self.bit[j/CHAR_BIT]&(0200>>j%CHAR_BIT))
				x->vector.self.bit[i/CHAR_BIT]
				|= 0200>>i%CHAR_BIT;
			else
				x->vector.self.bit[i/CHAR_BIT]
				&= ~(0200>>i%CHAR_BIT);
			if (k)
				x->vector.self.bit[j/CHAR_BIT]
				|= 0200>>j%CHAR_BIT;
			else
				x->vector.self.bit[j/CHAR_BIT]
				&= ~(0200>>j%CHAR_BIT);
		}
		break;
	default:
		FEwrong_type_argument(@'sequence', seq);
	}
	@(return seq)
}
