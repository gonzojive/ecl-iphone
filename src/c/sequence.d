/*
    sequence.d -- Sequence routines.
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

#undef endp

#define	endp(obje)	(endp_temp = (obje), CONSP(endp_temp) ? \
			 FALSE : endp_temp == Cnil ? TRUE : \
			 (FEwrong_type_argument(Slist, endp_temp), FALSE))

/*
	I know the following name is not good.
*/
cl_object
alloc_simple_vector(int l, enum aelttype aet)
{
	cl_object x;

	x = alloc_object(t_vector);
	x->vector.hasfillp = FALSE;
	x->vector.adjustable = FALSE;
	x->vector.displaced = Cnil;
	x->vector.dim = x->vector.fillp = l;
	x->vector.self.t = NULL;
	x->vector.elttype = (short)aet;
	return(x);
}

cl_object
alloc_simple_bitvector(int l)
{
	cl_object x;

	x = alloc_object(t_bitvector);
	x->vector.hasfillp = FALSE;
	x->vector.adjustable = FALSE;
	x->vector.displaced = Cnil;
	x->vector.dim = x->vector.fillp = l;
	x->vector.offset = 0;
	x->vector.self.bit = NULL;
	return(x);
}

@(defun elt (x i)
@
	@(return elt(x, fixint(i)))
@)

cl_object
elt(cl_object seq, cl_fixnum index)
{
	cl_fixnum i;
	cl_object l;
	cl_object endp_temp;

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
		return(code_char(seq->string.self[index]));

	default:
		FEerror("~S is not a sequence.", 1, seq);
	}
E:
	FEtype_error_index(MAKE_FIXNUM(index));
}

@(defun si::elt_set (seq index val)
@
	@(return elt_set(seq, fixint(index), val))
@)

cl_object
elt_set(cl_object seq, cl_fixnum index, cl_object val)
{
	int i;
	cl_object l;
	cl_object endp_temp;

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
		FEerror("~S is not a sequence.", 1, seq);
	}
E:
	FEtype_error_index(MAKE_FIXNUM(index));
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
		FEwrong_type_argument(Ssequence, sequence);

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
			@(return `copy_list(sequence)`)
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
		x = alloc_simple_vector(e - s, sequence->vector.elttype);
		array_allocself(x);
		switch ((enum aelttype)sequence->vector.elttype) {
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
		x = alloc_simple_string(e - s);
		x->string.self = alloc_atomic(e - s + 1);
		x->string.self[e-s] = '\0';
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
		x = alloc_simple_bitvector(e - s);
		x->vector.self.bit = alloc_atomic((e-s+CHAR_BIT-1)/CHAR_BIT);
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
		FEwrong_type_argument(Ssequence, x);
	}

ILLEGAL_START_END:
	FEerror("~S and ~S are illegal as :START and :END~%\
for the sequence ~S.", 3, start, end, sequence);
@)

@(defun copy_seq (x)
@
	/* INV: #'subseq outputs only one value */
	return Lsubseq(2, x, MAKE_FIXNUM(0));
@)

@(defun length (x)
@
	@(return MAKE_FIXNUM(length(x)))
@)

cl_fixnum
length(cl_object x)
{
	cl_fixnum i;

	switch (type_of(x)) {
	case t_symbol:
		if (Null(x))
			return(0);
		FEwrong_type_argument(Ssequence, x);

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
		FEwrong_type_argument(Ssequence, x);
	}
}

@(defun reverse (x)
@
	@(return reverse(x))
@)

cl_object
reverse(cl_object seq)
{
	cl_object x, y, v;
	int i, j, k;
	cl_object endp_temp;

	switch (type_of(seq)) {
	case t_symbol:
		if (Null(seq))
			return(Cnil);
		FEwrong_type_argument(Ssequence, seq);

	case t_cons:
		v = Cnil;
		for (x = seq;  !endp(x);  x = CDR(x))
			v = CONS(CAR(x), v);
		return(v);

	case t_vector:
		x = seq;
		k = x->vector.fillp;
		y = alloc_simple_vector(k, x->vector.elttype);
		array_allocself(y);
		switch ((enum aelttype)x->vector.elttype) {
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
		default:
			internal_error("reverse");
		}
		return(y);

	case t_string:
		x = seq;
		y = alloc_simple_string(x->string.fillp);
		y->string.self = alloc_atomic(x->string.fillp+1);
		for (j = x->string.fillp - 1, i = 0;  j >=0;  --j, i++)
			y->string.self[j] = x->string.self[i];
		y->string.self[x->string.fillp] = '\0';
		return(y);

	case t_bitvector:
		x = seq;
		y = alloc_simple_bitvector(x->vector.fillp);
		y->vector.self.bit = alloc_atomic((x->vector.fillp+CHAR_BIT-1)/CHAR_BIT);
		for (j = x->vector.fillp - 1, i = x->vector.offset;
		     j >=0;
		     --j, i++)
			if (x->vector.self.bit[i/CHAR_BIT]&(0200>>i%CHAR_BIT))
				y->vector.self.bit[j/CHAR_BIT] |= 0200>>j%CHAR_BIT;
			else
				y->vector.self.bit[j/CHAR_BIT] &= ~(0200>>j%CHAR_BIT);
		return(v);

	default:
		FEwrong_type_argument(Ssequence, seq);
	}
}

@(defun nreverse (x)
@
	@(return nreverse(x))
@)

cl_object
nreverse(cl_object seq)
{
	cl_object x, y, z;
	int i, j, k;
	cl_object endp_temp;

	switch (type_of(seq)) {
	case t_symbol:
		if (Null(seq))
			return(Cnil);
		FEwrong_type_argument(Ssequence, seq);

	case t_cons:
		for (x = Cnil, y = seq;  !endp(CDR(y));) {
			z = y;
			y = CDR(y);
			CDR(z) = x;
			x = z;
		}
		CDR(y) = x;
		return(y);

	case t_vector:
		x = seq;
		k = x->vector.fillp;
		switch ((enum aelttype)x->vector.elttype) {
		case aet_object:
		case aet_fix:
			for (i = 0, j = k - 1;  i < j;  i++, --j) {
				y = x->vector.self.t[i];
				x->vector.self.t[i] = x->vector.self.t[j];
				x->vector.self.t[j] = y;
			}
			return(seq);

		case aet_sf:
			for (i = 0, j = k - 1;  i < j;  i++, --j) {
				float y = x->array.self.sf[i];
				x->array.self.sf[i] = x->array.self.sf[j];
				x->array.self.sf[j] = y;
			}
			return(seq);

		case aet_lf:
			for (i = 0, j = k - 1;  i < j;  i++, --j) {
				double y = x->array.self.lf[i];
				x->array.self.lf[i] = x->array.self.lf[j];
				x->array.self.lf[j] = y;
			}
			return(seq);
		default:
			internal_error("subseq");
		}

	case t_string:
		x = seq;
		for (i = 0, j = x->string.fillp - 1;  i < j;  i++, --j) {
			k = x->string.self[i];
			x->string.self[i] = x->string.self[j];
			x->string.self[j] = k;
		}
		return(seq);

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
		return(seq);

	default:
		FEwrong_type_argument(Ssequence, seq);
	}
}
