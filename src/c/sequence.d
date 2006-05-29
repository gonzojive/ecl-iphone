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

#include <ecl/ecl.h>
#include <limits.h>
#include <ecl/ecl-inl.h>

/*
	I know the following name is not good.
*/
cl_object
cl_alloc_simple_vector(cl_index l, cl_elttype aet)
{
	cl_object x;

	if (aet == aet_bc)
		return cl_alloc_simple_base_string(l);
	if (aet == aet_bit) {
		x = cl_alloc_object(t_bitvector);
		x->vector.hasfillp = FALSE;
		x->vector.adjustable = FALSE;
		x->vector.displaced = Cnil;
		x->vector.dim = x->vector.fillp = l;
		x->vector.offset = 0;
		x->vector.self.bit = NULL;
	} else {
		x = cl_alloc_object(t_vector);
		x->vector.hasfillp = FALSE;
		x->vector.adjustable = FALSE;
		x->vector.displaced = Cnil;
		x->vector.dim = x->vector.fillp = l;
		x->vector.self.t = NULL;
		x->vector.elttype = (short)aet;
	}
	array_allocself(x);
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

#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_vector:
	case t_bitvector:
		if (index >= seq->vector.fillp)
			goto E;
		return(aref(seq, index));

	case t_base_string:
		if (index >= seq->base_string.fillp)
			goto E;
		return(CODE_CHAR(seq->base_string.self[index]));

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

#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_vector:
	case t_bitvector:
		if (index >= seq->vector.fillp)
			goto E;
		return(aset(seq, index, val));

	case t_base_string:
		if (index >= seq->base_string.fillp)
			goto E;
		/* INV: char_code() checks the type of `val' */
		seq->base_string.self[index] = char_code(val);
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

#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_vector:
	case t_bitvector:
	case t_base_string:
		if (s > sequence->vector.fillp)
			goto ILLEGAL_START_END;
		if (e < 0)
			e = sequence->vector.fillp;
		else if (e < s || e > sequence->vector.fillp)
			goto ILLEGAL_START_END;
		x = cl_alloc_simple_vector(e - s, array_elttype(sequence));
		ecl_copy_subarray(x, 0, sequence, s, e-s);
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

#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_vector:
	case t_base_string:
	case t_bitvector:
		return(x->vector.fillp);

	default:
		FEwrong_type_argument(@'sequence', x);
	}
}

cl_object
cl_reverse(cl_object seq)
{
	cl_object output, x;

	switch (type_of(seq)) {
	case t_symbol:
		if (Null(seq))
			output = Cnil;
		else
			FEwrong_type_argument(@'sequence', seq);
		break;
	case t_cons: {
		for (x = seq, output = Cnil;  !endp(x);  x = CDR(x))
			output = CONS(CAR(x), output);
		break;
	}
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_vector:
	case t_bitvector:
	case t_base_string:
		output = cl_alloc_simple_vector(seq->vector.fillp, array_elttype(seq));
		ecl_copy_subarray(output, 0, seq, 0, seq->vector.fillp);
		ecl_reverse_subarray(output, 0, seq->vector.fillp);
		break;

	default:
		FEwrong_type_argument(@'sequence', seq);
	}
	@(return output)
}

cl_object
cl_nreverse(cl_object seq)
{
	switch (type_of(seq)) {
	case t_symbol:
		if (!Null(seq))
			FEwrong_type_argument(@'sequence', seq);
		break;
	case t_cons: {
		cl_object x, y, z;
		for (x = Cnil, y = seq;  !endp(CDR(y));) {
			z = y;
			y = CDR(y);
			CDR(z) = x;
			x = z;
		}
		CDR(y) = x;
		seq = y;
		break;
	}
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_vector:
	case t_base_string:
	case t_bitvector:
		ecl_reverse_subarray(seq, 0, seq->vector.fillp);
		break;
	default:
		FEwrong_type_argument(@'sequence', seq);
	}
	@(return seq)
}
