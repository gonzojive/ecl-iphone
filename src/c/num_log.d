/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    num_log.c  -- Logical operations on numbers.
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
#include <stdlib.h>
#include <ecl/internal.h>

/*
 * BIT OPERATIONS FOR FIXNUMS
 */

static cl_fixnum
ior_op(cl_fixnum i, cl_fixnum j)
{
	return(i | j);
}

static void
mpz_ior_op(cl_object i, cl_object j)
{
#ifdef WITH_GMP
	mpz_ior(i->big.big_num, i->big.big_num, j->big.big_num);
#else  /* WITH_GMP */
        i->big.big_num |= j->big.big_num;
#endif /* WITH_GMP */
}

static cl_fixnum
xor_op(cl_fixnum i, cl_fixnum j)
{
	return(i ^ j);
}

static void
mpz_xor_op(cl_object i, cl_object j)
{
#ifdef WITH_GMP
	mpz_xor(i->big.big_num, i->big.big_num, j->big.big_num);
#else  /* WITH_GMP */
        i->big.big_num ^= j->big.big_num;
#endif /* WITH_GMP */
}

static cl_fixnum
and_op(cl_fixnum i, cl_fixnum j)
{
	return(i & j);
}

static void
mpz_and_op(cl_object i, cl_object j)
{
#ifdef WITH_GMP
	mpz_and(i->big.big_num, i->big.big_num, j->big.big_num);
#else  /* WITH_GMP */
        i->big.big_num &= j->big.big_num;
#endif /* WITH_GMP */
}

static cl_fixnum
eqv_op(cl_fixnum i, cl_fixnum j)
{
	return(~(i ^ j));
}

static void
mpz_eqv_op(cl_object i, cl_object j)
{
#ifdef WITH_GMP
	mpz_xor(i->big.big_num, i->big.big_num, j->big.big_num);
	mpz_com(i->big.big_num, i->big.big_num);
#else  /* WITH_GMP */
        i->big.big_num = ~(i->big.big_num ^ j->big.big_num);
#endif /* WITH_GMP */
}

static cl_fixnum
nand_op(cl_fixnum i, cl_fixnum j)
{
	return(~(i & j));
}

static void
mpz_nand_op(cl_object i, cl_object j)
{
#ifdef WITH_GMP
	mpz_and(i->big.big_num, i->big.big_num, j->big.big_num);
	mpz_com(i->big.big_num, i->big.big_num);
#else  /* WITH_GMP */
        i->big.big_num = ~(i->big.big_num & j->big.big_num);
#endif /* WITH_GMP */
}

static cl_fixnum
nor_op(cl_fixnum i, cl_fixnum j)
{
	return(~(i | j));
}

static void
mpz_nor_op(cl_object i, cl_object j)
{
#ifdef WITH_GMP
	mpz_ior(i->big.big_num, i->big.big_num, j->big.big_num);
	mpz_com(i->big.big_num, i->big.big_num);
#else  /* WITH_GMP */
        i->big.big_num = ~(i->big.big_num | j->big.big_num);
#endif /* WITH_GMP */
}

static cl_fixnum
andc1_op(cl_fixnum i, cl_fixnum j)
{
	return((~i) & j);
}

static void
mpz_andc1_op(cl_object i, cl_object j)
{
#ifdef WITH_GMP
	mpz_com(i->big.big_num, i->big.big_num);
	mpz_and(i->big.big_num, i->big.big_num, j->big.big_num);
#else  /* WITH_GMP */
        i->big.big_num = (~i->big.big_num) & (big_num_t)j;
#endif /* WITH_GMP */
}

static cl_fixnum
andc2_op(cl_fixnum i, cl_fixnum j)
{
	return(i & (~j));
}

static void mpz_orc1_op(cl_object, cl_object);

static void
mpz_andc2_op(cl_object i, cl_object j)
{
#ifdef WITH_GMP
	/* (i & ~j) = ~((~i) | j) */
	mpz_orc1_op(i, j);
	mpz_com(i->big.big_num, i->big.big_num);
#else  /* WITH_GMP */
        i->big.big_num = i->big.big_num & (~j->big.big_num);
#endif /* WITH_GMP */
}

static cl_fixnum
orc1_op(cl_fixnum i, cl_fixnum j)
{
	return((~i) | j);
}

static void
mpz_orc1_op(cl_object i, cl_object j)
{
#ifdef WITH_GMP
	mpz_com(i->big.big_num, i->big.big_num);
	mpz_ior(i->big.big_num, i->big.big_num, j->big.big_num);
#else  /* WITH_GMP */
        i->big.big_num = (~i->big.big_num) | j->big.big_num;
#endif /* WITH_GMP */
}

static cl_fixnum
orc2_op(cl_fixnum i, cl_fixnum j)
{
	return(i | (~j));
}

static void
mpz_orc2_op(cl_object i, cl_object j)
{
#ifdef WITH_GMP
	/* (i | ~j) = ~((~i) & j) */
	mpz_andc1_op(i, j);
	mpz_com(i->big.big_num, i->big.big_num);
#else  /* WITH_GMP */
        i->big.big_num = i->big.big_num | (~j->big.big_num);
#endif /* WITH_GMP */
}

static cl_fixnum
b_clr_op(cl_fixnum i, cl_fixnum j)
{
	return(0);
}

static void
mpz_b_clr_op(cl_object i, cl_object j)
{
#ifdef WITH_GMP
	mpz_set_si(i->big.big_num, 0);
#else  /* WITH_GMP */
        i->big.big_num = 0ll;
#endif /* WITH_GMP */
}

static cl_fixnum
b_set_op(cl_fixnum i, cl_fixnum j)
{
	return(-1);
}

static void
mpz_b_set_op(cl_object i, cl_object j)
{
#ifdef WITH_GMP
	mpz_set_si(i->big.big_num, -1);
#else  /* WITH_GMP */
        i->big.big_num = -1ll;
#endif /* WITH_GMP */
}

static cl_fixnum
b_1_op(cl_fixnum i, cl_fixnum j)
{
	return(i);
}

static void
mpz_b_1_op(cl_object i, cl_object j)
{
}

static cl_fixnum
b_2_op(cl_fixnum i, cl_fixnum j)
{
	return(j);
}

static void
mpz_b_2_op(cl_object i, cl_object j)
{
#ifdef WITH_GMP
	mpz_set(i->big.big_num, j->big.big_num);
#else  /* WITH_GMP */
        i->big.big_num = j->big.big_num;
#endif /* WITH_GMP */
}

static cl_fixnum
b_c1_op(cl_fixnum i, cl_fixnum j)
{
	return(~i);
}

static void
mpz_b_c1_op(cl_object i, cl_object j)
{
#ifdef WITH_GMP
	mpz_com(i->big.big_num, i->big.big_num);
#else  /* WITH_GMP */
        i->big.big_num = ~i->big.big_num;
#endif /* WITH_GMP */
}

static cl_fixnum
b_c2_op(cl_fixnum i, cl_fixnum j)
{
	return(~j);
}

static void
mpz_b_c2_op(cl_object i, cl_object j)
{
#ifdef WITH_GMP
	mpz_com(i->big.big_num, j->big.big_num);
#else  /* WITH_GMP */
        i->big.big_num = ~j->big.big_num;
#endif /* WITH_GMP */
}

typedef cl_fixnum (*bit_operator)(cl_fixnum, cl_fixnum);
typedef void (*bignum_bit_operator)(cl_object, cl_object);

static bit_operator fixnum_operations[16] = {
	b_clr_op,
	and_op,
	andc2_op,
	b_1_op,
	andc1_op,
	b_2_op,
	xor_op,
	ior_op,
	nor_op,
	eqv_op,
	b_c2_op,
	orc2_op,
	b_c1_op,
	orc1_op,
	nand_op,
	b_set_op};

static bignum_bit_operator bignum_operations[16] = {
	mpz_b_clr_op,
	mpz_and_op,
	mpz_andc2_op,
	mpz_b_1_op,
	mpz_andc1_op,
	mpz_b_2_op,
	mpz_xor_op,
	mpz_ior_op,
	mpz_nor_op,
	mpz_eqv_op,
	mpz_b_c2_op,
	mpz_orc2_op,
	mpz_b_c1_op,
	mpz_orc1_op,
	mpz_nand_op,
	mpz_b_set_op};


static cl_object
log_op(cl_narg narg, int op, cl_va_list ARGS)
{
#if 1
	cl_object x, y;
	/* FIXME! This can be optimized */
	x = cl_va_arg(ARGS);
	if (narg-- == 1) {
		assert_type_integer(x);
	} else {
		do {
			y = cl_va_arg(ARGS);
			x = ecl_boole(op, x, y);
		} while (--narg);
	}
	return x;
#else
	cl_object x, numi;
	bit_operator fix_log_op;
	bignum_bit_operator big_log_op;
	int i = 1;
	cl_fixnum j;

	x = cl_va_arg(ARGS);
	switch (type_of(x)) {
	    case t_fixnum:
		break;
	    case t_bignum:
		x = big_copy(x);	/* since big_log_op clobbers it */
		goto BIG_OP;
	    default:
		FEtype_error_integer(x);
	}
	if (narg == 1)
		return x;
	j = fix(x);
	fix_log_op = fixnum_operations[op];
	for (; i < narg; i++) {
		numi = cl_va_arg(ARGS);
		switch (type_of(numi)) {
		    case t_fixnum:
			j = (*fix_log_op)(j, fix(numi));
			break;
		    case t_bignum:
			big_log_op = bignum_operations[op];
			x = bignum1(j);
			goto BIG_OP2;
		    default:
			FEtype_error_integer(numi);
		}
	}
	return(MAKE_FIXNUM(j));

BIG_OP:
	if (narg == 1)
		return x;
	big_log_op = bignum_operations[op];
	for (; i < narg; i++) {
		numi = cl_va_arg(ARGS);
		switch (type_of(numi)) {
		    case t_fixnum: {
			cl_object z = big_register1_get();
			mpz_set_si(z->big.big_num, fix(numi));
			(*big_log_op)(x, z);
			big_register_free(z);
			break;
		    }
		    case t_bignum: BIG_OP2:
			(*big_log_op)(x, numi);
			break;
		    default:
			FEtype_error_integer(numi);
		}
	}
	return(big_normalize(x));
#endif
}

cl_object
ecl_boole(int op, cl_object x, cl_object y)
{
	switch (type_of(x)) {
	case t_fixnum:
		switch (type_of(y)) {
		case t_fixnum: {
			cl_fixnum (*fix_log_op)(cl_fixnum, cl_fixnum);
			fix_log_op = fixnum_operations[op];
			return MAKE_FIXNUM((*fix_log_op)(fix(x), fix(y)));
		}
		case t_bignum: {
			void (*big_log_op)(cl_object, cl_object);
			big_log_op = bignum_operations[op];
			x = bignum1(fix(x));
			(*big_log_op)(x, y);
			break;
		}
		default:
			FEtype_error_integer(y);
		}
		break;
	case t_bignum: {
		void (*big_log_op)(cl_object, cl_object);
		big_log_op = bignum_operations[op];
		x = big_copy(x);
		switch (type_of(y)) {
		case t_fixnum: {
			cl_object z = big_register1_get();
#ifdef WITH_GMP
			mpz_set_si(z->big.big_num, fix(y));
#else  /* WITH_GMP */
                        z->big.big_num = fix(y);
#endif /* WITH_GMP */
			(*big_log_op)(x, z);
			big_register_free(z);
			break;
		}
		case t_bignum:
			(*big_log_op)(x,y);
			break;
		default:
			FEtype_error_integer(y);
		}
		break;
	}
	default:
		FEtype_error_integer(x);
	}
	return big_normalize(x);
}

cl_object
cl_lognot(cl_object x)
{
	return @logxor(2,x,MAKE_FIXNUM(-1));
}

static cl_fixnum
count_bits(cl_object x)
{
	cl_fixnum count;

	switch (type_of(x)) {
	case t_fixnum: {
		cl_fixnum i = fix(x);
		cl_fixnum j = (i < 0) ? ~i : i;
		for (count=0 ; j ; j >>= 1)
			if (j & 1) count++;
		break;
	}
	case t_bignum:
#ifdef WITH_GMP
		if (big_sign(x) >= 0)
			count = mpz_popcount(x->big.big_num);
		else {
			cl_object z = big_register0_get();
			mpz_com(z->big.big_num, x->big.big_num);
			count = mpz_popcount(z->big.big_num);
			big_register_free(z);
		}
#else  /* WITH_GMP */
                {
                     big_num_t i = x->big.big_num;
                     if ( i<0 ) 
                          i = ~i;
                     for ( count=0 ; i ; i >>= 1 )
                          if ( i&1 ) count++;
                }
#endif /* WITH_GMP */
		break;
	default:
		FEtype_error_integer(x);
	}
	return count;
}

/*
   Left shift if w > 0, right shift if w < 0.
 */
cl_object
ecl_ash(cl_object x, cl_fixnum w)
{
	cl_object y;

	if (w == 0)
		return(x);
	y = big_register0_get();
	if (w < 0) {
		cl_index bits = -w;
		if (FIXNUMP(x)) {
			/* The result of shifting a number further than the number
			 * of digits it has is unpredictable in C. For instance, GCC
			 * on intel masks out all bits of "bits" beyond the 5 and
			 * it may happen that a shift of 37 becomes a shift of 5.
			 * Furthermore, in general, shifting negative numbers leads
			 * to implementation-specific results :-/
			 */
			cl_fixnum y = fix(x);
			if (bits >= FIXNUM_BITS) {
				y = (y < 0)? -1 : 0;
			} else {
				y >>= bits;
			}
			return MAKE_FIXNUM(y);
		}
#ifdef WITH_GMP
		mpz_div_2exp(y->big.big_num, x->big.big_num, bits);
#else  /* WITH_GMP */
                y->big.big_num = x->big.big_num >> bits;
#endif /* WITH_GMP */
	} else {
#ifdef WITH_GMP
		if (FIXNUMP(x)) {
			mpz_set_si(y->big.big_num, fix(x));
			x = y;
		}
		mpz_mul_2exp(y->big.big_num, x->big.big_num, (unsigned long)w);
#else  /* WITH_GMP */
                y->big.big_num = FIXNUMP(x) ? fix(x) : x->big.big_num;
                y->big.big_num <<= w;
#endif /* WITH_GMP */
	}
	return(big_register_normalize(y));
}

int
ecl_fixnum_bit_length(cl_fixnum i)
{
	int count;
	if (i < 0)
		i = ~i;
	for (count = 0; i && (count < FIXNUM_BITS); i >>= 1, count++)
		;
	return count;
}

@(defun logior (&rest nums)
@
	if (narg == 0)
		@(return MAKE_FIXNUM(0))
	/* INV: log_op() checks types and outputs first argument as default. */
	@(return log_op(narg, ECL_BOOLIOR, nums))
@)

@(defun logxor (&rest nums)
@
	if (narg == 0)
		@(return MAKE_FIXNUM(0))
	/* INV: log_op() checks types and outputs first argument as default. */
	@(return log_op(narg, ECL_BOOLXOR, nums))
@)

@(defun logand (&rest nums)
@
	if (narg == 0)
		@(return MAKE_FIXNUM(-1))
	/* INV: log_op() checks types and outputs first argument as default. */
	@(return log_op(narg, ECL_BOOLAND, nums))
@)

@(defun logeqv (&rest nums)
@
	if (narg == 0)
		@(return MAKE_FIXNUM(-1))
	/* INV: log_op() checks types and outputs first argument as default. */
	@(return log_op(narg, ECL_BOOLEQV, nums))
@)

cl_object
cl_lognand(cl_object x, cl_object y)
{
	@(return ecl_boole(ECL_BOOLNAND, x, y))
}

cl_object
cl_lognor(cl_object x, cl_object y)
{
	@(return ecl_boole(ECL_BOOLNOR, x, y))
}

cl_object
cl_logandc1(cl_object x, cl_object y)
{
	@(return ecl_boole(ECL_BOOLANDC1, x, y))
}

cl_object
cl_logandc2(cl_object x, cl_object y)
{
	@(return ecl_boole(ECL_BOOLANDC2, x, y))
}

cl_object
cl_logorc1(cl_object x, cl_object y)
{
	@(return ecl_boole(ECL_BOOLORC1, x, y))
}

cl_object
cl_logorc2(cl_object x, cl_object y)
{
	@(return ecl_boole(ECL_BOOLORC2, x, y))
}

static int
coerce_to_logical_operator(cl_object o)
{
	cl_fixnum op;
	op = fixint(o);
	if (op < 0 || op > ECL_BOOLSET)
		FEerror("~S is an invalid logical operator.", 1, o);
	return op;
}

cl_object
cl_boole(cl_object o, cl_object x, cl_object y)
{
	/* INV: log_op2() checks types */
	@(return ecl_boole(coerce_to_logical_operator(o), x, y))
}

cl_object
cl_logbitp(cl_object p, cl_object x)
{
	bool i;

	assert_type_integer(x);
	if (FIXNUMP(p)) {
		cl_index n = fixnnint(p);
		if (FIXNUMP(x)) {
			cl_fixnum y = fix(x);
			if (n >= FIXNUM_BITS) {
				i = (y < 0);
			} else {
				i = ((y >> n) & 1);
			}
		} else {
#ifdef WITH_GMP
			i = mpz_tstbit(x->big.big_num, n);
#else  /* WITH_GMP */
                        if ( n >= 8*sizeof(big_num_t) ) {
                                i = (x->big.big_num < 0);
                        } else {
                                i = (x->big.big_num >> n) & 1;
                        }
#endif /* WITH_GMP */
		}
	} else {
		assert_type_non_negative_integer(p);
		if (FIXNUMP(x))
			i = (fix(x) < 0);
		else
			i = (big_sign(x) < 0);
	}
	@(return (i ? Ct : Cnil))
}

cl_object
cl_ash(cl_object x, cl_object y)
{
	cl_object r;
	int sign_x;

        assert_type_integer(x);
	assert_type_integer(y);
	if (FIXNUMP(y))
	  r = ecl_ash(x, fix(y));
	else {
	  /*
	    bit position represented by bignum is probably
	    out of our address space. So, result is returned
	    according to sign of integer.
	    */
	  if (FIXNUMP(x))
	    if (FIXNUM_MINUSP(x))
	      sign_x = -1;
	    else if (x == MAKE_FIXNUM(0))
	      sign_x = 0;
	    else
	      sign_x = 1;
	  else
	    sign_x = big_sign(x);
	  if (big_sign(y) < 0)
	    if (sign_x < 0)
	      r = MAKE_FIXNUM(-1);
	    else
	      r = MAKE_FIXNUM(0);
	  else if (sign_x == 0)
	    r = x;
	  else
	    FEerror("Insufficient memory.", 0);
	}
	@(return r)
}

cl_object
cl_logcount(cl_object x)
{
	@(return MAKE_FIXNUM(count_bits(x)))
}

cl_object
cl_integer_length(cl_object x)
{
	int count;
	cl_fixnum i;

	switch (type_of(x)) {
	case t_fixnum:
		i = fix(x);
		count = ecl_fixnum_bit_length(i);
		break;
	case t_bignum:
		if (big_sign(x) < 0)
			x = cl_lognot(x);
#ifdef WITH_GMP
		count = mpz_sizeinbase(x->big.big_num, 2);
#else  /* WITH_GMP */
                for ( i=(8*sizeof(big_num_t))-1 ; i>0 ; i-- )
                        if ( (x->big.big_num >> i) & 1 ) {
                                count = i;
                                break;
                        }
#endif /* WITH_GMP */
		break;
	default:
		FEtype_error_integer(x);
	}
	@(return MAKE_FIXNUM(count))
}

cl_object
si_bit_array_op(cl_object o, cl_object x, cl_object y, cl_object r)
{
	cl_fixnum i, j, n, d;
	cl_object r0;
	bit_operator op;
	bool replace = FALSE;
	int xi, yi, ri;
	byte *xp, *yp, *rp;
	int xo, yo, ro;

	if (type_of(x) == t_bitvector) {
		d = x->vector.dim;
		xp = x->vector.self.bit;
		xo = x->vector.offset;
		if (type_of(y) != t_bitvector)
			goto ERROR;
		if (d != y->vector.dim)
			goto ERROR;
		yp = y->vector.self.bit;
		yo = y->vector.offset;
		if (r == Ct)
			r = x;
		if (r != Cnil) {
			if (type_of(r) != t_bitvector)
				goto ERROR;
			if (r->vector.dim != d)
				goto ERROR;
			i = (r->vector.self.bit - xp)*8 + (r->vector.offset - xo);
			if ((i > 0 && i < d) || (i < 0 && -i < d)) {
				r0 = r;
				r = Cnil;
				replace = TRUE;
				goto L1;
			}
			i = (r->vector.self.bit - yp)*8 + (r->vector.offset - yo);
			if ((i > 0 && i < d) || (i < 0 && -i < d)) {
				r0 = r;
				r = Cnil;
				replace = TRUE;
			}
		}
	L1:
		if (Null(r)) {
			r = si_make_vector(@'bit', MAKE_FIXNUM(d), Cnil, Cnil, Cnil, Cnil);
		}
	} else {
		if (type_of(x) != t_array)
			goto ERROR;
		if ((cl_elttype)x->array.elttype != aet_bit)
			goto ERROR;
		d = x->array.dim;
		xp = x->vector.self.bit;
		xo = x->vector.offset;
		if (type_of(y) != t_array)
			goto ERROR;
		if ((cl_elttype)y->array.elttype != aet_bit)
			goto ERROR;
		if (x->array.rank != y->array.rank)
			goto ERROR;
		yp = y->vector.self.bit;
		yo = y->vector.offset;
		for (i = 0;  i < x->array.rank;  i++)
			if (x->array.dims[i] != y->array.dims[i])
				goto ERROR;
		if (r == Ct)
			r = x;
		if (r != Cnil) {
			if (type_of(r) != t_array)
				goto ERROR;
			if ((cl_elttype)r->array.elttype != aet_bit)
				goto ERROR;
			if (r->array.rank != x->array.rank)
				goto ERROR;
			for (i = 0;  i < x->array.rank;  i++)
				if (r->array.dims[i] != x->array.dims[i])
					goto ERROR;
			i = (r->vector.self.bit - xp)*8 + (r->vector.offset - xo);
			if ((i > 0 && i < d) || (i < 0 && -i < d)) {
				r0 = r;
				r = Cnil;
				replace = TRUE;
				goto L2;
			} 
			i = (r->vector.self.bit - yp)*8 + (r->vector.offset - yo);
			if ((i > 0 && i < d) || (i < 0 && -i < d)) {
				r0 = r;
				r = Cnil;
				replace = TRUE;
			}
		}
	L2:
		if (Null(r)) {
		  r = cl_alloc_object(t_array);
		  r->array.self.t = NULL;
		  r->array.displaced = Cnil;
		  r->array.rank = x->array.rank;
		  r->array.dims = x->array.dims;
		  r->array.elttype = aet_bit;
		  r->array.dim = x->array.dim;
		  r->array.adjustable = FALSE;
		  ecl_array_allocself(r);
		}
	}
	rp = r->vector.self.bit;
	ro = r->vector.offset;
	op = fixnum_operations[coerce_to_logical_operator(o)];

#define	set_high(place, nbits, value) \
	(place)=((place)&~(-0400>>(nbits)))|((value)&(-0400>>(nbits)))

#define	set_low(place, nbits, value) \
	(place)=((place)&(-0400>>(8-(nbits))))|((value)&~(-0400>>(8-(nbits))))

#define	extract_byte(integer, pointer, index, offset) \
	(integer) = (pointer)[(index)+1] & 0377; \
	(integer) = ((pointer)[index]<<(offset))|((integer)>>(8-(offset)))

#define	store_byte(pointer, index, offset, value) \
	set_low((pointer)[index], 8-(offset), (value)>>(offset)); \
	set_high((pointer)[(index)+1], offset, (value)<<(8-(offset)))

	if (xo == 0 && yo == 0 && ro == 0) {
		for (n = d/8, i = 0;  i < n;  i++)
			rp[i] = (*op)(xp[i], yp[i]);
		if ((j = d%8) > 0)
			set_high(rp[n], j, (*op)(xp[n], yp[n]));
		if (!replace)
			@(return r)
	} else {
		for (n = d/8, i = 0;  i <= n;  i++) {
			extract_byte(xi, xp, i, xo);
			extract_byte(yi, yp, i, yo);
			if (i == n) {
				if ((j = d%8) == 0)
					break;
				extract_byte(ri, rp, n, ro);
				set_high(ri, j, (*op)(xi, yi));
			} else
				ri = (*op)(xi, yi);
			store_byte(rp, i, ro, ri);
		}
		if (!replace)
			@(return r)
	}
	rp = r0->vector.self.bit;
	ro = r0->vector.offset;
	for (n = d/8, i = 0;  i <= n;  i++) {
		if (i == n) {
			if ((j = d%8) == 0)
				break;
			extract_byte(ri, rp, n, ro);
			set_high(ri, j, r->vector.self.bit[n]);
		} else
			ri = r->vector.self.bit[i];
		store_byte(rp, i, ro, ri);
	}
	@(return r0)
ERROR:
	FEerror("Illegal arguments for bit-array operation.", 0);
}
