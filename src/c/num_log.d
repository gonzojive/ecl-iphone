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

#include "ecl.h"

#define BOOLCLR		0
#define BOOLSET		017
#define BOOL1		03
#define BOOL2		05
#define BOOLC1		014
#define BOOLC2		012
#define BOOLAND		01
#define BOOLIOR		07
#define BOOLXOR		06
#define BOOLEQV		011
#define BOOLNAND	016
#define BOOLNOR		010
#define BOOLANDC1	04
#define BOOLANDC2	02
#define BOOLORC1	015
#define BOOLORC2	013

/*
	x : fixnum or bignum (may be not normalized)
	y : integer
   returns
	fixnum or bignum ( not normalized )
*/

typedef cl_fixnum (*bit_operator)(cl_fixnum, cl_fixnum);

static cl_object big_log_op(cl_object x, cl_object y, bit_operator op);

static cl_object
log_op(int narg, bit_operator op, cl_va_list ARGS)
{
	cl_type t;
	cl_object x, numi;
	int i = 1, j;

	if (narg < 2) FEtoo_few_arguments(narg);
	x = cl_va_arg(ARGS);
	t = type_of(x);
	if (t == t_bignum) {
		x = big_copy(x);	/* since big_log_op clobbers it */
		goto BIG_OP;
	} if (t != t_fixnum) {
		FEtype_error_integer(x);
	}
	j = fix(x);
	for (; i < narg; i++) {
	  numi = cl_va_arg(ARGS);
	  t = type_of(numi);
	  if (t == t_bignum) {
	    x = big_log_op(bignum1(j), numi, op);
	    i++;
	    goto BIG_OP;
	  } else if (t != t_fixnum) {
	    FEtype_error_integer(numi);
	  }
	  j = (*op)(j, fix(numi));
	}
	return(MAKE_FIXNUM(j));

BIG_OP:
	for (; i < narg; i++)	  
	  x = big_log_op(x, cl_va_arg(ARGS), op);
	return(big_normalize(x));
}

static cl_object
log_op2(cl_object x, cl_object y, bit_operator op)
{
	switch (type_of(x)) {
	case t_fixnum:
		if (FIXNUMP(y))
			return MAKE_FIXNUM((*op)(fix(x),fix(y)));
		else
			x = big_log_op(bignum1(fix(x)), y, op);
		break;
	case t_bignum:
		x = big_log_op(big_copy(x), y, op);
		break;
	default:
		FEtype_error_integer(x);
	}
	return big_normalize(x);
}

/*
	big_log_op(x, y, op) performs the logical operation op on
	bignum x and integer y, and returns the result in x destructively.
*/
static cl_object
big_log_op(cl_object x, cl_object y, bit_operator op)
{
	int flag;
	int y_size, x_size;
	mp_limb_t word, *x_limbs, *y_limbs;

	if (FIXNUMP(y)) {
		cl_object z = big_register1_get();
		mpz_set_si(y->big.big_num, fix(y));
		y = z;
	} else if (type_of(y) != t_bignum) {
		FEtype_error_integer(y);
	}
	if (big_sign(y) > 0)
		flag = 0;
	else {
		cl_object z = big_register2_get();
		mpz_com(z->big.big_num, y->big.big_num);
		y = z;
		flag = ~0;
	}
	y_size = y->big.big_size;
	y_limbs = y->big.big_limbs;
	if (big_sign(x) > 0)
		flag = (*op)(0,flag);
	else {
		flag = (*op)(0,flag);
		mpz_com(x->big.big_num, x->big.big_num);
	}
	x_size = x->big.big_size;
	if (y_size > x_size) {
		x->big.big_size = x_size = y_size;
		mpz_realloc(x->big.big_num, x_size);
	}
	x_limbs = x->big.big_limbs;

	/* Compute the logical operation */
	for (word = 0; x_size--; ) {
		mp_limb_t aux = (*op)(x_limbs[x_size], y_limbs[x_size]);
		x_limbs[x_size] = aux;
		word |= aux;
	}
	/* When output is zero, notice that */
	if (word == 0)
		x->big.big_size = 0;
	/* If result should be a negative number, perform two's complement. */
	if (flag)
		mpz_com(x->big.big_num, x->big.big_num);
	return x;
}

static cl_fixnum
ior_op(cl_fixnum i, cl_fixnum j)
{
	return(i | j);
}

static cl_fixnum
xor_op(cl_fixnum i, cl_fixnum j)
{
	return(i ^ j);
}

static cl_fixnum
and_op(cl_fixnum i, cl_fixnum j)
{
	return(i & j);
}

static cl_fixnum
eqv_op(cl_fixnum i, cl_fixnum j)
{
	return(~(i ^ j));
}

static cl_fixnum
nand_op(cl_fixnum i, cl_fixnum j)
{
	return(~(i & j));
}

static cl_fixnum
nor_op(cl_fixnum i, cl_fixnum j)
{
	return(~(i | j));
}

static cl_fixnum
andc1_op(cl_fixnum i, cl_fixnum j)
{
	return((~i) & j);
}

static cl_fixnum
andc2_op(cl_fixnum i, cl_fixnum j)
{
	return(i & (~j));
}

static cl_fixnum
orc1_op(cl_fixnum i, cl_fixnum j)
{
	return((~i) | j);
}

static cl_fixnum
orc2_op(cl_fixnum i, cl_fixnum j)
{
	return(i | (~j));
}

static cl_fixnum
b_clr_op(cl_fixnum i, cl_fixnum j)
{
	return(0);
}

static cl_fixnum
b_set_op(cl_fixnum i, cl_fixnum j)
{
	return(-1);
}

static cl_fixnum
b_1_op(cl_fixnum i, cl_fixnum j)
{
	return(i);
}

static cl_fixnum
b_2_op(cl_fixnum i, cl_fixnum j)
{
	return(j);
}

static cl_fixnum
b_c1_op(cl_fixnum i, cl_fixnum j)
{
	return(~i);
}

static cl_fixnum
b_c2_op(cl_fixnum i, cl_fixnum j)
{
	return(~j);
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
		if (big_sign(x) >= 0)
			count = mpz_popcount(x->big.big_num);
		else {
			cl_object z = big_register0_get();
			mpz_com(z->big.big_num, x->big.big_num);
			count = mpz_popcount(x->big.big_num);
			big_register_free(z);
		}
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
integer_shift(cl_object x, cl_fixnum w)
{
	cl_object y;

	if (w == 0)
		return(x);
	y = big_register0_get();
	if (w < 0) {
		if (FIXNUMP(x)) 
			return MAKE_FIXNUM(fix(x) >> -w);
		mpz_div_2exp(y->big.big_num, x->big.big_num, -w);
	} else {
		if (FIXNUMP(x)) {
			mpz_set_si(y->big.big_num, fix(x));
			x = y;
		}
		mpz_mul_2exp(y->big.big_num, x->big.big_num, w);
	}
	return(big_register_normalize(y));
}

int
int_bit_length(int i)
{
	register int	count, j;

	count = 0;
	for (j = 0; j < 31 ; j++)
		if (((i >> j) & 1) == 1) count = j + 1;
	return(count);
}

@(defun logior (&rest nums)
@
	if (narg == 0)
		@(return MAKE_FIXNUM(0))
	/* INV: log_op() checks types and outputs first argument as default. */
	@(return log_op(narg, ior_op, nums))
@)

@(defun logxor (&rest nums)
@
	if (narg == 0)
		@(return MAKE_FIXNUM(0))
	/* INV: log_op() checks types and outputs first argument as default. */
	@(return log_op(narg, xor_op, nums))
@)

@(defun logand (&rest nums)
@
	if (narg == 0)
		@(return MAKE_FIXNUM(-1))
	/* INV: log_op() checks types and outputs first argument as default. */
	@(return log_op(narg, and_op, nums))
@)

@(defun logeqv (&rest nums)
@
	if (narg == 0)
		@(return MAKE_FIXNUM(-1))
	/* INV: log_op() checks types and outputs first argument as default. */
	@(return log_op(narg, eqv_op, nums))
@)

cl_object
cl_lognand(cl_object x, cl_object y)
{
	@(return log_op2(x, y, nand_op))
}

cl_object
cl_lognor(cl_object x, cl_object y)
{
	@(return log_op2(x, y, nor_op))
}

cl_object
cl_logandc1(cl_object x, cl_object y)
{
	@(return log_op2(x, y, andc1_op))
}

cl_object
cl_logandc2(cl_object x, cl_object y)
{
	@(return log_op2(x, y, andc2_op))
}

cl_object
cl_logorc1(cl_object x, cl_object y)
{
	@(return log_op2(x, y, orc1_op))
}

cl_object
cl_logorc2(cl_object x, cl_object y)
{
	@(return log_op2(x, y, orc2_op))
}

cl_object
cl_boole(cl_object o, cl_object x, cl_object y)
{
	bit_operator op;

	/* INV: log_op() checks types */
	switch(fixint(o)) {
		case BOOLCLR:	op = b_clr_op;	break;
		case BOOLSET:	op = b_set_op;	break;
		case BOOL1:	op = b_1_op;	break;
		case BOOL2:	op = b_2_op;	break;
		case BOOLC1:	op = b_c1_op;	break;
		case BOOLC2:	op = b_c2_op;	break;
		case BOOLAND:	op = and_op;	break;
		case BOOLIOR:	op = ior_op;	break;
		case BOOLXOR:	op = xor_op;	break;
		case BOOLEQV:	op = eqv_op;	break;
		case BOOLNAND:	op = nand_op;	break;
		case BOOLNOR:	op = nor_op;	break;
		case BOOLANDC1:	op = andc1_op;	break;
		case BOOLANDC2:	op = andc2_op;	break;
		case BOOLORC1:	op = orc1_op;	break;
		case BOOLORC2:	op = orc2_op;	break;
		default:
			FEerror("~S is an invalid logical operator.",
				1, o);
	}
	@(return log_op2(x, y, op))
}

cl_object
cl_logbitp(cl_object p, cl_object x)
{
	bool	i;
	int	n;

	assert_type_integer(x);
	if (FIXNUMP(p)) {
		cl_fixnum n = fixnnint(p);
		if (n < 0)
			FEtype_error_index(p);
		if (FIXNUMP(x))
			i = ((fix(x) >> n) & 1);
		else
			i = mpz_tstbit(x->big.big_num, n);
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
	  r = integer_shift(x, fix(y));
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
	int	count, i;

	switch (type_of(x)) {
	case t_fixnum:
		i = fix(x);
		count = int_bit_length((i < 0) ? ~i : i);
		break;
	case t_bignum: {
	  	int last = abs(x->big.big_size) - 1;
		i = x->big.big_limbs[last];
		count = last * (sizeof(mp_limb_t) * 8) + int_bit_length(i);
		break;
	}
	default:
		FEtype_error_integer(x);
	}
	@(return MAKE_FIXNUM(count))
}

void
init_num_log(void)
{
	SYM_VAL(@'BOOLE-CLR') = MAKE_FIXNUM(BOOLCLR);
	SYM_VAL(@'BOOLE-SET') = MAKE_FIXNUM(BOOLSET);
	SYM_VAL(@'BOOLE-1') = MAKE_FIXNUM(BOOL1);
	SYM_VAL(@'BOOLE-2') = MAKE_FIXNUM(BOOL2);
	SYM_VAL(@'BOOLE-C1') = MAKE_FIXNUM(BOOLC1);
	SYM_VAL(@'BOOLE-C2') = MAKE_FIXNUM(BOOLC2);
	SYM_VAL(@'BOOLE-AND') = MAKE_FIXNUM(BOOLAND);
	SYM_VAL(@'BOOLE-IOR') = MAKE_FIXNUM(BOOLIOR);
	SYM_VAL(@'BOOLE-XOR') = MAKE_FIXNUM(BOOLXOR);
	SYM_VAL(@'BOOLE-EQV') = MAKE_FIXNUM(BOOLEQV);
	SYM_VAL(@'BOOLE-NAND') = MAKE_FIXNUM(BOOLNAND);
	SYM_VAL(@'BOOLE-NOR') = MAKE_FIXNUM(BOOLNOR);
	SYM_VAL(@'BOOLE-ANDC1') = MAKE_FIXNUM(BOOLANDC1);
	SYM_VAL(@'BOOLE-ANDC2') = MAKE_FIXNUM(BOOLANDC2);
	SYM_VAL(@'BOOLE-ORC1') = MAKE_FIXNUM(BOOLORC1);
	SYM_VAL(@'BOOLE-ORC2') = MAKE_FIXNUM(BOOLORC2);
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
		  r->array.rank = 1;
		  r->array.dims = NULL;
		  r->array.elttype = get_elttype(@'bit');
		  r->array.dims = (cl_index *)cl_alloc_atomic_align(x->array.dim * sizeof(cl_index), sizeof(cl_index));
		  r->array.dim = x->array.dim;
		  r->array.adjustable = FALSE;
		  array_allocself(r);
		}
	}
	rp = r->vector.self.bit;
	ro = r->vector.offset;
	switch(fixint(o)) {
		case BOOLCLR:	op = b_clr_op;	break;
		case BOOLSET:	op = b_set_op;	break;
		case BOOL1:	op = b_1_op;	break;
		case BOOL2:	op = b_2_op;	break;
		case BOOLC1:	op = b_c1_op;	break;
		case BOOLC2:	op = b_c2_op;	break;
		case BOOLAND:	op = and_op;	break;
		case BOOLIOR:	op = ior_op;	break;
		case BOOLXOR:	op = xor_op;	break;
		case BOOLEQV:	op = eqv_op;	break;
		case BOOLNAND:	op = nand_op;	break;
		case BOOLNOR:	op = nor_op;	break;
		case BOOLANDC1:	op = andc1_op;	break;
		case BOOLANDC2:	op = andc2_op;	break;
		case BOOLORC1:	op = orc1_op;	break;
		case BOOLORC2:	op = orc2_op;	break;
		default:
			FEerror("~S is an invalid logical operator.", 1, o);
	}

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
