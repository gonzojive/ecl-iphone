/*
    num_log.c  -- Logical operations on numbers.
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

static cl_object big_log_op(struct bignum *x, cl_object y, int (*op)());

static cl_object
log_op(int narg, int (*op)(), cl_object *nums)
{
	enum cl_type t;
	cl_object x, numi;
	int i = 1, j;

	if (narg < 2) FEtoo_few_arguments(&narg);
	x = *nums++;
	t = type_of(x);
	if (t == t_bignum) {
		x = big_copy(x);	/* since big_log_op clobbers it */
		goto BIG_OP;
	} if (t != t_fixnum) {
		FEtype_error_integer(x);
	}
	j = fix(x);
	for (; i < narg; i++) {
	  numi = *nums++;
	  t = type_of(numi);
	  if (t == t_bignum) {
	    x = big_log_op(&bignum1(j)->big, numi, op);
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
	  x = big_log_op(&x->big, *nums++, op);
	return(big_normalize(x));
}
/*
	big_log_op(x, y, op) performs the logical operation op on
	bignum x and integer y, and returns the result in x destructively.
*/
static cl_object
big_log_op(struct bignum *x, cl_object y, int (*op)())
{
	int	i, j;
	int 	y_size, x_size = x->big_size;
	mp_limb_t *y_limbs, *x_limbs = x->big_limbs;
	int	y_sign, x_sign = (big_sign((cl_object)x) < 0);

	if (FIXNUMP(y)) {
	  i = fix(y);
	  x_limbs[x_size-1] = (*op)(x_limbs[x_size-1], i);
	  y_sign = (i < 0);
	} else if (type_of(y) != t_bignum) {
	  FEtype_error_integer(y);
	} else {
	  y_sign = big_sign((cl_object)y) < 0;
	  x_size = abs(x->big_size);
	  y_size = abs(y->big.big_size);
	  y_limbs = y->big.big_limbs;

	  if (y_size > x_size) {
	    /* First loop finds the size of the result.  */
	    for (i = y_size - 1; i >= 0; i--) {
	      j = (i >= x_size) ? 0 : x_limbs[i];
	      if ((*op)(j, y_limbs[i]) != 0)
		break;
	    }
	    x_size = i + 1;

	    /* Handle allocation, now that we know exactly how much space is
	       needed for the result.  */
	    if (x->big_dim < x_size) {
	      _mpz_realloc(x->big_num, x_size);
	      x_limbs = x->big_limbs;
	    }
	  }
	  /* Second loop computes the real result.  */
	  for (i = 0 ; i < x_size; i++)
	    x_limbs[i] = (*op)(x_limbs[i], y_limbs[i]);
	}
	/*
	  Set the sign according to operation.
	  */
	x->big_size = (*op)(x_sign, y_sign)? -x_size : x_size;
	return((cl_object)x);
}

static int
ior_op(int i, int j)
{
	return(i | j);
}

static int
xor_op(int i, int j)
{
	return(i ^ j);
}

static int
and_op(int i, int j)
{
	return(i & j);
}

static int
eqv_op(int i, int j)
{
	return(~(i ^ j));
}

static int
nand_op(int i, int j)
{
	return(~(i & j));
}

static int
nor_op(int i, int j)
{
	return(~(i | j));
}

static int
andc1_op(int i, int j)
{
	return((~i) & j);
}

static int
andc2_op(int i, int j)
{
	return(i & (~j));
}

static int
orc1_op(int i, int j)
{
	return((~i) | j);
}

static int
orc2_op(int i, int j)
{
	return(i | (~j));
}

static int
b_clr_op(int i, int j)
{
	return(0);
}

static int
b_set_op(int i, int j)
{
	return(-1);
}

static int
b_1_op(int i, int j)
{
	return(i);
}

static int
b_2_op(int i, int j)
{
	return(j);
}

static int
b_c1_op(int i, int j)
{
	return(~i);
}

static int
b_c2_op(int i, int j)
{
	return(~j);
}

static int
big_bitp(cl_object	x, int p)
{
	if (p < 0)
		return 0;
	else {
#define BITS_PER_LIMB (sizeof(mp_limb_t)*8)
 	  	int size = x->big.big_size;
 		mp_limb_t *limbs = x->big.big_limbs;
 		int cell = p / BITS_PER_LIMB;
 		int bit = p % BITS_PER_LIMB;
 		if (size > 0)
		  if (cell > size)
		    return(size < 0);
		  else
		    return (limbs[cell] >> bit) & 1;
		else {
		  mp_size_t zero_bound;
		  size = -size;
		  /* Locate the least significant non-zero limb.  */
		  for (zero_bound = 0; limbs[zero_bound] == 0; zero_bound++)
		    ;
		  if (cell > size)
		    return 1;
		  else if (cell < zero_bound)
		    return 0;
		  else if (cell == zero_bound)
		    return (-limbs[cell] >> bit) & 1;
		  else /* cell > zero_bound */
		    return (~limbs[cell] >> bit) & 1;
		}
 	}
}

@(defun lognot (x)
@
	return Llogxor(1,x,MAKE_FIXNUM(-1));
@)

static int
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
		if (big_sign(x) < 0) {
			Llognot(1,x);
			VALUES(0) = x;
		}
		count = mpz_popcount(x->big.big_num);
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
integer_shift(cl_object x, int w)
{
	cl_object y;
	int cell, bits, i;
	
	if (w == 0) return(x);
	cell = w / 32;
	bits = w % 32;
	if (FIXNUMP(x)) {
		i = fix(x);
		if (i == 0) return(x);
		if (cell == 0) {
			if (w < 0) {
				if (i >= 0)
					return(MAKE_FIXNUM(i >> -w));
				else
					return(MAKE_FIXNUM(~((~i) >> -w)));
			}
			if (i > 0) {
				if (((~MOST_POSITIVE_FIX >> w) & i) == 0)
					return(MAKE_FIXNUM(i << w));
			} else {
				if (((MOST_NEGATIVE_FIX >> w) & ~i) == 0)
					return(MAKE_FIXNUM(i << w));
			}
		}
		x = bignum1(i);
	}
	y = big_register0_get();
	if (w < 0) {
	  mpz_div_2exp(y->big.big_num, x->big.big_num, -w);
	} else {
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
	/* INV: log_op() checks types */
	if (narg == 1)
		@(return va_arg(nums, cl_object))
	@(return log_op(narg, ior_op, (cl_object *)nums))
@)

@(defun logxor (&rest nums)
@
	if (narg == 0)
		@(return MAKE_FIXNUM(0))
	/* INV: log_op() checks types */
	if (narg == 1)
		@(return va_arg(nums, cl_object))
	@(return log_op(narg, xor_op, (cl_object *)nums))
@)

@(defun logand (&rest nums)
@
	if (narg == 0)
		@(return MAKE_FIXNUM(-1))
	/* INV: log_op() checks types */
	if (narg == 1)
		@(return va_arg(nums, cl_object))
	@(return log_op(narg, and_op, (cl_object *)nums))
@)

@(defun logeqv (&rest nums)
@
	if (narg == 0)
		@(return MAKE_FIXNUM(-1))
	/* INV: log_op() checks types */
	if (narg == 1)
		@(return va_arg(nums, cl_object))
	@(return log_op(narg, eqv_op, (cl_object *)nums))
@)

@(defun boole (o &rest nums)
	int	(*op)();
@
	/* FIXME! Is this check ok? */
	check_arg(3);
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
	@(return log_op(2, op, (cl_object *)nums))
@)

@(defun logbitp (p x)
	bool	i;
@
	assert_type_non_negative_integer(p);
	assert_type_integer(x);
	if (FIXNUMP(p))
		if (FIXNUMP(x))
			i = ((fix(x) >> fix(p)) & 1);
		else
			i = big_bitp(x, fix(p));
	else if (FIXNUMP(x))
		i = (fix(x) < 0);
	else
		i = (big_sign(x) < 0);
	@(return (i ? Ct : Cnil))
@)

@(defun ash (x y)
	cl_object r;
	int sign_x;
@
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
	    if (fix(x) > 0)
	      sign_x = 1;
	    else if (fix(x) == 0)
	      sign_x = 0;
	    else
	      sign_x = -1;
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
@)

@(defun logcount (x)
@
	@(return MAKE_FIXNUM(count_bits(x)))
@)

@(defun integer_length (x)
	int	count, i;
@
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
@)

void
init_num_log(void)
{
	make_constant("BOOLE-CLR", MAKE_FIXNUM(BOOLCLR));
	make_constant("BOOLE-SET", MAKE_FIXNUM(BOOLSET));
	make_constant("BOOLE-1", MAKE_FIXNUM(BOOL1));
	make_constant("BOOLE-2", MAKE_FIXNUM(BOOL2));
	make_constant("BOOLE-C1", MAKE_FIXNUM(BOOLC1));
	make_constant("BOOLE-C2", MAKE_FIXNUM(BOOLC2));
	make_constant("BOOLE-AND", MAKE_FIXNUM(BOOLAND));
	make_constant("BOOLE-IOR", MAKE_FIXNUM(BOOLIOR));
	make_constant("BOOLE-XOR", MAKE_FIXNUM(BOOLXOR));
	make_constant("BOOLE-EQV", MAKE_FIXNUM(BOOLEQV));
	make_constant("BOOLE-NAND", MAKE_FIXNUM(BOOLNAND));
	make_constant("BOOLE-NOR", MAKE_FIXNUM(BOOLNOR));
	make_constant("BOOLE-ANDC1", MAKE_FIXNUM(BOOLANDC1));
	make_constant("BOOLE-ANDC2", MAKE_FIXNUM(BOOLANDC2));
	make_constant("BOOLE-ORC1", MAKE_FIXNUM(BOOLORC1));
	make_constant("BOOLE-ORC2", MAKE_FIXNUM(BOOLORC2));
}

@(defun si::bit_array_op (o x y r)
	cl_fixnum i, j, n, d;
	cl_object r0;
	int (*op)();
	bool replace = FALSE;
	int xi, yi, ri;
	byte *xp, *yp, *rp;
	int xo, yo, ro;
@
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
			r = siLmake_vector(6, Sbit, MAKE_FIXNUM(d), Cnil, Cnil, Cnil, Cnil);
		}
	} else {
		if (type_of(x) != t_array)
			goto ERROR;
		if ((enum aelttype)x->array.elttype != aet_bit)
			goto ERROR;
		d = x->array.dim;
		xp = x->vector.self.bit;
		xo = x->vector.offset;
		if (type_of(y) != t_array)
			goto ERROR;
		if ((enum aelttype)y->array.elttype != aet_bit)
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
			if ((enum aelttype)r->array.elttype != aet_bit)
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
		  r = alloc_object(t_array);
		  r->array.self.t = NULL;
		  r->array.displaced = Cnil;
		  r->array.rank = 1;
		  r->array.dims = NULL;
		  r->array.elttype = get_aelttype(Sbit);
		  r->array.dims = alloc_atomic_align(sizeof(int), sizeof(int));
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
@)

