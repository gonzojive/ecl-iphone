/*
    big.c -- Bignum routines.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECLS is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include "ecls.h"

#ifndef THREADS
cl_object bignum_register[3];
mp_limb_t bignum_register_limbs[3][BIGNUM_REGISTER_SIZE];
#endif THREADS

/* 
 * Using GMP multiple precision integers:
 *
 * typedef struct
 * {
 *  long int alloc;		// Number of limbs allocated and pointed
 *				//   to by the D field.
 *  long int size;		// abs(SIZE) is the number of limbs
 *				//   the last field points to.  If SIZE
 *				//   is negative this is a negative number.
 *  unsigned long int *d;	// Pointer to the limbs,
 *				//   d[0] is the least significative.
 * } MP_INT;
 *
 * typedef unsigned long int	mp_limb_t;
 *
 */

void
big_register_free(cl_object x)
{
	/* FIXME! Is this thread safe? */
	if (x == bignum_register[0])
	  x->big.big_limbs = bignum_register_limbs[0];
	else if (x == bignum_register[1])
	  x->big.big_limbs = bignum_register_limbs[1];
	else if (x == bignum_register[2])
	  x->big.big_limbs = bignum_register_limbs[2];
	else
	  error("big_register_free: unknown register");
	x->big.big_size = 0;
	x->big.big_dim = BIGNUM_REGISTER_SIZE;
}

cl_object
big_register_copy(cl_object old)
{
	cl_object new = alloc_object(t_bignum);
	if (old->big.big_dim > BIGNUM_REGISTER_SIZE) {
	  /* The object already has suffered a mpz_realloc() so
	     we can use the pointer */
	  new->big = old->big;
	  big_register_free(old);
	} else {
	  /* As the bignum points to the bignum_register_limbs[] area
	     we must duplicate its contents. */
	  mpz_init_set(new->big.big_num,old->big.big_num);
	}
	return new;
}

cl_object
big_register_normalize(cl_object x)
{
	int s = x->big.big_size;
	mp_limb_t y;
	if (s == 0)
	  return(MAKE_FIXNUM(0));
	y = x->big.big_limbs[0];
	if (s == 1) {
	  if (y <= MOST_POSITIVE_FIX)
	    return(MAKE_FIXNUM(y));
	} else if (s == -1) {
	  if (y <= -MOST_NEGATIVE_FIX)
	    return(MAKE_FIXNUM(-y));
	}
	return big_register_copy(x);
}

/*
 * Different from mpz_init since we initialize with NULL limbs
 */

cl_object
big_alloc(int size)
{
  volatile cl_object x = alloc_object(t_bignum);
  if (size <= 0)
    error("negative or zero size for bignum in big_alloc");
  x->big.big_dim = size;
  x->big.big_size = 0;
  x->big.big_limbs = alloc_atomic_align(size * sizeof(mp_limb_t), sizeof(mp_limb_t));
  return(x);
}

cl_object
bignum1(int val)
{
  volatile cl_object z = alloc_object(t_bignum);
  mpz_init_set_si(z->big.big_num, val);
  return(z);
}

cl_object
bignum2(mp_limb_t hi, mp_limb_t lo)
{
  cl_object z;

  z = big_alloc(2);
  z->big.big_size = 2;
  z->big.big_limbs[0] = lo;
  z->big.big_limbs[1] = hi;
  return(z);
}

cl_object
big_copy(cl_object x)
{
	volatile cl_object y = alloc_object(t_bignum);
	mpz_init_set(y->big.big_num, x->big.big_num);
	return(y);
}

/*
	big_zerop(x) tells whether bignum x is zero or not.

#define big_zerop(x)	(mp_size(x->big.big_num) == 0)
*/

/*
	big_sign(x) returns
		something < 0	if x < 0
		0		if x = 0
		something > 0	if x > 0.

#define big_sign(x)	(x->big.big_size)
*/

/*
	big_compare(x, y) returns
		-1	if x < y
		0	if x = y
		1	if x > y.

#define big_compare(x, y)	mpz_cmp(x->big.big_num, y->big.big_num)
*/

/*
	big_complement(x) destructively takes
	the complement of bignum x.

#define big_complement(x)	mpz_neg(x->big.big_num, x->big.num);
*/

/*
	big_minus(x) returns the complement of bignum x.
*/
cl_object
big_minus(cl_object x)
{
	volatile cl_object y = big_copy(x);
	mpz_neg(y->big.big_num, y->big.big_num);
	return y;
}

/*
	big_add_ui(x, i) destructively adds non-negative int i
	to bignum x.
	I should be non-negative.

	mpz_add_ui(x->big.big_num, x->big.big_num, i)
*/

/*
	big_sub_ui(x, i) destructively subtracts non-negative int i
	from bignum x.
	I should be non-negative.

	mpz_sub_ui(x->big.big_num, x->big.big_num, i)
*/

/*
	big_mul_ui(x, i) destructively multiplies non-negative bignum x
	by non-negative int i.
	I should be non-negative.
	X should be non-negative.

	mpn_mul(&x->big.big_limbs, &x->big.big_limbs, x->big.big_size, &i, 1)
*/

/*
	big_div_ui(x, i) destructively divides non-negative bignum x
	by positive int i.
	X will hold the remainder of the division.
	div_int_big(i, x) returns the remainder of the division.
	I should be positive.
	X should be non-negative.

	mp_limb_t q[x->big.big_size];
	mpn_div(q, &x->big.big_limbs, &x->big.big_size, &i, 1), x
*/

/*
	big_plus(x, y) returns the sum of bignum x and bignum y.
	X and y may be any bignum.
*/
cl_object
big_plus(cl_object x, cl_object y)
{
	volatile cl_object z = big_register0_get();
	mpz_add(z->big.big_num, x->big.big_num, y->big.big_num);
	return(big_register_copy(z));
}

cl_object
big_normalize(cl_object x)
{
  int s = x->big.big_size;
  mp_limb_t y;
  if (s == 0)
    return(MAKE_FIXNUM(0));
  y = x->big.big_limbs[0];
  if (s == 1 && y <= MOST_POSITIVE_FIX)
    return(MAKE_FIXNUM(y));
  if (s == -1 && y <= -MOST_NEGATIVE_FIX)
    return(MAKE_FIXNUM(-y));
  return(x);
}

static void *
mp_alloc(size_t size)
{
	return alloc_atomic_align(size, sizeof(mp_limb_t));
}

static void *
mp_realloc(void *ptr, size_t osize, size_t nsize)
{
	void *p = alloc_atomic_align(nsize, sizeof(mp_limb_t));
	memcpy(p, ptr, osize);
	return p;
}

static void
mp_free(void *ptr, size_t size)
{
  /*dealloc(ptr,size);*/
}

void
init_big(void)
{
	int i;
	for (i = 0; i < 3; i++) {
	  bignum_register[i] = alloc_object(t_bignum);
	  register_root(&bignum_register[i]);
	  big_register_free(bignum_register[i]);
	}
	mp_set_memory_functions(mp_alloc, mp_realloc, mp_free);
}
