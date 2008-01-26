/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    number.h  -- GMP interface.
*/
/*
    Copyright (c) 1995, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#ifdef WITH_GMP
#define big_odd_p(x)		((mpz_get_ui(x->big.big_num) & 1) != 0)
#define big_even_p(x)		((mpz_get_ui(x->big.big_num) & 1) == 0)
#define big_zerop(x)		((x)->big.big_size == 0)
#define big_sign(x)		((x)->big.big_size)
#define big_compare(x, y)	mpz_cmp(x->big.big_num, y->big.big_num)
#define big_complement(x)	mpz_neg(x->big.big_num, x->big.big_num)
#define big_add_ui(x, i)	mpz_add_ui(x->big.big_num, x->big.big_num, i)
#define	big_mul_ui(x, i) 	mpz_mul_ui(x->big.big_num, x->big.big_num, i)
#define big_set_ui(x, i)	mpz_set_ui(x->big.big_num, (unsigned long int)i)
#define big_set_si(x, i)	mpz_set_ui(x->big.big_num, (long int)i)
#define big_to_double(x)	mpz_get_d(x->big.big_num)
#define big_to_long(x)		mpz_get_si(x->big.big_num)
#define big_to_ulong(x)		mpz_get_ui(x->big.big_num)
#else  /* WITH_GMP */
extern int big_num_t_sgn(big_num_t x);
#define big_odd_p(x)		((int)((x)->big.big_num&1) != 0)
#define big_even_p(x)		((int)((x)->big.big_num&1) == 0)
#define big_zerop(x)		((x)->big.big_num == (big_num_t)0)
#define big_sign(x)		big_num_t_sgn((x)->big.big_num)
#define big_compare(x,y)	big_num_t_sgn((x)->big.big_num - (y)->big.big_num)
#define big_complement(x)	((x)->big.big_num = -((x)->big.big_num))
#define big_add_ui(x, i)	((x)->big.big_num += (unsigned long)(i))
#define big_mul_ui(x, i)	((x)->big.big_num *= (unsigned long)(i))
#define big_set_ui(x, i)	((x)->big.big_num = ((big_num_t)((unsigned long int)i)))
#define big_set_si(x, i)	((x)->big.big_num = ((big_num_t)((long int)i)))
#define big_to_double(x)	((double)((x)->big.big_num))
#define big_to_long(x)		((long int)((x)->big.big_num))
#define big_to_ulong(x)		((unsigned long int)((x)->big.big_num))
#endif /* WITH_GMP */
