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
#define big_register0_get()	(bignum_register[0]->big.big_size = 0, bignum_register[0])
#define big_register1_get()	(bignum_register[1]->big.big_size = 0, bignum_register[1])
#define big_register2_get()	(bignum_register[2]->big.big_size = 0, bignum_register[2])
