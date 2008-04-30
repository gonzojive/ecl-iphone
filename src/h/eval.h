/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    eval.h -- Macros and parameters..
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#define check_symbol(x) if (!SYMBOLP(x)) FEtype_error_symbol(x);

#define CONS(a,d)	ecl_cons((a),(d))
#define ACONS(a,b,c)	ecl_cons(ecl_cons((a),(b)),(c))
#define CAR(x)		(Null(x)? (x) : ECL_CONS_CAR(x))
#define CDR(x)		(Null(x)? (x) : ECL_CONS_CDR(x))

#define CAAR(x)		CAR(CAR(x))
#define CADR(x)		CAR(CDR(x))
#define CAAAR(x)	CAR(CAAR(x))
#define CAADR(x)	CAR(CADR(x))
#define CADAR(x)	CAR(CDAR(x))
#define CADDR(x)	CAR(CDDR(x))
#define CAAAAR(x)	CAR(CAAAR(x))
#define CAAADR(x)	CAR(CAADR(x))
#define CAADAR(x)	CAR(CADAR(x))
#define CAADDR(x)	CAR(CADDR(x))
#define CADAAR(x)	CAR(CDAAR(x))
#define CADADR(x)	CAR(CDADR(x))
#define CADDAR(x)	CAR(CDDAR(x))
#define CADDDR(x)	CAR(CDDDR(x))

#define CDAR(x)		CDR(CAR(x))
#define CDDR(x)		CDR(CDR(x))
#define CDAAR(x)	CDR(CAAR(x))
#define CDADR(x)	CDR(CADR(x))
#define CDDAR(x)	CDR(CDAR(x))
#define CDDDR(x)	CDR(CDDR(x))
#define CDAAAR(x)	CDR(CAAAR(x))
#define CDAADR(x)	CDR(CAADR(x))
#define CDADAR(x)	CDR(CADAR(x))
#define CDADDR(x)	CDR(CADDR(x))
#define CDDAAR(x)	CDR(CDAAR(x))
#define CDDADR(x)	CDR(CDADR(x))
#define CDDDAR(x)	CDR(CDDAR(x))
#define CDDDDR(x)	CDR(CDDDR(x))

#define Null(x)	((x)==Cnil)
