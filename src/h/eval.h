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
#define CAR(x)		(x)->cons.car
#define CDR(x)		(x)->cons.cdr
#define CAAR(x)		(x)->cons.car->cons.car
#define CADR(x)		(x)->cons.cdr->cons.car
#define CDAR(x)		(x)->cons.car->cons.cdr
#define CDDR(x)		(x)->cons.cdr->cons.cdr
#define CAAAR(x)	(x)->cons.car->cons.car->cons.car
#define CAADR(x)	(x)->cons.cdr->cons.car->cons.car
#define CADAR(x)	(x)->cons.car->cons.cdr->cons.car
#define CADDR(x)	(x)->cons.cdr->cons.cdr->cons.car
#define CDAAR(x)	(x)->cons.car->cons.car->cons.cdr
#define CDADR(x)	(x)->cons.cdr->cons.car->cons.cdr
#define CDDAR(x)	(x)->cons.car->cons.cdr->cons.cdr
#define CDDDR(x)	(x)->cons.cdr->cons.cdr->cons.cdr
#define CAAAAR(x)	(x)->cons.car->cons.car->cons.car->cons.car
#define CAAADR(x)	(x)->cons.cdr->cons.car->cons.car->cons.car
#define CAADAR(x)	(x)->cons.car->cons.cdr->cons.car->cons.car
#define CAADDR(x)	(x)->cons.cdr->cons.cdr->cons.car->cons.car
#define CADAAR(x)	(x)->cons.car->cons.car->cons.cdr->cons.car
#define CADADR(x)	(x)->cons.cdr->cons.car->cons.cdr->cons.car
#define CADDAR(x)	(x)->cons.car->cons.cdr->cons.cdr->cons.car
#define CADDDR(x)	(x)->cons.cdr->cons.cdr->cons.cdr->cons.car
#define CDAAAR(x)	(x)->cons.car->cons.car->cons.car->cons.car
#define CDAADR(x)	(x)->cons.cdr->cons.car->cons.car->cons.cdr
#define CDADAR(x)	(x)->cons.car->cons.cdr->cons.car->cons.cdr
#define CDADDR(x)	(x)->cons.cdr->cons.cdr->cons.car->cons.cdr
#define CDDAAR(x)	(x)->cons.car->cons.car->cons.cdr->cons.cdr
#define CDDADR(x)	(x)->cons.cdr->cons.car->cons.cdr->cons.cdr
#define CDDDAR(x)	(x)->cons.car->cons.cdr->cons.cdr->cons.cdr
#define CDDDDR(x)	(x)->cons.cdr->cons.cdr->cons.cdr->cons.cdr

#define Null(x)	((x)==Cnil)
