/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    symbol.d -- Symbols.
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
#include <ecl/ecl-inl.h>

/******************************* ------- ******************************/
/* FIXME! CURRENTLY SYMBOLS ARE RESTRICTED TO HAVE NON-UNICODE NAMES */

cl_object
ecl_symbol_package(cl_object s)
{
	do {
		if (Null(s))
			return Cnil_symbol->symbol.hpack;
		if (type_of(s) == t_symbol)
			return s->symbol.hpack;
		s = ecl_type_error(@'symbol-package', "symbol", s, @'symbol');
	} while(1);
}

int
ecl_symbol_type(cl_object s)
{
	do {
		if (Null(s))
			return Cnil_symbol->symbol.stype;
		if (type_of(s) == t_symbol)
			return s->symbol.stype;
		s = ecl_type_error(@'symbol-name', "symbol", s, @'symbol');
	} while(1);
}

void
ecl_symbol_type_set(cl_object s, int type)
{
	do {
		if (Null(s)) {
			Cnil_symbol->symbol.stype = type;
			return;
		}
		if (type_of(s) == t_symbol) {
			s->symbol.stype = type;
			return;
		}
		s = ecl_type_error(@'symbol-name', "symbol", s, @'symbol');
	} while(1);
}

cl_object
ecl_symbol_name(cl_object s)
{
	do {
		if (Null(s)) {
			return Cnil_symbol->symbol.name;
		}
		if (type_of(s) == t_symbol) {
			return s->symbol.name;
		}
		s = ecl_type_error(@'symbol-name', "symbol", s, @'symbol');
	} while(1);
}

static cl_object *
ecl_symbol_plist(cl_object s)
{
	do {
		if (Null(s)) {
			return &Cnil_symbol->symbol.plist;
		}
		if (type_of(s) == t_symbol) {
			return &s->symbol.plist;
		}
		s = ecl_type_error(@'symbol-plist', "symbol", s, @'symbol');
	} while(1);
}

/**********************************************************************/

static void FEtype_error_plist(cl_object x) /*__attribute__((noreturn))*/;

cl_object
cl_make_symbol(cl_object str)
{
	cl_object x;
 AGAIN:
	/* INV: In several places it is assumed that we copy the string! */
	switch (type_of(str)) {
#ifdef ECL_UNICODE
	case t_string:
		if (!ecl_fits_in_base_string(str)) {
			str = cl_copy_seq(str);
		} else {
			str = si_copy_to_simple_base_string(str);
		}
		break;
#endif
	case t_base_string:
		str = si_copy_to_simple_base_string(str);
		break;
	default:
		str = ecl_type_error(@'make-symbol',"name",str,@'string');
		goto AGAIN;
	}
	x = cl_alloc_object(t_symbol);
	x->symbol.name = str;
	x->symbol.dynamic = 0;
	ECL_SET(x,OBJNULL);
	SYM_FUN(x) = Cnil;
	x->symbol.plist = Cnil;
	x->symbol.hpack = Cnil;
	x->symbol.stype = stp_ordinary;
	@(return x)
}

/*
	ecl_make_keyword(s) makes a keyword from C string s.
*/
cl_object
ecl_make_keyword(const char *s)
{
	cl_object x = _ecl_intern(s, cl_core.keyword_package);
	/* cl_export(x, keyword_package); this is implicit in ecl_intern() */
	return x;
}

cl_object
ecl_symbol_value(cl_object s)
{
	if (Null(s)) {
		return s;
	} else {
		/* FIXME: Should we check symbol type? */
		cl_object value = SYM_VAL(s);
		if (value == OBJNULL)
			FEunbound_variable(s);
		return value;
	}
}

static void
FEtype_error_plist(cl_object x)
{
	cl_error(9, @'simple-type-error', @':format-control',
		 make_constant_base_string("Not a valid property list ~D"),
		 @':format-arguments', cl_list(1, x),
		 @':expected-type', @'si::property-list',
		 @':datum', x);
}

cl_object
ecl_getf(cl_object place, cl_object indicator, cl_object deflt)
{
	cl_object l;

#ifdef ECL_SAFE
	assert_type_proper_list(place);
#endif
	for (l = place; CONSP(l); ) {
		cl_object cdr_l = ECL_CONS_CDR(l);
		if (!CONSP(cdr_l))
			break;
		if (ECL_CONS_CAR(l) == indicator)
			return ECL_CONS_CAR(cdr_l);
		l = ECL_CONS_CDR(cdr_l);
	}
	if (l != Cnil)
		FEtype_error_plist(place);
	return(deflt);
}

cl_object
ecl_get(cl_object s, cl_object p, cl_object d)
{
	return ecl_getf(*ecl_symbol_plist(s), p, d);
}

/*
	(SI:PUT-F plist value indicator)
	returns the new property list with value for property indicator.
	It will be used in SETF for GETF.
*/
cl_object
si_put_f(cl_object place, cl_object value, cl_object indicator)
{
	cl_object l;

#ifdef ECL_SAFE
	assert_type_proper_list(place);
#endif
	/* This loop guarantees finishing for circular lists */
	for (l = place; CONSP(l); ) {
		cl_object cdr_l = ECL_CONS_CDR(l);
		if (!CONSP(cdr_l))
			break;
		if (ECL_CONS_CAR(l) == indicator) {
			ECL_RPLACA(cdr_l, value);
			@(return place);
		}
		l = ECL_CONS_CDR(cdr_l);
	}
	if (l != Cnil)
		FEtype_error_plist(place);
	place = CONS(value, place);
	@(return CONS(indicator, place));
}

/*
	Remf(p, i) removes property i
	from the property list pointed by p,
	which is a pointer to an cl_object.
	The returned value of remf(p, i) is:

		TRUE    if the property existed
		FALSE   otherwise.
*/
static bool
remf(cl_object *place, cl_object indicator)
{
	cl_object l = *place, tail = Cnil;
	while (!Null(l)) {
		cl_object ind;
		if (!LISTP(l))
			FEtype_error_plist(*place);
		ind = ECL_CONS_CAR(l);
		l = ECL_CONS_CDR(l);
		if (!CONSP(l))
			FEtype_error_plist(*place);
		if (ind == indicator) {
			l = ECL_CONS_CDR(l);
			if (Null(tail))
				*place = l;
			else
				ECL_RPLACD(tail, l);
			return TRUE;
		}
		tail = l;
		l = ECL_CONS_CDR(l);
	}
	return FALSE;
}

bool
ecl_keywordp(cl_object s)
{
	return (type_of(s) == t_symbol) && (s->symbol.hpack == cl_core.keyword_package);
}

@(defun get (sym indicator &optional deflt)
	cl_object *plist;
@
	plist = ecl_symbol_plist(sym);
	@(return ecl_getf(*plist, indicator, deflt))
@)

cl_object
cl_remprop(cl_object sym, cl_object prop)
{
	cl_object *plist = ecl_symbol_plist(sym);
	@(return (remf(plist, prop)? Ct: Cnil))
}

cl_object
cl_symbol_plist(cl_object sym)
{
	@(return *ecl_symbol_plist(sym))
}

@(defun getf (place indicator &optional deflt)
@
	@(return ecl_getf(place, indicator, deflt))
@)

cl_object
cl_get_properties(cl_object place, cl_object indicator_list)
{
	cl_object l;

#ifdef ECL_SAFE
	assert_type_proper_list(place);
#endif
	for (l = place;  CONSP(l); ) {
		cl_object cdr_l = ECL_CONS_CDR(l);
		if (!CONSP(cdr_l))
			break;
		if (ecl_member_eq(ECL_CONS_CAR(l), indicator_list))
			@(return ECL_CONS_CAR(l) ECL_CONS_CAR(cdr_l) l)
		l = ECL_CONS_CDR(cdr_l);
	}
	if (l != Cnil)
		FEtype_error_plist(place);
	@(return Cnil Cnil Cnil)
}

cl_object
cl_symbol_name(cl_object x)
{
	@(return ecl_symbol_name(x))
}

@(defun copy_symbol (sym &optional cp &aux x)
@
	if (Null(sym))
		sym = Cnil_symbol;
	x = cl_make_symbol(ecl_symbol_name(sym));
	if (!Null(cp)) {
		x->symbol.dynamic = 0;
		x->symbol.stype = sym->symbol.stype;
		x->symbol.value = sym->symbol.value;
		x->symbol.gfdef = sym->symbol.gfdef;
		x->symbol.plist = cl_copy_list(sym->symbol.plist);
		/* FIXME!!! We should also copy the system property list */
	}
	@(return x)
@)

@(defun gensym (&optional (prefix cl_core.gensym_prefix))
	cl_type t;
	cl_object counter, output;
	bool increment;
@ {
 AGAIN:
	if (ecl_stringp(prefix)) {
		counter = SYM_VAL(@'*gensym-counter*');
		increment = 1;
	} else if ((t = type_of(prefix)) == t_fixnum || t == t_bignum) {
		counter = prefix;
		prefix = cl_core.gensym_prefix;
		increment = 0;
	} else {
		prefix = ecl_type_error(@'gensym',"prefix",prefix,
					cl_list(3, @'or', @'string', @'integer'));
		goto AGAIN;
	}
	output = ecl_make_string_output_stream(64);
	bds_bind(@'*print-escape*', Cnil);
	bds_bind(@'*print-readably*', Cnil);
	bds_bind(@'*print-base*', MAKE_FIXNUM(10));
	bds_bind(@'*print-radix*', Cnil);
	si_write_ugly_object(prefix, output);
	si_write_ugly_object(counter, output);
	bds_unwind_n(4);
	output = cl_make_symbol(cl_get_output_stream_string(output));
	if (increment)
		ECL_SETQ(@'*gensym-counter*',ecl_one_plus(counter));
	@(return output);
} @)

@(defun gentemp (&optional (prefix cl_core.gentemp_prefix) (pack ecl_current_package()))
	cl_object output, s;
	int intern_flag;
@
	prefix = ecl_check_type_string(@'gentemp', prefix);
	pack = si_coerce_to_package(pack);
ONCE_MORE:
	output = ecl_make_string_output_stream(64);
	bds_bind(@'*print-escape*', Cnil);
	bds_bind(@'*print-readably*', Cnil);
	bds_bind(@'*print-base*', MAKE_FIXNUM(10));
	bds_bind(@'*print-radix*', Cnil);
	si_write_ugly_object(prefix, output);
	si_write_ugly_object(cl_core.gentemp_counter, output);
	bds_unwind_n(4);
	cl_core.gentemp_counter = ecl_one_plus(cl_core.gentemp_counter);
	s = ecl_intern(cl_get_output_stream_string(output), pack, &intern_flag);
	if (intern_flag != 0)
		goto ONCE_MORE;
	@(return s)
@)

cl_object
cl_symbol_package(cl_object sym)
{
	@(return ecl_symbol_package(sym))
}

cl_object
cl_keywordp(cl_object sym)
{
	@(return (ecl_keywordp(sym)? Ct: Cnil))
}

/*
	(SI:REM-F plist indicator) returns two values:

		* the new property list
		  in which property indcator is removed

		* T     if really removed
		  NIL   otherwise.

	It will be used for macro REMF.
*/
cl_object
si_rem_f(cl_object plist, cl_object indicator)
{
	bool found = remf(&plist, indicator);
	@(return plist (found? Ct : Cnil))
}

cl_object
si_set_symbol_plist(cl_object sym, cl_object plist)
{
	*ecl_symbol_plist(sym) = plist;
	@(return plist)
}

cl_object
si_putprop(cl_object sym, cl_object value, cl_object indicator)
{
	cl_object *plist = ecl_symbol_plist(sym);
	*plist = si_put_f(*plist, value, indicator);
	@(return value)
}

/* Added for defstruct. Beppe */
@(defun si::put_properties (sym &rest ind_values)
@
	while (--narg >= 2) {
		cl_object prop = cl_va_arg(ind_values);
		si_putprop(sym, cl_va_arg(ind_values), prop);
		narg--;
	}
	@(return sym)
@)

cl_object
@si::*make_special(cl_object sym)
{
	int type = ecl_symbol_type(sym);
	if (type & stp_constant)
		FEerror("~S is a constant.", 1, sym);
	ecl_symbol_type_set(sym, type | stp_special);
	cl_remprop(sym, @'si::symbol-macro');
	@(return sym)
}

cl_object
@si::*make_constant(cl_object sym, cl_object val)
{
	int type = ecl_symbol_type(sym);
	if (type & stp_special)
		FEerror("The argument ~S to DEFCONSTANT is a special variable.",
			1, sym);
	ecl_symbol_type_set(sym, type | stp_constant);
	ECL_SET(sym, val);
	@(return sym)
}
