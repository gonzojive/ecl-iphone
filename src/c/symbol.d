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

#include "ecl.h"

/******************************* ------- ******************************/

static void FEtype_error_plist(cl_object x) __attribute__((noreturn));

cl_object
cl_make_symbol(cl_object str)
{
	assert_type_string(str);
	@(return make_symbol(str))
}

cl_object
make_symbol(cl_object st)
{
	cl_object x;

	x = cl_alloc_object(t_symbol);
	/* FIXME! Should we copy? */
	x->symbol.name = st;
	x->symbol.dynamic = 0;
	ECL_SET(x,OBJNULL);
	SYM_FUN(x) = OBJNULL;
	x->symbol.plist = Cnil;
	x->symbol.hpack = Cnil;
	x->symbol.stype = stp_ordinary;
	x->symbol.mflag = FALSE;
	x->symbol.isform = FALSE;
	return(x);
}

/*
	cl_defvar(s, v) makes a special variable from a symbol and, if it was
	unbound, assignes it the value V.
*/
void
cl_defvar(cl_object s, cl_object v)
{
	assert_type_symbol(s);
	s->symbol.stype = (short)stp_special;
	if (SYM_VAL(s) == OBJNULL)
		ECL_SET(s, v);
}

/*
	cl_defparameter(s, v) makes a special variable from a symbol and,
	assignes it the value V.
*/
void
cl_defparameter(cl_object s, cl_object v)
{
	cl_defvar(s, v);
	ECL_SET(s, v);
}

/*
	Make_keyword(s) makes a keyword from C string s.
*/
cl_object
make_keyword(const char *s)
{
	cl_object x = _intern(s, cl_core.keyword_package);
	/* cl_export(x, keyword_package); this is implicit in intern() */
	return x;
}

cl_object
symbol_value(cl_object s)
{
	/* FIXME: Should we check symbol type? */
	cl_object value = SYM_VAL(s);
	if (value == OBJNULL)
		FEunbound_variable(s);
	return value;
}

static void
FEtype_error_plist(cl_object x)
{
	cl_error(9, @'simple-type-error', @':format-control',
		 make_simple_string("Not a valid property list ~D"),
		 @':format-arguments', cl_list(1, x),
		 @':expected-type', @'list',
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
		cl_object cdr_l = CDR(l);
		if (!CONSP(cdr_l))
			break;
		if (CAR(l) == indicator)
			return CAR(cdr_l);
		l = CDR(cdr_l);
	}
	if (l != Cnil)
		FEtype_error_plist(place);
	return(deflt);
}

cl_object
ecl_get(cl_object s, cl_object p, cl_object d)
{
	if (!SYMBOLP(s))
		FEtype_error_symbol(s);
	return ecl_getf(s->symbol.plist, p, d);
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
		cl_object cdr_l = CDR(l);
		if (!CONSP(cdr_l))
			break;
		if (CAR(l) == indicator) {
			CAR(cdr_l) = value;
			@(return place);
		}
		l = CDR(cdr_l);
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
	cl_object *l;

#ifdef ECL_SAFE
	assert_type_proper_list(*place);
#endif
	for (l = place; CONSP(*l); ) {
		cl_object cdr_l = CDR(*l);
		if (!CONSP(cdr_l))
			break;
		if (CAR(*l) == indicator) {
			*l = CDR(cdr_l);
			return TRUE;
		}
		l = &CDR(cdr_l);
	}
	if (*l != Cnil)
		FEtype_error_plist(*place);
	return(FALSE);
}

bool
keywordp(cl_object s)
{
	return (SYMBOLP(s) && s->symbol.hpack == cl_core.keyword_package);
}

@(defun get (sym indicator &optional deflt)
@
	assert_type_symbol(sym);
	@(return ecl_getf(sym->symbol.plist, indicator, deflt))
@)

cl_object
cl_remprop(cl_object sym, cl_object prop)
{
	assert_type_symbol(sym);
	@(return (remf(&sym->symbol.plist, prop)? Ct: Cnil))
}

cl_object
cl_symbol_plist(cl_object sym)
{
	assert_type_symbol(sym);
	@(return sym->symbol.plist)
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
		cl_object cdr_l = CDR(l);
		if (!CONSP(cdr_l))
			break;
		if (member_eq(CAR(l), indicator_list))
			@(return CAR(l) CADR(l) l)
		l = CDR(cdr_l);
	}
	if (l != Cnil)
		FEtype_error_plist(place);
	@(return Cnil Cnil Cnil)
}

cl_object
cl_symbol_name(cl_object x)
{
	assert_type_symbol(x);
	@(return x->symbol.name)
}

@(defun copy_symbol (sym &optional cp &aux x)
@
	assert_type_symbol(sym);
	x = make_symbol(sym->symbol.name);
	if (Null(cp))
		@(return x)
	x->symbol.stype = sym->symbol.stype;
	x->symbol.dynamic = 0;
	ECL_SET(x, SYM_VAL(sym));
	x->symbol.mflag = sym->symbol.mflag;
	SYM_FUN(x) = SYM_FUN(sym);
	x->symbol.plist = cl_copy_list(sym->symbol.plist);
	/* FIXME!!! We should also copy the system property list */
	@(return x)
@)

@(defun gensym (&optional (prefix cl_core.gensym_prefix))
	cl_type t;
	cl_object counter, output;
	bool increment;
@
	t = type_of(prefix);
	if (t == t_string) {
		counter = SYM_VAL(@'*gensym-counter*');
		increment = 1;
	} else if (t == t_fixnum || t == t_bignum) {
		counter = prefix;
		prefix = cl_core.gensym_prefix;
		increment = 0;
	} else {
		FEwrong_type_argument(cl_list(3, @'or', @'string', @'integer'),
				      prefix);
	}
	output = make_string_output_stream(64);
	bds_bind(@'*print-base*', MAKE_FIXNUM(10));
	bds_bind(@'*print-radix*', Cnil);
	princ(prefix, output);
	princ(counter, output);
	bds_unwind_n(2);
	output = make_symbol(get_output_stream_string(output));
	if (increment)
		ECL_SETQ(@'*gensym-counter*',one_plus(counter));
	@(return output)
@)

@(defun gentemp (&optional (prefix cl_core.gentemp_prefix) (pack current_package()))
	cl_object output, s;
	int intern_flag;
@
	assert_type_string(prefix);
	assert_type_package(pack);
	s = cl_alloc_adjustable_string(64);
ONCE_MORE:
	output = make_string_output_stream_from_string(s);
	bds_bind(@'*print-base*', MAKE_FIXNUM(10));
	bds_bind(@'*print-radix*', Cnil);
	princ(prefix, output);
	princ(cl_core.gentemp_counter, output);
	bds_unwind_n(2);
	cl_core.gentemp_counter = one_plus(cl_core.gentemp_counter);
	s = intern(get_output_stream_string(output), pack, &intern_flag);
	if (intern_flag != 0)
		goto ONCE_MORE;
	@(return s)
@)

cl_object
cl_symbol_package(cl_object sym)
{
	assert_type_symbol(sym);
	@(return sym->symbol.hpack)
}

cl_object
cl_keywordp(cl_object sym)
{
	@(return ((SYMBOLP(sym) && keywordp(sym))? Ct: Cnil))
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
	assert_type_symbol(sym);
	sym->symbol.plist = plist;
	@(return plist)
}

cl_object
si_putprop(cl_object sym, cl_object value, cl_object indicator)
{
	assert_type_symbol(sym);
	sym->symbol.plist = si_put_f(sym->symbol.plist, value, indicator);
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
	assert_type_symbol(sym);
	if ((enum ecl_stype)sym->symbol.stype == stp_constant)
		FEerror("~S is a constant.", 1, sym);
	sym->symbol.stype = (short)stp_special;
	cl_remprop(sym, @'si::symbol-macro');
	@(return sym)
}

cl_object
@si::*make_constant(cl_object sym, cl_object val)
{
	assert_type_symbol(sym);
	if ((enum ecl_stype)sym->symbol.stype == stp_special)
		FEerror(
		 "The argument ~S to DEFCONSTANT is a special variable.",
		 1, sym);
	sym->symbol.stype = (short)stp_constant;
	ECL_SET(sym, val);
	@(return sym)
}
