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

/******************************* EXPORTS ******************************/

#ifndef THREADS
cl_object cl_token;
#endif

/******************************* ------- ******************************/

static cl_object gensym_prefix;
static cl_object gentemp_prefix;
static cl_index gentemp_counter;

@(defun make_symbol (str)
@
	assert_type_string(str);
	@(return make_symbol(str))
@)

cl_object
make_symbol(cl_object st)
{
	cl_object x;

	x = cl_alloc_object(t_symbol);
	SYM_VAL(x) = OBJNULL;
	/* FIXME! Should we copy? */
	x->symbol.name = st;
	SYM_FUN(x) = OBJNULL;
	x->symbol.plist = Cnil;
	x->symbol.hpack = Cnil;
	x->symbol.stype = stp_ordinary;
	x->symbol.mflag = FALSE;
	x->symbol.isform = FALSE;
	return(x);
}

/*
	Make_ordinary(s) makes an ordinary symbol from C string s
	and interns it in lisp package as an external symbol.
*/
cl_object
make_ordinary(const char *s)
{
	cl_object x = _intern(s, lisp_package);
	cl_export(x, lisp_package);
	return(x);
}

/*
	Make_special(s, v) makes a special variable from C string s
	with initial value v in lisp package.
*/
cl_object
make_special(const char *s, cl_object v)
{
	cl_object x = make_ordinary(s);
	x->symbol.stype = (short)stp_special;
	SYM_VAL(x) = v;
	return(x);
}

/*
	Make_constant(s, v) makes a constant from C string s
	with constant value v in lisp package.
*/
cl_object
make_constant(const char *s, cl_object v)
{
	cl_object x = make_ordinary(s);
	x->symbol.stype = (short)stp_constant;
	SYM_VAL(x) = v;
	return(x);
}

/*
	Make_si_ordinary(s) makes an ordinary symbol from C string s
	and interns it in system package as an external symbol.
*/
cl_object
make_si_ordinary(const char *s)
{
	cl_object x = _intern(s, system_package);
	cl_export(x, system_package);
	return(x);
}

/*
	Make_si_special(s, v) makes a special variable from C string s
	with initial value v in system package.
*/
cl_object
make_si_special(const char *s, cl_object v)
{
	cl_object x = make_si_ordinary(s);
	x->symbol.stype = (short)stp_special;
	SYM_VAL(x) = v;
	return(x);
}

/*
	Make_si_constant(s, v) makes a constant from C string s
	with constant value v in system package.
*/
cl_object
make_si_constant(const char *s, cl_object v)
{
	cl_object x = make_si_ordinary(s);
	x->symbol.stype = (short)stp_constant;
	SYM_VAL(x) = v;
	return(x);
}

/*
	Make_keyword(s) makes a keyword from C string s.
*/
cl_object
make_keyword(const char *s)
{
	cl_object x = _intern(s, keyword_package);
	/* cl_export(x, keyword_package); this is implicit in intern() */
	return x;
}

cl_object
symbol_value(cl_object s)
{
	/* FIXME: Should we check symbol type? */
	if (SYM_VAL(s) == OBJNULL)
		FEunbound_variable(s);
	return(SYM_VAL(s));
}

cl_object
getf(cl_object place, cl_object indicator, cl_object deflt)
{
	cl_object slow, l;

	/* This loop guarantees finishing for circular lists */
	slow = l = place;
	while (CONSP(l)) {
		cl_object cdr_l = CDR(l);
		if (!CONSP(cdr_l))
			FEtype_error_plist(place);
		if (CAR(l) == indicator)
			return CAR(cdr_l);
		l = CDR(cdr_l);
		slow = CDR(slow);
		if (l == slow)
			FEcircular_list(place);
	}
	if (l != Cnil)
		FEtype_error_plist(place);
	return(deflt);
}

cl_object
get(cl_object s, cl_object p, cl_object d)
{
	if (!SYMBOLP(s))
		FEtype_error_symbol(s);
	return(getf(s->symbol.plist, p, d));
}

/*
	Putf(p, v, i) puts value v for property i to property list p
	and returns the resulting property list.
*/
cl_object
putf(cl_object place, cl_object value, cl_object indicator)
{
	cl_object slow, l;

	/* This loop guarantees finishing for circular lists */
	slow = l = place;
	while (CONSP(l)) {
		cl_object cdr_l = CDR(l);
		if (!CONSP(cdr_l))
			FEtype_error_plist(place);
		if (CAR(l) == indicator) {
			CAR(cdr_l) = value;
			return place;
		}
		l = CDR(cdr_l);
		slow = CDR(slow);
		if (l == slow)
			FEcircular_list(place);
	}
	if (l != Cnil)
		FEtype_error_plist(place);
	place = CONS(value, place);
	return CONS(indicator, place);
}

cl_object
putprop(cl_object s, cl_object v, cl_object p)
{
	if (!SYMBOLP(s))
		FEtype_error_symbol(s);
	s->symbol.plist = putf(s->symbol.plist, v, p);
	return(v);
}

/*
	Remf(p, i) removes property i
	from the property list pointed by p,
	which is a pointer to an cl_object.
	The returned value of remf(p, i) is:

		TRUE    if the property existed
		FALSE   otherwise.
*/
bool
remf(cl_object *place, cl_object indicator)
{
	cl_object *slow, *l;

	/* This loop guarantees finishing for circular lists */
	slow = l = place;
	while (CONSP(*l)) {
		cl_object cdr_l = CDR(*l);
		if (!CONSP(cdr_l))
			FEtype_error_plist(*place);
		if (CAR(*l) == indicator) {
			*l = CDR(cdr_l);
			return TRUE;
		}
		l = &CDR(cdr_l);
		slow = &CDR(*slow);
		if (l == slow)
			FEcircular_list(*place);
	}
	if (*l != Cnil)
		FEtype_error_plist(*place);
	return(FALSE);
}

cl_object
remprop(cl_object s, cl_object p)
{
	if (!SYMBOLP(s))
		FEtype_error_symbol(s);
	if (remf(&s->symbol.plist, p))
		return(Ct);
	else
		return(Cnil);
}

bool
keywordp(cl_object s)
{
	return (SYMBOLP(s) && s->symbol.hpack == keyword_package);
}

@(defun get (sym indicator &optional deflt)
@
	assert_type_symbol(sym);
	@(return getf(sym->symbol.plist, indicator, deflt))
@)

@(defun remprop (sym prop)
@
	assert_type_symbol(sym);
	@(return (remf(&sym->symbol.plist, prop)? Ct: Cnil))
@)

@(defun symbol_plist (sym)
@
	assert_type_symbol(sym);
	@(return sym->symbol.plist)
@)

@(defun getf (place indicator &optional deflt)
@
	@(return getf(place, indicator, deflt))
@)

@(defun get_properties (place indicator_list)
	cl_object slow, cdr_l, l;
@
	/* This loop guarantees finishing for circular lists */
	for (slow = l = place;  CONSP(l); ) {
		cdr_l = CDR(l);
		if (!CONSP(cdr_l))
			FEtype_error_plist(place);
		if (member_eq(CAR(l), indicator_list))
			@(return CAR(l) CADR(l) l)
		l = CDR(cdr_l);
		slow = CDR(slow);
		if (l == slow)
			FEcircular_list(place);
	}
	if (l != Cnil)
		FEtype_error_plist(place);
	@(return Cnil Cnil Cnil)
@)

cl_object
symbol_name(cl_object x)
{
	assert_type_symbol(x);
	return x->symbol.name;
}

@(defun symbol_name (sym)
@
	@(return symbol_name(sym))
@)

@(defun copy_symbol (sym &optional cp &aux x)
@
	assert_type_symbol(sym);
	x = make_symbol(sym);
	if (Null(cp))
		@(return x)
	x->symbol.stype = sym->symbol.stype;
	SYM_VAL(x) = SYM_VAL(sym);
	x->symbol.mflag = sym->symbol.mflag;
	SYM_FUN(x) = SYM_FUN(sym);
	x->symbol.plist = copy_list(sym->symbol.plist);
	@(return x)
@)

@(defun gensym (&optional (prefix gensym_prefix) &aux str)
	cl_index name_length, j, counter_value;
	cl_object counter;
@
	if (type_of(prefix) == t_string) {
		counter = SYM_VAL(@'*gensym-counter*');
	} else {
		counter = prefix;
		prefix = gensym_prefix;
	}
	if (!FIXNUMP(counter) || FIXNUM_MINUSP(counter)) {
		FEerror("*gensym-counter*, ~A, not a positive fixnum",
			1, counter);
	}
	counter_value = fix(counter);
	name_length = prefix->string.fillp;
	for (j = counter_value;  j > 0;  j /= 10)
		name_length++;
	if (name_length == 0)
		name_length++;
	str = cl_alloc_simple_string(name_length);
	str->string.self = (char *)cl_alloc_atomic(name_length+1);
	str->string.self[name_length] = '\0';
	for (j = 0;  j < prefix->string.fillp;  j++)
		str->string.self[j] = prefix->string.self[j];
	if (counter_value == 0)
		str->string.self[--name_length] = '0';
	else
		for (j=counter_value;  j > 0;  j /= 10)
			str->string.self[--name_length] = j%10 + '0';
	if (prefix == gensym_prefix)
		SYM_VAL(@'*gensym-counter*') = MAKE_FIXNUM(counter_value+1);
	@(return make_symbol(str))
@)

@(defun gentemp (&optional (prefix gentemp_prefix) (pack current_package())
		 &aux str smbl)
	size_t name_length, j;
	int intern_flag;
@
	assert_type_string(prefix);
	assert_type_package(pack);
ONCE_MORE:
	name_length = prefix->string.fillp;
	for (j = gentemp_counter;  j > 0;  j /= 10)
		name_length++;
	if (name_length == 0)
		name_length++;
	str = cl_alloc_simple_string(name_length);
        str->string.self = (char *)cl_alloc_atomic(name_length+1);
	str->string.self[name_length] = '\0';
        for (j = 0;  j < prefix->string.fillp;  j++)
                str->string.self[j] = prefix->string.self[j];
	if ((j = gentemp_counter) == 0)
		str->string.self[--name_length] = '0';
	else
		for (;  j > 0;  j /= 10)
			str->string.self[--name_length] = j%10 + '0';
	gentemp_counter++;
	smbl = intern(str, pack, &intern_flag);
	if (intern_flag != 0)
		goto ONCE_MORE;
	@(return smbl)
@)

@(defun symbol_package (sym)
@
	assert_type_symbol(sym);
	@(return sym->symbol.hpack)
@)

@(defun keywordp (sym)
@
	@(return ((SYMBOLP(sym) && keywordp(sym))? Ct: Cnil))
@)

/*
	(SI:PUT-F plist value indicator)
	returns the new property list with value for property indicator.
	It will be used in SETF for GETF.
*/
@(defun si::put_f (plist value indicator)
@
	@(return putf(plist, value, indicator))
@)

/*
	(SI:REM-F plist indicator) returns two values:

		* the new property list
		  in which property indcator is removed

		* T     if really removed
		  NIL   otherwise.

	It will be used for macro REMF.
*/
@(defun si::rem_f (plist indicator)
	bool found;
@
	found = remf(&plist, indicator);
	@(return plist (found? Ct : Cnil))
@)

@(defun si::set_symbol_plist (sym plist)
@
	assert_type_symbol(sym);
	sym->symbol.plist = plist;
	@(return plist)
@)

@(defun si::putprop (sym value indicator)
@
	assert_type_symbol(sym);
	sym->symbol.plist = putf(sym->symbol.plist, value, indicator);
	@(return value)
@)

/* Added for defstruct. Beppe */
@(defun si::put_properties (sym &rest ind_values)
	cl_object prop;
@
	while (--narg >= 2) {
	  prop = va_arg(ind_values, cl_object);
	  putprop(sym, va_arg(ind_values, cl_object), prop);
	  narg--;
	}
	@(return sym)
@)

@(defun si::*make_special (sym)
@
	assert_type_symbol(sym);
	if ((enum stype)sym->symbol.stype == stp_constant)
		FEerror("~S is a constant.", 1, sym);
	sym->symbol.stype = (short)stp_special;
	remf(&sym->symbol.plist, @'si::symbol-macro');
	@(return sym)
@)

@(defun si::*make_constant (sym val)
@
	assert_type_symbol(sym);
	if ((enum stype)sym->symbol.stype == stp_special)
		FEerror(
		 "The argument ~S to DEFCONSTANT is a special variable.",
		 1, sym);
	sym->symbol.stype = (short)stp_constant;
	SYM_VAL(sym) = val;
	@(return sym)
@)

void
init_symbol(void)
{
	Cnil->symbol.t = (short)t_symbol;
	Cnil->symbol.dbind = Cnil;
	Cnil->symbol.name = make_simple_string("NIL");
	Cnil->symbol.gfdef = OBJNULL;
	Cnil->symbol.plist = Cnil;
	Cnil->symbol.hpack = Cnil;
	Cnil->symbol.stype = (short)stp_constant;
	Cnil->symbol.mflag = FALSE;
	Cnil->symbol.isform = FALSE;
	cl_num_symbols_in_core=1;

	Ct->symbol.t = (short)t_symbol;
	Ct->symbol.dbind = Ct;
	Ct->symbol.name = make_simple_string("T");
	Ct->symbol.gfdef = OBJNULL;
	Ct->symbol.plist = Cnil;
	Ct->symbol.hpack = Cnil;
	Ct->symbol.stype = (short)stp_constant;
	Ct->symbol.mflag = FALSE;
	Ct->symbol.isform = FALSE;
	cl_num_symbols_in_core=2;

	gensym_prefix = make_simple_string("G");
	gentemp_prefix = make_simple_string("T");
	gentemp_counter = 0;
	cl_token = cl_alloc_simple_string(LISP_PAGESIZE);
	cl_token->string.fillp = 0;
	cl_token->string.self = (char *)cl_alloc_atomic(LISP_PAGESIZE);
	cl_token->string.hasfillp = TRUE;
	cl_token->string.adjustable = TRUE;

	register_root(&gensym_prefix);
	register_root(&gentemp_prefix);
	register_root(&cl_token);
}

