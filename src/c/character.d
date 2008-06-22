/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    character.d -- Character routines.
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

#include <stdio.h>
#include <ecl/ecl.h>
#include <ctype.h>
#ifdef ECL_UNICODE
#include <wctype.h>
#endif

cl_fixnum
ecl_char_code(cl_object c)
{
	if (CHARACTERP(c))
		return CHAR_CODE(c);
	FEtype_error_character(c);
}

int
ecl_base_char_code(cl_object c)
{
#ifdef ECL_UNICODE
	if (CHARACTERP(c)) {
		cl_fixnum c = CHAR_CODE(c);
		if (c <= 255) {
			return (int)c;
		}
	}
	FEtype_error_character(c);
#else
	return ecl_char_code(c);
#endif
}

cl_object
cl_standard_char_p(cl_object c)
{
	/* INV: ecl_char_code() checks the type */
	cl_fixnum i = ecl_char_code(c);
	@(return (ecl_standard_char_p(i)? Ct : Cnil))
}

bool
ecl_standard_char_p(cl_index code)
{
	return ((' ' <= code) && (code < '\177')) || (code == '\n');
}

bool
ecl_base_char_p(cl_index c)
{
	return c <= 255;
}

cl_object
cl_graphic_char_p(cl_object c)
{
	/* INV: ecl_char_code() checks the type */
	@(return (ecl_graphic_char_p(ecl_char_code(c))? Ct : Cnil))
}

bool
ecl_graphic_char_p(cl_index code)
{
#ifdef ECL_UNICODE
	return code == ' ' || iswgraph(code);
#else
	return code == ' ' || isgraph(code);
#endif
}

cl_object
cl_alpha_char_p(cl_object c)
{
	/* INV: ecl_char_code() checks the type */
	@(return (ecl_alpha_char_p(ecl_char_code(c))? Ct : Cnil))
}

bool
ecl_alpha_char_p(cl_index code)
{
#ifdef ECL_UNICODE
	return iswalpha(code);
#else
	return isalpha(code);
#endif
}

cl_object
cl_upper_case_p(cl_object c)
{
	/* INV: ecl_char_code() checks the type */
	@(return (ecl_upper_case_p(ecl_char_code(c))? Ct : Cnil))
}

bool
ecl_upper_case_p(cl_index code)
{
#ifdef ECL_UNICODE
	return iswupper(code);
#else
	return isupper(code);
#endif
}

cl_object
cl_lower_case_p(cl_object c)
{
	/* INV: ecl_char_code() checks the type */
	@(return (ecl_lower_case_p(ecl_char_code(c))? Ct : Cnil))
}

bool
ecl_lower_case_p(cl_index code)
{
#ifdef ECL_UNICODE
	return iswlower(code);
#else
	return islower(code);
#endif
}

cl_object
cl_both_case_p(cl_object c)
{
	/* INV: ecl_char_code() checks the type */
	@(return (ecl_both_case_p(ecl_char_code(c))? Ct : Cnil))
}

bool
ecl_both_case_p(cl_index code)
{
#ifdef ECL_UNICODE
	return iswlower(code) || iswupper(code);
#else
	return islower(code) || isupper(code);
#endif
}

int
ecl_string_case(cl_object s)
{
	int upcase;
	cl_index i;
	const char *text;
	for (i = 0, upcase = 0, text = s->base_string.self; i <= s->base_string.dim; i++) {
		if (isupper(text[i])) {
			if (upcase < 0)
				return 0;
			upcase = +1;
		} else if (islower(text[i])) {
			if (upcase > 0)
				return 0;
			upcase = -1;
		}
	}
	return upcase;
}

@(defun digit_char_p (c &optional (radix MAKE_FIXNUM(10)))
@ {
	cl_fixnum basis = ecl_fixnum_in_range(@'digit-char-p',"radix", radix, 2, 36);
	cl_fixnum value = ecl_digitp(ecl_char_code(c), basis);
	@(return ((value < 0)? Cnil: MAKE_FIXNUM(value)));
} @)

/*
	Ecl_Digitp(i, r) returns the weight of code i
	as a digit of radix r, which must be 1 < r <= 36.
	If i is not a digit, -1 is returned.
*/
int
ecl_digitp(int i, int r)
{
	if (('0' <= i) && (i <= '9') && (i < '0' + r))
		return(i - '0');
	if (('A' <= i) && (10 < r) && (i < 'A' + (r - 10)))
		return(i - 'A' + 10);
	if (('a' <= i) && (10 < r) && (i < 'a' + (r - 10)))
		return(i - 'a' + 10);
	return(-1);
}

cl_object
cl_alphanumericp(cl_object c)
{
	/* INV: ecl_char_code() checks type of `c' */
	cl_fixnum i = ecl_char_code(c);
	@(return (ecl_alphanumericp(i)? Ct : Cnil))
}

bool
ecl_alphanumericp(cl_index i)
{
#ifdef ECL_UNICODE
	return iswalnum(i);
#else
	return isalnum(i);
#endif
}

@(defun char= (c &rest cs)
@
	/* INV: ecl_char_eq() checks types of `c' and `cs' */
	while (--narg)
		if (!ecl_char_eq(c, cl_va_arg(cs)))
			@(return Cnil)
	@(return Ct)
@)

bool
ecl_char_eq(cl_object x, cl_object y)
{
	return ecl_char_code(x) == ecl_char_code(y);
}

@(defun char/= (&rest cs)
	int i, j;
	cl_object c;
@
	/* INV: ecl_char_eq() checks types of its arguments */
	if (narg == 0)
		FEwrong_num_arguments(@'char/=');
	c = cl_va_arg(cs);
	for (i = 2; i<=narg; i++) {
		cl_va_list ds;
		cl_va_start(ds, narg, narg, 0);
		c = cl_va_arg(cs);
		for (j = 1; j<i; j++)
			if (ecl_char_eq(cl_va_arg(ds), c))
				@(return Cnil)
	}
	@(return Ct)
@)

static cl_object
Lchar_cmp(cl_narg narg, int s, int t, cl_va_list args)
{
	cl_object c, d;

	if (narg == 0)
		FEwrong_num_arguments_anonym();
	c = cl_va_arg(args);
	for (; --narg; c = d) {
		d = cl_va_arg(args);
		if (s*ecl_char_cmp(d, c) < t)
			@(return Cnil)
	}
	@(return Ct)
}

int
ecl_char_cmp(cl_object x, cl_object y)
{
	/* ecl_char_code(x) returns an integer which is well in the range
	 * of positive fixnums. Therefore, this subtraction never
	 * oveflows. */
	return ecl_char_code(x) - ecl_char_code(y);
}

@(defun char< (&rest args)
@
	return Lchar_cmp(narg, 1, 1, args);
@)

@(defun char> (&rest args)
@
	return Lchar_cmp(narg,-1, 1, args);
@)

@(defun char<= (&rest args)
@
	return Lchar_cmp(narg, 1, 0, args);
@)

@(defun char>= (&rest args)
@
	return Lchar_cmp(narg,-1, 0, args);
@)

@(defun char_equal (c &rest cs)
	int i;
@
	/* INV: ecl_char_equal() checks the type of its arguments */
	for (narg--, i = 0;  i < narg;  i++) {
		if (!ecl_char_equal(c, cl_va_arg(cs)))
			@(return Cnil)
	}
	@(return Ct)
@)

bool
ecl_char_equal(cl_object x, cl_object y)
{
	cl_fixnum i = ecl_char_code(x);
	cl_fixnum j = ecl_char_code(y);

	if (islower(i))
		i = toupper(i);
	if (islower(j))
		j = toupper(j);
	return(i == j);
}

@(defun char-not-equal (&rest cs)
	int i, j;
	cl_object c;
@
	/* INV: ecl_char_equal() checks the type of its arguments */
	if (narg == 0)
		FEwrong_num_arguments(@'char-not-equal');
	c = cl_va_arg(cs);
	for (i = 2;  i<=narg;  i++) {
		cl_va_list ds;
		cl_va_start(ds, narg, narg, 0);
		c = cl_va_arg(cs);
		for (j=1;  j<i;  j++)
			if (ecl_char_equal(c, cl_va_arg(ds)))
				@(return Cnil)
	}
	@(return Ct)
@)

static cl_object
Lchar_compare(cl_narg narg, int s, int t, cl_va_list args)
{
	cl_object c, d;

	/* INV: ecl_char_compare() checks the types of its arguments */
	if (narg == 0)
		FEwrong_num_arguments_anonym();
	c = cl_va_arg(args);
	for (; --narg; c = d) {
		d = cl_va_arg(args);
		if (s*ecl_char_compare(d, c) < t)
			@(return Cnil)
	}
	@(return Ct)
}

int
ecl_char_compare(cl_object x, cl_object y)
{
	cl_fixnum i = ecl_char_code(x);
	cl_fixnum j = ecl_char_code(y);

	if (islower(i))
		i = toupper(i);
	if (islower(j))
		j = toupper(j);
	if (i < j)
		return(-1);
	else if (i == j)
		return(0);
	else
		return(1);
}

@(defun char-lessp (&rest args)
@
	return Lchar_compare(narg, 1, 1, args);
@)

@(defun char-greaterp (&rest args)
@
	return Lchar_compare(narg,-1, 1, args);
@)

@(defun char-not-greaterp (&rest args)
@
	return Lchar_compare(narg, 1, 0, args);
@)

@(defun char-not-lessp (&rest args)
@
	return Lchar_compare(narg,-1, 0, args);
@)


cl_object
cl_character(cl_object x)
{
 AGAIN:
	switch (type_of(x)) {
	case t_character:
		break;
	case t_symbol:
		x = x->symbol.name;
		goto AGAIN;
#ifdef ECL_UNICODE
	case t_string:
		if (x->string.fillp == 1) {
			x = x->string.self[0];
			break;
		}
		goto ERROR;
#endif
	case t_base_string:
		if (x->base_string.fillp == 1) {
			x = CODE_CHAR(x->base_string.self[0]);
			break;
		}
	default: ERROR:
		x = ecl_type_error(@'character',"character designator",
				   x,c_string_to_object("(OR CHARACTER SYMBOL (ARRAY CHARACTER (1)) (ARRAY BASE-CHAR (1)))"));
		goto AGAIN;
	}
	@(return x)
}

cl_object
cl_char_code(cl_object c)
{
	/* INV: ecl_char_code() checks the type of `c' */
	@(return MAKE_FIXNUM(ecl_char_code(c)))
}

cl_object
cl_code_char(cl_object c)
{
	cl_fixnum fc;

	switch (type_of(c)) {
	case t_fixnum:
		fc = fix(c);
		if (fc < CHAR_CODE_LIMIT && fc >= 0) {
			c = CODE_CHAR(fc);
			break;
		}
	case t_bignum:
		c = Cnil;
		break;
	default:
		FEtype_error_integer(c);
	}
	@(return c)
}

cl_object
cl_char_upcase(cl_object c)
{
	/* INV: ecl_char_code() checks the type of `c' */
	cl_fixnum code = ecl_char_code(c);
	@(return CODE_CHAR(ecl_char_upcase(code)))
}

cl_index
ecl_char_upcase(cl_index code)
{
#ifdef ECL_UNICODE
	return iswlower(code)? towupper(code) : code;
#else
	return islower(code)? toupper(code) : code;
#endif
}

cl_object
cl_char_downcase(cl_object c)
{
	/* INV: ecl_char_code() checks the type of `c' */
	cl_fixnum code = ecl_char_code(c);
	@(return CODE_CHAR(ecl_char_downcase(code)))
}

cl_index
ecl_char_downcase(cl_index code)
{
#ifdef ECL_UNICODE
	return iswupper(code)? towlower(code) : code;
#else
	return isupper(code)? tolower(code) : code;
#endif
}

@(defun digit_char (weight &optional (radix MAKE_FIXNUM(10)))
@ {
	cl_fixnum basis = ecl_fixnum_in_range(@'digit-char',"radix",radix,2,36);
	cl_object output = Cnil;
  AGAIN:
	switch (type_of(weight)) {
	case t_fixnum: {
		cl_fixnum value = fix(weight);
		if (value >= 0) {
			int dw = ecl_digit_char(value, basis);
			if (dw >= 0) {
				output = CODE_CHAR(dw);
			}
		}
		break;
	}
	case t_bignum:
		break;
	default:
		weight = ecl_type_error(@'digit-char',"weight",weight,@'integer');
		goto AGAIN;
	}
	@(return output)
} @)

short
ecl_digit_char(cl_fixnum w, cl_fixnum r)
{
	if (r < 2 || r > 36 || w < 0 || w >= r)
		return(-1);
	if (w < 10)
		return(w + '0');
	else
		return(w - 10 + 'A');
}

cl_object
cl_char_int(cl_object c)
{
	/* INV: ecl_char_code() checks the type of `c' */
	return1(MAKE_FIXNUM(ecl_char_code(c)));
}

/* here we give every character an implicit name of the form 'u#' where # is a hexadecimal number,
   corresponding to a unicode code point.
   #\u14ea should work, for example
*/

cl_object
cl_char_name(cl_object c)
{
	cl_index code = ecl_char_code(c);
	cl_object output;
	if (code > 127) {
		char name[20]; /* cleanup */
		sprintf(name, "U%04x", code);
		output = make_base_string_copy(name);
	} else {
		output = ecl_gethash_safe(c, cl_core.char_names, Cnil);
	}
	@(return output);
}

cl_object
cl_name_char(cl_object name)
{
	cl_object c;
	cl_index l;
	name = cl_string(name);
	c = ecl_gethash_safe(name, cl_core.char_names, Cnil);
	if (c == Cnil && ecl_stringp(name) && (l = ecl_length(name))) {
		c = cl_char(name, MAKE_FIXNUM(0));
		if (l == 1) {
			(void)0;
		} else if (c != CODE_CHAR('u') && c != CODE_CHAR('U')) {
			c = Cnil;
		} else {
			cl_index used_l;
			if (type_of(name) == t_base_string) {
				cl_index end = name->base_string.fillp;
				cl_index real_end = end;
				c = ecl_parse_integer(name, 1, end, &real_end, 16);
				used_l = real_end;
			} else {
				/* Unsafe code: what about read errors?
				bds_bind(@'*read-base*', MAKE_FIXNUM(16));
				c = cl_funcall(6, @'read-from-string', name,
					       Cnil, Cnil, @':start', MAKE_FIXNUM(1));
				bds_unwind1();
				used_l = fix(VALUES(0));
				*/
				c = Cnil;
			}
			if (!FIXNUMP(c) || (used_l == (l - 1))) {
				c = Cnil;
			} else {
				c = CODE_CHAR(fix(c));
			}
		}
	}
	@(return c);
}
