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


#include "ecl.h"
#include <ctype.h>

cl_fixnum
char_code(cl_object c)
{
	if (CHARACTERP(c))
		return CHAR_CODE(c);
	FEtype_error_character(c);
}

cl_object
cl_standard_char_p(cl_object c)
{
	/* INV: char_code() checks the type */
	cl_fixnum i = char_code(c);
	if ((' ' <= i && i < '\177') || i == '\n')
		return1(Ct);
	return1(Cnil);
}

cl_object
cl_graphic_char_p(cl_object c)
{
	/* INV: char_code() checks the type */
	cl_fixnum i = char_code(c);
	if (' ' <= i && i < '\177')     /* ' ' < '\177'  ??? Beppe*/
		return1(Ct);
	return1(Cnil);
}

cl_object
cl_alpha_char_p(cl_object c)
{
	/* INV: char_code() checks the type */
	cl_fixnum i = char_code(c);
	if (isalpha(i))
		return1(Ct);
	else
		return1(Cnil);
}

cl_object
cl_upper_case_p(cl_object c)
{
	/* INV: char_code() checks the type */
	if (isupper(char_code(c)))
		return1(Ct);
	return1(Cnil);
}

cl_object
cl_lower_case_p(cl_object c)
{
	/* INV: char_code() checks the type */
	if (islower(char_code(c)))
		return1(Ct);
	return1(Cnil);
}

cl_object
cl_both_case_p(cl_object c)
{
	/* INV: char_code() checks the type */
	cl_fixnum code = char_code(c);
	return1((isupper(code) || islower(code)) ? Ct : Cnil);
}

#define basep(d)	(d <= 36)

@(defun digit_char_p (c &optional (r MAKE_FIXNUM(10)))
	cl_fixnum d;
@
	/* INV: char_code() checks `c' and fixnnint() checks `r' */
	if (type_of(r) == t_bignum)
		@(return Cnil)
	d = fixnnint(r);
	if (!basep(d) || (d = digitp(char_code(c), d)) < 0)
		@(return Cnil)
	@(return MAKE_FIXNUM(d))
@)

/*
	Digitp(i, r) returns the weight of code i
	as a digit of radix r, which must be 1 < r <= 36.
	If i is not a digit, -1 is returned.
*/
int
digitp(int i, int r)
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
	/* INV: char_code() checks type of `c' */
	cl_fixnum i = char_code(c);
	return1(isalnum(i)? Ct : Cnil);
}

@(defun char= (c &rest cs)
@
	/* INV: char_eq() checks types of `c' and `cs' */
	while (--narg)
		if (!char_eq(c, cl_va_arg(cs)))
			@(return Cnil)
	@(return Ct)
@)

bool
char_eq(cl_object x, cl_object y)
{
	return char_code(x) == char_code(y);
}

@(defun char/= (&rest cs)
	int i, j;
	cl_object c;
@
	/* INV: char_eq() checks types of its arguments */
	if (narg == 0)
		FEwrong_num_arguments(@'char/=');
	c = cl_va_arg(cs);
	for (i = 2; i<=narg; i++) {
		cl_va_list ds;
		cl_va_start(ds, narg, narg, 0);
		c = cl_va_arg(cs);
		for (j = 1; j<i; j++)
			if (char_eq(cl_va_arg(ds), c))
				@(return Cnil)
	}
	@(return Ct)
@)

static cl_return
Lchar_cmp(int narg, int s, int t, cl_va_list args)
{
	cl_object c, d;

	if (narg == 0)
		FEwrong_num_arguments_anonym();
	c = cl_va_arg(args);
	for (; --narg; c = d) {
		d = cl_va_arg(args);
		if (s*char_cmp(d, c) < t)
			return1(Cnil);
	}
	return1(Ct);
}

int
char_cmp(cl_object x, cl_object y)
{
	/* char_code(x) returns an integer which is well in the range
	 * of positive fixnums. Therefore, this subtraction never
	 * oveflows. */
	return char_code(x) - char_code(y);
}

@(defun char< (&rest args)
@
	@(return Lchar_cmp(narg, 1, 1, args))
@)

@(defun char> (&rest args)
@
	@(return Lchar_cmp(narg,-1, 1, args))
@)

@(defun char<= (&rest args)
@
	@(return Lchar_cmp(narg, 1, 0, args))
@)

@(defun char>= (&rest args)
@
	@(return Lchar_cmp(narg,-1, 0, args))
@)

@(defun char_equal (c &rest cs)
	int i;
@
	/* INV: char_equal() checks the type of its arguments */
	for (narg--, i = 0;  i < narg;  i++) {
		if (!char_equal(c, cl_va_arg(cs)))
			@(return Cnil)
	}
	@(return Ct)
@)

bool
char_equal(cl_object x, cl_object y)
{
	cl_fixnum i = char_code(x);
	cl_fixnum j = char_code(y);

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
	/* INV: char_equal() checks the type of its arguments */
	if (narg == 0)
		FEwrong_num_arguments(@'char-not-equal');
	c = cl_va_arg(cs);
	for (i = 2;  i<=narg;  i++) {
		cl_va_list ds;
		cl_va_start(ds, narg, narg, 0);
		c = cl_va_arg(cs);
		for (j=1;  j<i;  j++)
			if (char_equal(c, cl_va_arg(ds)))
				@(return Cnil)
	}
	@(return Ct)
@)

static cl_return
Lchar_compare(int narg, int s, int t, cl_va_list args)
{
	cl_object c, d;

	/* INV: char_compare() checks the types of its arguments */
	if (narg == 0)
		FEwrong_num_arguments_anonym();
	c = cl_va_arg(args);
	for (; --narg; c = d) {
		d = cl_va_arg(args);
		if (s*char_compare(d, c) < t)
			return1(Cnil);
	}
	return1(Ct);
}

int
char_compare(cl_object x, cl_object y)
{
	cl_fixnum i = char_code(x);
	cl_fixnum j = char_code(y);

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
	@(return Lchar_compare(narg, 1, 1, args))
@)

@(defun char-greaterp (&rest args)
@
	@(return Lchar_compare(narg,-1, 1, args))
@)

@(defun char-not-greaterp (&rest args)
@
	@(return Lchar_compare(narg, 1, 0, args))
@)

@(defun char-not-lessp (&rest args)
@
	@(return Lchar_compare(narg,-1, 0, args))
@)


cl_object
cl_character(cl_object x)
{
	switch (type_of(x)) {
	case t_character:
		break;
	case t_symbol:
		x = x->symbol.name;
	case t_string:
		if (x->string.fillp == 1)
			x = CODE_CHAR(x->string.self[0]);
		break;
	default:
		FEtype_error_character(x);
	}
	@(return x)
}

cl_object
cl_char_code(cl_object c)
{
	/* INV: char_code() checks the type of `c' */
	@(return MAKE_FIXNUM(char_code(c)))
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
	/* INV: char_code() checks the type of `c' */
	cl_fixnum code = char_code(c);
	return1(islower(code) ?
		CODE_CHAR(toupper(code)) :
		c);
}

cl_object
cl_char_downcase(cl_object c)
{
	/* INV: char_code() checks the type of `c' */
	cl_fixnum code = char_code(c);
	return1(isupper(code) ?
		CODE_CHAR(tolower(code)) :
		c);
}

@(defun digit_char (w &optional (r MAKE_FIXNUM(10)))
	int dw;
@
	/* INV: fixnnint() checks the types of `w' and `r' */
	if (type_of(w) == t_bignum || type_of(r) == t_bignum)
		@(return Cnil)
	dw = digit_weight(fixnnint(w), fixnnint(r));
	if (dw < 0)
		@(return Cnil)
	@(return CODE_CHAR(dw))
@)

short
digit_weight(int w, int r)
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
	/* INV: char_code() checks the type of `c' */
	return1(MAKE_FIXNUM(char_code(c)));
}

cl_object
cl_char_name(cl_object c)
{
	/* INV: char_code() checks the type of `c' */
	switch (char_code(c)) {
	case '\000':
		return1(cl_core.string_null);
	case '\r':
		return1(cl_core.string_return);
	case ' ':
		return1(cl_core.string_space);
	case '\177':
		return1(cl_core.string_rubout);
	case '\f':
		return1(cl_core.string_page);
	case '\t':
		return1(cl_core.string_tab);
	case '\b':
		return1(cl_core.string_backspace);
	case '\n':
		return1(cl_core.string_newline);
	}
	return1(Cnil);
}

cl_object
cl_name_char(cl_object s)
{
	char c;

	s = cl_string(s);
	if (string_equal(s, cl_core.string_return))
		c = '\r'; else
	if (string_equal(s, cl_core.string_space))
		c = ' '; else
	if (string_equal(s, cl_core.string_rubout))
		c = '\177'; else
	if (string_equal(s, cl_core.string_page))
		c = '\f'; else
	if (string_equal(s, cl_core.string_tab))
		c = '\t'; else
	if (string_equal(s, cl_core.string_backspace))
		c = '\b'; else
	if (string_equal(s, cl_core.string_linefeed) || string_equal(s, cl_core.string_newline))
		c = '\n'; else
	if (string_equal(s, cl_core.string_null))
		c = '\000'; else
		return1(Cnil);
	return1(CODE_CHAR(c));
}
