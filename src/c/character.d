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

/******************************* LOCALS *******************************/

static cl_object STreturn;
static cl_object STspace;
static cl_object STrubout;
static cl_object STpage;
static cl_object STtab;
static cl_object STbackspace;
static cl_object STlinefeed;
static cl_object STnewline;
static cl_object STnull;

/******************************* ------- ******************************/

cl_fixnum
char_code(cl_object c)
{
	if (CHARACTERP(c))
		return CHAR_CODE(c);
	FEtype_error_character(c);
}

@(defun standard_char_p (c)
	cl_fixnum i;
@
	/* INV: char_code() checks the type */
	i = char_code(c);
	if ((' ' <= i && i < '\177') || i == '\n')
		@(return Ct)
	@(return Cnil)
@)

@(defun graphic_char_p (c)
	cl_fixnum i;
@
	/* INV: char_code() checks the type */
	i = char_code(c);
	if (' ' <= i && i < '\177')     /* ' ' < '\177'  ??? Beppe*/
		@(return Ct)
	@(return Cnil)
@)

@(defun alpha_char_p (c)
	cl_fixnum i;
@
	/* INV: char_code() checks the type */
	i = char_code(c);
	if (isalpha(i))
		@(return Ct)
	else
		@(return Cnil)
@)

@(defun upper_case_p (c)
@
	/* INV: char_code() checks the type */
	if (isupper(char_code(c)))
		@(return Ct)
	@(return Cnil)
@)

@(defun lower_case_p (c)
@
	/* INV: char_code() checks the type */
	if (islower(char_code(c)))
		@(return Ct)
	@(return Cnil)
@)

@(defun both_case_p (c)
	cl_fixnum code;
@
	/* INV: char_code() checks the type */
	code = char_code(c);
	@(return ((isupper(code) || islower(code)) ? Ct : Cnil))
@)

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

@(defun alphanumericp (c)
	cl_fixnum i;
@
	/* INV: char_code() checks type of `c' */
	i = char_code(c);
	if (isalnum(i))
		@(return Ct)
	else
		@(return Cnil)
@)

@(defun char= (c &rest cs)
@
	/* INV: char_eq() checks types of `c' and `cs' */
	while (--narg)
		if (!char_eq(c, cl_nextarg(cs)))
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
		@(return Ct)
	c = cl_nextarg(cs);
	for (i = 2; i<=narg; i++) {
		va_list ds;
		va_start(ds, narg);
		c = cl_nextarg(cs);
		for (j = 1; j<i; j++)
			if (char_eq(cl_nextarg(ds), c))
				@(return Cnil)
	}
	@(return Ct)
@)

static cl_return
Lchar_cmp(int narg, int s, int t, va_list args)
{
	cl_object c, d;

	if (narg == 0)
		FEtoo_few_arguments(&narg);
	c = cl_nextarg(args);
	for (; --narg; c = d) {
		d = cl_nextarg(args);
		if (s*char_cmp(d, c) < t)
			return1(Cnil);
	}
	return1(Ct);
}

int
char_cmp(cl_object x, cl_object y)
{
	cl_fixnum cx = char_code(x);
	cl_fixnum cy = char_code(y);
	if (cx < cy)
		return(-1);
	if (cx > cy)
		return(1);
	return(0);
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
		if (!char_equal(c, cl_nextarg(cs)))
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
	if (narg == 0)
		@(return Ct)
	/* INV: char_equal() checks the type of its arguments */
	c = cl_nextarg(cs);
	for (i = 2;  i<=narg;  i++) {
		va_list ds;
		va_start(ds, narg);
		c = cl_nextarg(cs);
		for (j=1;  j<i;  j++)
			if (char_equal(c, cl_nextarg(ds)))
				@(return Cnil)
	}
	@(return Ct)
@)

static cl_return
Lchar_compare(int narg, int s, int t, va_list args)
{
	cl_object c, d;

	/* INV: char_compare() checks the types of its arguments */
	if (narg == 0)
		FEtoo_few_arguments(&narg);
	c = cl_nextarg(args);
	for (; --narg; c = d) {
		d = cl_nextarg(args);
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


@(defun character (x)
@
	@(return coerce_to_character(x))
@)

cl_object
coerce_to_character(cl_object x)
{
	switch (type_of(x)) {
	case t_character:
		return x;
	case t_symbol:
		x = x->symbol.name;
	case t_string:
		if (x->string.fillp == 1)
			return(CODE_CHAR(x->string.self[0]));
	default:
		FEtype_error_character(x);
	}
}

@(defun char_code (c)
@
	/* INV: char_code() checks the type of `c' */
	@(return MAKE_FIXNUM(char_code(c)))
@)

@(defun code_char (c)
	cl_fixnum fc;
@
	/* INV: fixnnint() checks the type of `c' */
	if (type_of(c) == t_bignum)
		@(return Cnil)
	if ((fc = fixnnint(c)) >= CHAR_CODE_LIMIT)
		@(return Cnil)
	@(return CODE_CHAR(fc))
@)

@(defun char_upcase (c)
	cl_fixnum code;
@
	/* INV: char_code() checks the type of `c' */
	code = char_code(c);
	@(return (islower(char_code(c)) ?
				CODE_CHAR(toupper(char_code(c))) :
				c))
@)

@(defun char_downcase (c)
	cl_fixnum code;
@
	/* INV: char_code() checks the type of `c' */
	code = char_code(c);
	@(return (isupper(char_code(c)) ?
				CODE_CHAR(tolower(char_code(c))) :
				c))
@)

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

@(defun char_int (c)
@
	/* INV: char_code() checks the type of `c' */
	@(return MAKE_FIXNUM(char_code(c)))
@)

@(defun int_char (x)
@
	/* INV: fixnnint(x) checks the type of `c' */
	if (type_of(x) == t_bignum)
		@(return Cnil)
	@(return CODE_CHAR(fixnnint(x)))
@)

@(defun char_name (c)
@
	/* INV: char_code() checks the type of `c' */
	switch (char_code(c)) {
	case '0':
		@(return STnull)
	case '\r':
		@(return STreturn)
	case ' ':
		@(return STspace)
	case '\177':
		@(return STrubout)
	case '\f':
		@(return STpage)
	case '\t':
		@(return STtab)
	case '\b':
		@(return STbackspace)
	case '\n':
		@(return STnewline)
	}
	@(return Cnil)
@)

@(defun name_char (s)
	char c;
@
	s = coerce_to_string(s);
	if (string_equal(s, STreturn))
		c = '\r'; else
	if (string_equal(s, STspace))
		c = ' '; else
	if (string_equal(s, STrubout))
		c = '\177'; else
	if (string_equal(s, STpage))
		c = '\f'; else
	if (string_equal(s, STtab))
		c = '\t'; else
	if (string_equal(s, STbackspace))
		c = '\b'; else
	if (string_equal(s, STlinefeed) || string_equal(s, STnewline))
		c = '\n'; else
	if (string_equal(s, STnull))
		c = '\000'; else
		@(return Cnil)
	@(return CODE_CHAR(c))
@)

void
init_character(void)
{
	make_constant("CHAR-CODE-LIMIT", MAKE_FIXNUM(CHAR_CODE_LIMIT));

	STreturn = make_simple_string("RETURN");
	register_root(&STreturn);
	STspace = make_simple_string("SPACE");
	register_root(&STspace);
	STrubout = make_simple_string("RUBOUT");
	register_root(&STrubout);
	STpage = make_simple_string("PAGE");
	register_root(&STpage);
	STtab = make_simple_string("TAB");
	register_root(&STtab);
	STbackspace = make_simple_string("BACKSPACE");
	register_root(&STbackspace);
	STlinefeed = make_simple_string("LINEFEED");
	register_root(&STlinefeed);
	STnull = make_simple_string("NULL");
	register_root(&STnull);
	STnewline = make_simple_string("NEWLINE");
	register_root(&STnewline);
}
