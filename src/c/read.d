/*
    read.d -- Read.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECLS is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#include "ecls.h"
#include "ecls-inl.h"
#include <ctype.h>

/******************************* EXPORTS ******************************/

cl_object standard_readtable;

cl_object @'*readtable*';
cl_object @'*read_default_float_format*';
cl_object @'*read_base*';
cl_object @'*read_suppress*';

cl_object @':junk_allowed';

cl_object siSsharp_comma;

#ifndef THREADS
cl_object READtable;
int READdefault_float_format;
int READbase;
bool READsuppress;
bool preserving_whitespace_flag;
bool escape_flag;
cl_object delimiting_char;
bool detect_eos_flag;
bool in_list_flag;
bool dot_flag;
cl_object default_dispatch_macro;
cl_object sharp_eq_context;
cl_object (*read_ch_fun)() = readc;
#endif THREADS

#ifdef CLOS
cl_object @'stream_read_line',
  @'stream_read_char',
  @'stream_unread_char',
  @'stream_peek_char',
  @'stream_listen',
  @'stream_clear_input';
#endif

/******************************* ------- ******************************/

static cl_object dispatch_reader;

#define	token_buffer	cl_token->string.self

#define	cat(c)	(READtable->readtable.table[char_code((c))].syntax_type)

static void too_long_token (void);
static void too_long_string (void);
static void extra_argument (int c, cl_object d);

static void
setup_READtable(void)
{
	READtable = current_readtable();
}

static void
setup_READ(void)
{
	cl_object x;

	READtable = current_readtable();
	x = symbol_value(@'*read_default_float_format*');
	if (x == @'single-float' || x == @'short-float')
		READdefault_float_format = 'S';
	else if (x == @'double_float' || x == @'long_float')
		READdefault_float_format = 'F';
	else {
		SYM_VAL(@'*read_default_float_format*') = @'single-float';
	FEerror("The value of *READ-DEFAULT-FLOAT-FORMAT*, ~S, was illegal.",
			1, x);
	}
	x = symbol_value(@'*read_base*');
	if (!FIXNUMP(x) || fix(x) < 2 || fix(x) > 36) {
		SYM_VAL(@'*read_base*') = MAKE_FIXNUM(10);
		FEerror("The value of *READ-BASE*, ~S, was illegal.", 1, x);
	}
	READbase = fix(x);
	READsuppress = symbol_value(@'*read_suppress*') != Cnil;
}

static void
setup_standard_READ(void)
{
	READtable = standard_readtable;
	READdefault_float_format = 'S';
	READbase = 10;
	READsuppress = FALSE;
	sharp_eq_context = Cnil;
	backq_level = 0;
}

#ifdef CLOS
cl_object
interactive_readc(cl_object stream)
{
	return _funcall(2, @'stream_read_char', stream);
}
#endif CLOS

cl_object
readc(cl_object in)
{
	return(code_char(readc_stream(in)));
}

#define	read_char(in)	(*read_ch_fun)(in)

void unread_char(cl_object c, cl_object in)
{
	/* INV: char_code() checks the type of `c' */
#ifdef CLOS
	if (type_of(in) == t_instance)
	  funcall(3, @'stream_unread_char', in, c);
	else
#endif
	  unreadc_stream(char_code(c), in);
}

/*
	peek_char corresponds to COMMON Lisp function PEEK-CHAR.
	When pt is TRUE, preceeding whitespaces are ignored.
*/
cl_object
peek_char(bool pt, cl_object in)
{
	cl_object c;

	c = read_char(in);
	if (pt)
	  while (cat(c) == cat_whitespace)
	    c = read_char(in);
	unread_char(c, in);
	return(c);
}

static cl_object
read_object_recursive(cl_object in)
{
	volatile cl_object x;
	bool e;

	cl_object old_READtable = READtable;
	int old_READdefault_float_format = READdefault_float_format;
	int old_READbase = READbase;
	bool old_READsuppress = READsuppress;

	if (frs_push(FRS_PROTECT, Cnil))
		e = TRUE;
	else {
		setup_READ();
		x = read_object(in);
		e = FALSE;
	}
	frs_pop();

	READtable = old_READtable;
	READdefault_float_format = old_READdefault_float_format;
	READbase = old_READbase;
	READsuppress = old_READsuppress;

	if (e) unwind(nlj_fr, nlj_tag);
	return(x);
}

cl_object
read_object_non_recursive(cl_object in)
{
	volatile cl_object x;
	bool e;
	cl_object old_READtable;
	int old_READdefault_float_format;
	int old_READbase;
	int old_READsuppress;
	int old_backq_level;
	cl_object old_sharp_eq_context;

	old_READtable = READtable;
	old_READdefault_float_format = READdefault_float_format;
	old_READbase = READbase;
	old_READsuppress = READsuppress;
	old_sharp_eq_context = sharp_eq_context;
	old_backq_level = backq_level;
	setup_READ();
	sharp_eq_context = Cnil;
	backq_level = 0;

	if (frs_push(FRS_PROTECT, Cnil))
		e = TRUE;
	else {
	  	static cl_object patch_sharp(cl_object x);
		e = FALSE;
		x = read_object(in);
		if (!Null(sharp_eq_context))
			x = patch_sharp(x);
	}
	frs_pop();

	READtable = old_READtable;
	READdefault_float_format = old_READdefault_float_format;
	READbase = old_READbase;
	READsuppress = old_READsuppress;
	sharp_eq_context = old_sharp_eq_context;
	backq_level = old_backq_level;
	if (e) unwind(nlj_fr, nlj_tag);
	return(x);
}

#if TK
extern bool no_input;
#define GETC(c, fp)	{ if (fp == stdin) \
			   while (no_input) Tk_DoOneEvent(0); \
			  c = getc(fp); \
			  no_input = !FILE_CNT(fp); }
#else
#define GETC(c, fp)	c = getc(fp)
#endif /* TK */

/* Beppe: faster code for inner loop from file stream */
#if !defined(CLOS)
#define READ_CHAR_TO(res, in, eof_code) \
  {FILE *fp = in->stream.file; \
     if (fp) { int ch; \
	       GETC(ch, fp); \
	       if (ch == EOF) \
		 {eof_code;} \
	       else res = code_char(ch);} \
      else \
	if (stream_at_end(in)) \
	   {eof_code;} \
	else res = read_char(in);}
#else
#define READ_CHAR_TO(res, in, eof_code) \
  {if (stream_at_end(in)) {eof_code;} else res = read_char(in);}
#endif unix

/*
	Read_object(in) reads an object from stream in.
	This routine corresponds to COMMON Lisp function READ.
*/
cl_object
read_object(cl_object in)
{
	cl_object x, c;
	enum chattrib a;
	cl_object old_delimiter, p;
	cl_index length, i, colon;
	int colon_type;
	bool df, ilf;

	cs_check(in);

	old_delimiter = delimiting_char;
	delimiting_char = OBJNULL;
	df = detect_eos_flag;
	detect_eos_flag = FALSE;
	ilf = in_list_flag;
	in_list_flag = FALSE;
	dot_flag = FALSE;

BEGIN:
	/* Beppe: */
	do {
		READ_CHAR_TO(c, in, if (df) return(OBJNULL);
					else
					    FEend_of_file(in));
		a = cat(c);
	} while (a == cat_whitespace);
	delimiting_char = OBJNULL;
	if (old_delimiter != OBJNULL && old_delimiter == c)
		return(OBJNULL);
	if (a == cat_terminating || a == cat_non_terminating) {
		cl_object x = READtable->readtable.table[char_code(c)].macro;
		cl_object o = _funcall(3, x, in, c);
		if (NValues == 0) goto BEGIN;
		if (NValues > 1) FEerror("The readmacro ~S returned ~D values.",
					 2, x, MAKE_FIXNUM(i));
		return o;
	}
	escape_flag = FALSE;
	length = 0;
	colon_type = 0;
	for (;;) {
		if (a == cat_single_escape) {
			c = read_char(in);
			a = cat_constituent;
			escape_flag = TRUE;
		} else if (a == cat_multiple_escape) {
			escape_flag = TRUE;
			for (;;) {
				if (stream_at_end(in))
					FEend_of_file(in);
				c = read_char(in);
				a = cat(c);
				if (a == cat_single_escape) {
					c = read_char(in);
					a = cat_constituent;
				} else if (a == cat_multiple_escape)
					break;
				if (length >= cl_token->string.dim)
					too_long_token();
				token_buffer[length++] = char_code(c);
			}
			goto NEXT;
		} else if ('a' <= char_code(c) && char_code(c) <= 'z')
			c = code_char(toupper(char_code(c)));
		else if (char_code(c) == ':') {
			if (colon_type == 0) {
				colon_type = 1;
				colon = length;
			} else if (colon_type == 1 && colon == length-1)
				colon_type = 2;
			else
				colon_type = -1;
				/*  Colon has appeared twice.  */
		}
		if (a == cat_whitespace || a == cat_terminating) {
			if (preserving_whitespace_flag ||
			    cat(c) != cat_whitespace)
				unread_char(c, in);
			break;
		}
		if (length >= cl_token->string.dim)
			too_long_token();
		token_buffer[length++] = char_code(c);
	NEXT:
		READ_CHAR_TO(c, in, break);
		a = cat(c);
	}

	if (READsuppress) {
		cl_token->string.fillp = length;
		return(Cnil);
	}
	if (ilf && !escape_flag &&
	    length == 1 && cl_token->string.self[0] == '.') {
		dot_flag = TRUE;
		return(Cnil);
	} else if (!escape_flag && length > 0) {
		for (i = 0;  i < length;  i++)
			if (cl_token->string.self[i] != '.')
				goto N;
		FEerror("Dots appeared illegally.", 0);
	}

N:
	cl_token->string.fillp = length;
	if (escape_flag || (READbase <= 10 && token_buffer[0] > '9'))
		goto SYMBOL;
	x = parse_number(token_buffer, length, &i, READbase);
	if (x != OBJNULL && length == i)
		return(x);

SYMBOL:
	if (colon_type == 1 /* && length > colon + 1 */) {
		if (colon == 0)
			p = keyword_package;
		else {
			cl_token->string.fillp = colon;
			p = find_package(cl_token);
			if (Null(p))
			    FEerror("There is no package with the name ~A.",
				    1, copy_simple_string(cl_token));
		}
		for (i = colon + 1;  i < length;  i++)
			token_buffer[i - (colon + 1)]
			= token_buffer[i];
		cl_token->string.fillp = length - (colon + 1);
		if (colon > 0) {
			cl_token->string.self[cl_token->string.fillp] = '\0';
			x = find_symbol(cl_token, p);
			if (intern_flag != EXTERNAL)
			FEerror("Cannot find the external symbol ~A in ~S.",
						2, copy_simple_string(cl_token), p);
			return(x);
		}
	} else if (colon_type == 2 /* && colon > 0 && length > colon + 2 */) {
		cl_token->string.fillp = colon;
		p = find_package(cl_token);
		if (Null(p))
			FEerror("There is no package with the name ~A.",
				1, copy_simple_string(cl_token));
		for (i = colon + 2;  i < length;  i++)
			token_buffer[i - (colon + 2)] = token_buffer[i];
		cl_token->string.fillp = length - (colon + 2);
	} else
		p = current_package();
	token_buffer[cl_token->string.fillp] = '\0';
	x = intern(cl_token, p);
	if (x->symbol.name == cl_token)
		x->symbol.name = copy_simple_string(cl_token);
	return(x);
}

#define	is_exponent_marker(i)	\
	((i) == 'e' || (i) == 'E' ||	\
	 (i) == 's' || (i) == 'S' || (i) == 'f' || (i) == 'F' || \
	 (i) == 'd' || (i) == 'D' || (i) == 'l' || (i) == 'L' || \
	 (i) == 'b' || (i) == 'B')

#define basep(d)	(d <= 36)

/*
	parse_number(s, end, ep, radix) parses C string s
	up to (but not including) s[end]
	using radix as the radix for the rational number.
	(For floating numbers, radix should be 10.)
	When parsing succeeds,
	the index of the next character is assigned to *ep,
	and the number is returned as a lisp data object.
	If not, OBJNULL is returned.
*/
cl_object
parse_number(char *s, cl_index end, cl_index *ep, int radix)
{
	cl_object x, y;
	int sign;
	cl_object integer_part;
	double fraction, fraction_unit, f;
	char exponent_marker;
	int exponent, d;
	cl_index i, j, k;

	if (s[end-1] == '.')
		radix = 10;
		/*
			DIRTY CODE!!
		*/
BEGIN:
	exponent_marker = 'E';
	i = 0;
	sign = 1;
	if (s[i] == '+')
		i++;
	else if (s[i] == '-') {
		sign = -1;
		i++;
	}
	integer_part = big_register0_get();
	if (i >= end)
		goto NO_NUMBER;
	if (s[i] == '.') {
		if (radix != 10) {
			radix = 10;
			goto BEGIN;
		}
		i++;
		goto FRACTION;
	}
	if (!basep(radix) || (d = digitp(s[i], radix)) < 0)
		goto NO_NUMBER;
	do {
		big_mul_ui(integer_part, radix);
		big_add_ui(integer_part, d);
		i++;
	} while (i < end && (d = digitp(s[i], radix)) >= 0);
	if (i >= end)
		goto MAKE_INTEGER;
	if (s[i] == '.') {
		if (radix != 10) {
			radix = 10;
			goto BEGIN;
		}
		if (++i >= end)
			goto MAKE_INTEGER;
		else if (digitp(s[i], radix) >= 0)
			goto FRACTION;
		else if (is_exponent_marker(s[i])) {
			fraction = (double)sign * big_to_double(integer_part);
			goto EXPONENT;
		} else
			goto MAKE_INTEGER;
	}
	if (s[i] == '/') {
	  i++;
	  if (sign < 0)
	    big_complement(integer_part);
	  x = big_register_normalize(integer_part);

	  /* 		DENOMINATOR			 */

	  if ((d = digitp(s[i], radix)) < 0)
	    goto NO_NUMBER;
	  integer_part = big_register0_get();
	  do {
	    big_mul_ui(integer_part, radix);
	    big_add_ui(integer_part, d);
	    i++;
	  } while (i < end && (d = digitp(s[i], radix)) >= 0);
	  y = big_register_normalize(integer_part);
	  x = make_ratio(x, y);
	  goto END;
	}

	if (is_exponent_marker(s[i])) {
		fraction = (double)sign * big_to_double(integer_part);
		goto EXPONENT;
	}

	goto NO_NUMBER;

MAKE_INTEGER:
	if (sign < 0)
		big_complement(integer_part);
	x = big_register_normalize(integer_part);
	goto END;

FRACTION:

	if (radix != 10)
		goto NO_NUMBER;

	radix = 10;
	if ((d = digitp(s[i], radix)) < 0)
		goto NO_NUMBER;
	fraction = 0.0;
	fraction_unit = 1000000000.0;
	for (;;) {
		k = j = 0;
		do {
			j = 10*j + d;
			i++;
			k++;
			if (i < end)
				d = digitp(s[i], radix);
			else
				break;
		} while (k < 9 && d >= 0);
		while (k++ < 9)
			j *= 10;
		fraction += ((double)j /fraction_unit);
		if (i >= end || d < 0)
			break;
		fraction_unit *= 1000000000.0;
	}
	fraction += big_to_double(integer_part);
	fraction *= (double)sign;
	if (i >= end)
		goto MAKE_FLOAT;
	if (is_exponent_marker(s[i]))
		goto EXPONENT;
	goto MAKE_FLOAT;

EXPONENT:

	if (radix != 10)
		goto NO_NUMBER;

	radix = 10;
	exponent_marker = s[i];
	i++;
	if (i >= end)
		goto NO_NUMBER;
	sign = 1;
	if (s[i] == '+')
		i++;
	else if (s[i] == '-') {
		sign = -1;
		i++;
	}
	if (i >= end)
		goto NO_NUMBER;
	if ((d = digitp(s[i], radix)) < 0)
		goto NO_NUMBER;
	exponent = 0;
	do {
		exponent = 10 * exponent + d;
		i++;
	} while (i < end && (d = digitp(s[i], radix)) >= 0);
	d = exponent;
	f = 10.0;
	/* Use pow because it is more accurate */
	{
	  double pow(double, double);
	  double po = pow(10.0, (double)(sign * d));
          if (po == 0.0) {
	    fraction *= pow(10.0, (double)(sign * (d-1)));
	    fraction /= 10.0;
	  } else     
	    fraction *= po;
	}

MAKE_FLOAT:
	switch (exponent_marker) {

	case 'e':  case 'E':
		exponent_marker = READdefault_float_format;
		goto MAKE_FLOAT;

	case 's':  case 'S':
#ifdef IEEEFLOAT
		{
		  float biggest_float;
		  *((int *)&biggest_float) = 0x7f7fffff;
		  if (fraction > biggest_float || fraction < -biggest_float)
		    FEerror("Floating-point overflow.", 0);
		}
#endif
		x = make_shortfloat((float)fraction);
		break;

	case 'f':  case 'F':  case 'd':  case 'D':  case 'l':  case 'L':
#ifdef IEEEFLOAT
		if ((*((int *)&fraction + HIND) & 0x7ff00000) == 0x7ff00000)
		  FEerror("Floating-point overflow.", 0);
#endif
		x = make_longfloat((double)fraction);
		break;

	case 'b':  case 'B':
		goto NO_NUMBER;
	}

END:
	*ep = i;
	return(x);

NO_NUMBER:
	*ep = i;
	return(OBJNULL);
}

cl_object
parse_integer(char *s, cl_index end, cl_index *ep, int radix)
{
	cl_object x;
	int sign, d;
	cl_object integer_part;
	cl_index i;

	i = 0;
	sign = 1;
	if (s[i] == '+')
		i++;
	else if (s[i] == '-') {
		sign = -1;
		i++;
	}
	if (i >= end || !basep(radix) || (d = digitp(s[i], radix)) < 0) {
	  *ep = i;
	  return(OBJNULL);
	}
	integer_part = big_register0_get();
	do {
		big_mul_ui(integer_part, radix);
		big_add_ui(integer_part, d);
		i++;
	} while (i < end && (d = digitp(s[i], radix)) >= 0);
	if (sign < 0)
		big_complement(integer_part);
	x = big_register_normalize(integer_part);
	*ep = i;
	return(x);
}

static
@(defun si::left_parenthesis_reader (in c)
  cl_object x, y;
  cl_object *p;
@
  y = Cnil;
  for (p = &y ; ; p = &(CDR(*p))) {
    delimiting_char = code_char(')');
    in_list_flag = TRUE;
    x = read_object(in);
    if (x == OBJNULL)
      break;
    if (dot_flag) {
      if (p == &y)
	FEerror("A dot appeared after a left parenthesis.", 0);
      in_list_flag = TRUE;
      *p = read_object(in);
      if (dot_flag)
	FEerror("Two dots appeared consecutively.", 0);
      c = read_char(in);
      while (cat(c) == cat_whitespace)
	c = read_char(in);
      if (char_code(c) != ')')
	FEerror("A dot appeared before a right parenthesis.", 0);
      break;
    }
    *p = CONS(x, Cnil);
  }
  @(return y)
@)
/*
	read_string(delim, in) reads
	a simple string	terminated by character code delim
	and places it in token.
	Delim is not included in the string but discarded.
*/
static void
read_string(int delim, cl_object in)
{
	cl_index i;
	cl_object c;

	i = 0;
	for (;;) {
		c = read_char(in);
		if (char_code(c) == delim)
			break;
		else if (cat(c) == cat_single_escape)
			c = read_char(in);
		if (i >= cl_token->string.dim)
			too_long_string();
		token_buffer[i++] = char_code(c);
	}
	cl_token->string.fillp = i;
	cl_token->string.self[i] = '\0';
}

/*
	Read_constituent(in) reads
	a sequence of constituent characters from stream in
	and places it in token_buffer.
*/
static void
read_constituent(cl_object in)
{
	size_t i;
	cl_object c;

	i = 0;
	for (;;) {
		c = read_char(in);
		if (cat(c) != cat_constituent) {
			unread_char(c, in);
			break;
		}
		token_buffer[i++] = char_code(c);
	}
	cl_token->string.fillp = i;
}

static
@(defun si::double_quote_reader (in c)
@
	read_string('"', in);
	@(return copy_simple_string(cl_token))
@)

static
@(defun si::dispatch_reader (in dc)
	cl_object c, x, y;
	int i, d;
@
	if (READtable->readtable.table[char_code(dc)].dispatch_table == NULL)
		FEerror("~C is not a dispatching macro character", 1, dc);

	c = read_char(in);
	d = digitp((int)char_code(c), 10);
	if (d >= 0) {
		i = 0;
		do {
			i = 10*i + d;
			c = read_char(in);
			d = digitp(char_code(c), 10);
		} while (d >= 0);
		y = MAKE_FIXNUM(i);
	} else
		y = Cnil;

	x = READtable->readtable.table[char_code(dc)].dispatch_table[char_code(c)];
	return funcall(4, x, in, c, y);
@)

static
@(defun si::single_quote_reader (in c)
@
	@(return CONS(@'quote', CONS(read_object(in), Cnil)))
@)

static
@(defun si::void_reader (in c)
@
	/*  no result  */
	@(return)
@)

#define @si::right_parenthesis_reader @si::void_reader

/*
int
@comma-reader(){} in backq.c
*/

static
@(defun si::semicolon_reader (in c)
@
	do
		c = read_char(in);
	while (char_code(c) != '\n');
	/*  no result  */
	@(return)
@)

/*
int
@backquote-reader(){}
*/

/*
	sharpmacro routines
*/

static
@(defun si::sharp_C_reader (in c d)
	cl_object x, real, imag;
@
	if (d != Cnil && !READsuppress)
		extra_argument('C', d);
	c = read_char(in);
	if (char_code(c) != '(')
		FEerror("A left parenthesis is expected.", 0);
	delimiting_char = code_char(')');
	real = read_object(in);
	if (real == OBJNULL)
		FEerror("No real part.", 0);
	delimiting_char = code_char(')');
	imag = read_object(in);
	if (imag == OBJNULL)
		FEerror("No imaginary part.", 0);
	delimiting_char = code_char(')');
	x = read_object(in);
	if (x != OBJNULL)
		FEerror("A right parenthesis is expected.", 0);
	if (READsuppress)
		@(return Cnil)
	if (contains_sharp_comma(real) ||
	    contains_sharp_comma(imag)) {
		x = alloc_object(t_complex);
		x->complex.real = real;
		x->complex.imag = imag;
	} else {
		/* INV: make_complex() checks its types */
		x = make_complex(real, imag);
	}
	@(return x)
@)

static
@(defun si::sharp_backslash_reader (in c d)
@
	if (d != Cnil && !READsuppress)
		if (!FIXNUMP(d) ||
		    fix(d) != 0)
			FEerror("~S is an illegal CHAR-FONT.", 1, d);
			/*  assuming that CHAR-FONT-LIMIT is 1  */
	unread_char(code_char('\\'), in);
	if (READsuppress) {
		(void)read_object(in);
		@(return Cnil)
	}
	READsuppress = TRUE;
	(void)read_object(in);
	READsuppress = FALSE;
	c = cl_token;
	if (c->string.fillp == 1)
		c = code_char(c->string.self[0]);
	/*	#\^x	*/
	else if (c->string.fillp == 2 && c->string.self[0] == '^')
		c = code_char(c->string.self[1] & 037);
	else if (c->string.self[0] =='\\' && c->string.fillp > 1) {
		cl_index i, n;
		for (n = 0, i = 1;  i < c->string.fillp;  i++)
			if (c->string.self[i] < '0' ||
			    '7' < c->string.self[i])
				FEerror("Octal digit expected.", 0);
			else
				n = 8*n + c->string.self[i] - '0';
		c = code_char(n & 0377);
	} else {
		c = @name_char(1,c);
		if (Null(c)) FEerror("~S is an illegal character name.", 1, c);
	}
	@(return c)
@)

static
@(defun si::sharp_single_quote_reader (in c d)
@
	if(d != Cnil && !READsuppress)
		extra_argument('#', d);
	@(return CONS(@'function', CONS(read_object(in), Cnil)))
@)

#define	QUOTE	1
#define	EVAL	2
#define	LIST	3
#define	LISTX	4
#define	APPEND	5
#define	NCONC	6


/*
 *----------------------------------------------------------------------
 *	Stack of unknown size
 *----------------------------------------------------------------------
 */

#define INCREMENT 64
#define ESTACK(st)	volatile int _esize = 0; cl_object *(st), *(st ## 0);
#define ETOP(st)	(st ## 0)

#define EPUSH(st, val, count) \
    { int i; if (count == _esize) { \
      st = (cl_object *)alloca(INCREMENT*sizeof(cl_object)); \
      for ( i = 0; i < _esize; i++) \
	st[i] = st ## 0[i]; \
      (st ## 0) = st; st += _esize;\
      _esize += INCREMENT; \
    }; *(st)++ = (val);}


static
@(defun si::sharp_left_parenthesis_reader (in c d)
	int dim, dimcount, i, a;
	cl_object x, last;
	ESTACK(vsp);
@
	if (Null(d) || READsuppress)
		dim = -1;
	else if (FIXNUMP(d))
		dim = fix(d);
	if (backq_level > 0) {
		unreadc_stream('(', in);
		x = read_object(in);
		a = backq_car(&x);
		if (a == APPEND || a == NCONC)
		  FEerror(",at or ,. has appeared in an illegal position.", 0);
		if (a == QUOTE) {
		  for (dimcount = 0;  !endp(x);  x = CDR(x), dimcount++)
		    EPUSH(vsp, CAR(x), dimcount);
		  goto L;
		}
		@(return list(4, siScomma, @'apply',
			      CONS(@'quote', CONS(@'vector', Cnil)), x))
	}
	for (dimcount = 0 ;; dimcount++) {
	  delimiting_char = code_char(')');
	  x = read_object(in);
	  if (x == OBJNULL)
	    break;
	  EPUSH(vsp, x, dimcount);
	}
L:
	if (dim >= 0) {
		if (dimcount > dim)
			FEerror("Too many elements in #(...).", 0);
		if (dimcount == 0)
			FEerror("Cannot fill the vector #().", 0);
		else last = vsp[-1];
	} else
	  dim = dimcount;
	x = alloc_simple_vector(dim, aet_object);
	x->vector.self.t = alloc_align(dim * sizeof(cl_object), sizeof(cl_object));
	for (i = 0; i < dim; i++)
		x->vector.self.t[i] = (i < dimcount) ? ETOP(vsp)[i] : last;
	@(return x)
@)

static
@(defun si::sharp_asterisk_reader (in c d)
	int dim, dimcount, i;
	cl_object x, last, elt;
	ESTACK(vsp);
@
	if (READsuppress) {
		read_constituent(in);
		@(return Cnil)
	}
	if (Null(d))
		dim = -1;
	else if (FIXNUMP(d))
		dim = fix(d);
	for (dimcount = 0 ;; dimcount++) {
		if (stream_at_end(in))
			break;
		x = read_char(in);
		if (char_code(x) != '0' && char_code(x) != '1') {
			unread_char(x, in);
			break;
		}
		EPUSH(vsp, x, dimcount);
	}	
	if (dim >= 0) {
		if (dimcount > dim)
			FEerror("Too many elements in #*....", 0);
		if (dimcount == 0)
			FEerror("Cannot fill the bit-vector #*.", 0);
		else last = vsp[-1];
	} else {
	  dim = dimcount;	/* Beppe ? */
	  last = MAKE_FIXNUM(0);
	}
	x = alloc_simple_bitvector(dim);
	x->vector.self.bit = alloc_atomic((dim + CHAR_BIT - 1)/CHAR_BIT);
	for (i = 0; i < dim; i++) {
		elt = (i < dimcount) ? ETOP(vsp)[i] : last;
		if (char_code(elt) == '0')
			x->vector.self.bit[i/CHAR_BIT] &= ~(0200 >> i%CHAR_BIT);
		else
			x->vector.self.bit[i/CHAR_BIT] |= 0200 >> i%CHAR_BIT;
		}
	@(return x)
@)

static
@(defun si::sharp_colon_reader (in c d)
	cl_index length;
	enum chattrib a;
@
	if (d != Cnil && !READsuppress)
		extra_argument(':', d);
	c = read_char(in);
	a = cat(c);
	escape_flag = FALSE;
	length = 0;
	goto L;
	for (;;) {
		if (length >= cl_token->string.dim)
			too_long_token();
		token_buffer[length++] = char_code(c);
	K:
		if (stream_at_end(in))
			goto M;
		c = read_char(in);
		a = cat(c);
	L:
		if (a == cat_single_escape) {
			c = read_char(in);
			a = cat_constituent;
			escape_flag = TRUE;
		} else if (a == cat_multiple_escape) {
			escape_flag = TRUE;
			for (;;) {
				if (stream_at_end(in))
					FEend_of_file(in);
				c = read_char(in);
				a = cat(c);
				if (a == cat_single_escape) {
					c = read_char(in);
					a = cat_constituent;
				} else if (a == cat_multiple_escape)
					break;
				if (length >= cl_token->string.dim)
					too_long_token();
				token_buffer[length++] = char_code(c);
			}
			goto K;
		} else if ('a' <= char_code(c) && char_code(c) <= 'z')
			c = code_char(toupper(char_code(c)));
		if (a == cat_whitespace || a == cat_terminating)
			break;
	}
	if (preserving_whitespace_flag || cat(c) != cat_whitespace)
		unread_char(c, in);

M:
	if (READsuppress)
		@(return Cnil)
	cl_token->string.fillp = length;
	@(return make_symbol(copy_simple_string(cl_token)))
@)

static
@(defun si::sharp_dot_reader (in c d)
	cl_object lex_old = lex_env;
@
	if(d != Cnil && !READsuppress)
		extra_argument('.', d);
	if (READsuppress)
		@(return Cnil)
	in = read_object(in);
	lex_new();
	in = eval(in, NULL);
	lex_env = lex_old;
	@(return in)
@)

static
@(defun si::sharp_comma_reader (in c d)
	cl_object lex_old = lex_env;
@
	if(d != Cnil && !READsuppress)
		extra_argument(',', d);
	if (READsuppress)
		@(return Cnil)
	in = read_object(in);
	lex_new();
	in = eval(in, NULL);
	lex_env = lex_old;
	@(return in)
@)

@(defun si::sharp_comma_reader_for_compiler (in c d)
@
	if(d != Cnil && !READsuppress)
		extra_argument(',', d);
	if (READsuppress)
		@(return Cnil)
	@(return CONS(siSsharp_comma, read_object(in)))
@)

/*
	For fasload.
*/
static cl_object read_VV_block = OBJNULL;

static
@(defun si::sharp_exclamation_reader (in c d)
	cl_fixnum code;
@
	if(d != Cnil && !READsuppress)
		extra_argument('!', d);
	if (READsuppress)
		@(return Cnil)
	code = fixint(read_object(in));
	switch (code) {
	case 0: {
		cl_object name = read_object(in);
		@si::select-package(1,name);
		break;
	}
	case 1: {
		cl_object name = read_object(in);
		cl_object p = find_package(name);
		if (Null(p)) make_package(name,Cnil,Cnil);
		break;
	}
	default:
		code = -code - 1;
		if (code < 0 || code >= read_VV_block->cblock.data_size)
			FEerror("Bogus binary file. #!~S unknown.",1,
				MAKE_FIXNUM(code));
		@(return read_VV_block->cblock.data[code])
	}
	@(return)
@)

static
@(defun si::sharp_B_reader (in c d)
	cl_index i;
	cl_object x;
@
	if(d != Cnil && !READsuppress)
		extra_argument('B', d);
	read_constituent(in);
	if (READsuppress)
		@(return Cnil)
	x = parse_number(token_buffer, cl_token->string.fillp, &i, 2);
	if (x == OBJNULL || i != cl_token->string.fillp)
		FEerror("Cannot parse the #B readmacro.", 0);
	if (type_of(x) == t_shortfloat ||
	    type_of(x) == t_longfloat)
		FEerror("The float ~S appeared after the #B readmacro.",
			1, x);
	@(return x)
@)

static
@(defun si::sharp_O_reader (in c d)
	cl_index i;
	cl_object x;
@
	if(d != Cnil && !READsuppress)
		extra_argument('O', d);
	read_constituent(in);
	if (READsuppress)
		@(return Cnil)
	x = parse_number(token_buffer, cl_token->string.fillp, &i, 8);
	if (x == OBJNULL || i != cl_token->string.fillp)
		FEerror("Cannot parse the #O readmacro.", 0);
	if (type_of(x) == t_shortfloat ||
	    type_of(x) == t_longfloat)
		FEerror("The float ~S appeared after the #O readmacro.",
			1, x);
	@(return x)
@)

static
@(defun si::sharp_X_reader (in c d)
	cl_index i;
	cl_object x;
@
	if(d != Cnil && !READsuppress)
		extra_argument('X', d);
	read_constituent(in);
	if (READsuppress)
		@(return Cnil)
	x = parse_number(token_buffer, cl_token->string.fillp, &i, 16);
	if (x == OBJNULL || i != cl_token->string.fillp)
		FEerror("Cannot parse the #X readmacro.", 0);
	if (type_of(x) == t_shortfloat ||
	    type_of(x) == t_longfloat)
		FEerror("The float ~S appeared after the #X readmacro.",
			1, x);
	@(return x)
@)

static
@(defun si::sharp_R_reader (in c d)
	int radix;
	cl_index i;
	cl_object x;
@
	if (READsuppress)
		radix = 10;
	else if (FIXNUMP(d)) {
		radix = fix(d);
		if (radix > 36 || radix < 2)
			FEerror("~S is an illegal radix.", 1, d);
	} else
		FEerror("No radix was supplied in the #R readmacro.", 0);
	read_constituent(in);
	if (READsuppress)
		@(return Cnil)
	x = parse_number(token_buffer, cl_token->string.fillp, &i, radix);
	if (x == OBJNULL || i != cl_token->string.fillp)
		FEerror("Cannot parse the #R readmacro.", 0);
	if (type_of(x) == t_shortfloat ||
	    type_of(x) == t_longfloat)
		FEerror("The float ~S appeared after the #R readmacro.",
			1, x);
	@(return x)
@)

#define sharp_A_reader @void-reader
#define sharp_S_reader @void-reader

static
@(defun si::sharp_eq_reader (in c d)
	cl_object pair, value;
@
	if (READsuppress) @(return)
	if (Null(d))
		FEerror("The #= readmacro requires an argument.", 0);
	if (assql(d, sharp_eq_context) != Cnil)
		FEerror("Duplicate definitions for #~D=.", 1, d);
	pair = CONS(d, Cnil);
	sharp_eq_context = CONS(pair, sharp_eq_context);
	value = read_object(in);
	if (value == pair)
		FEerror("#~D# is defined by itself.", 1, d);
	@(return (CDR(pair) = value))
@)

static
@(defun si::sharp_sharp_reader (in c d)
	cl_object pair;
@
	if (READsuppress) @(return)
	if (Null(d))
		FEerror("The ## readmacro requires an argument.", 0);
	pair = assq(d, sharp_eq_context);
	if (pair != Cnil)
		@(return pair)
	FEerror("#~D# is undefined.", 1, d);
@)

static cl_object
do_patch_sharp(cl_object x)
{
	cs_check(x);

	switch (type_of(x)) {
	case t_cons: {
	  	cl_object y = x;
		cl_object *place = &x;
		do {
			/* This was the result of a #d# */
			if (CAR(y) == OBJNULL) {
				*place = CDR(y);
				return x;
			} else
				CAR(y) = do_patch_sharp(CAR(y));
			place = &CDR(y);
			y = CDR(y);
		} while (CONSP(y));
		break;
	}
	case t_vector: {
		cl_index i;

		for (i = 0;  i < x->vector.fillp;  i++)
			x->vector.self.t[i] = do_patch_sharp(x->vector.self.t[i]);
		break;
	}
	case t_array: {
		cl_index i, j;

		for (i = 0, j = 1;  i < x->array.rank;  i++)
			j *= x->array.dims[i];
		for (i = 0;  i < j;  i++)
			x->array.self.t[i] = do_patch_sharp(x->array.self.t[i]);
		break;
	}
	default:
	}
	return(x);
}

static cl_object
patch_sharp(cl_object x)
{
	cl_object pair = sharp_eq_context;
	loop_for_in(pair) { 
		CAAR(pair) = OBJNULL;
	} end_loop_for_in;

	x = do_patch_sharp(x);

	pair = sharp_eq_context;
	loop_for_in(pair) { 
		CAAR(pair) = Cnil;
	} end_loop_for_in;
	return x;
}

#define @si::sharp_plus_reader @si::void_reader
#define @si::sharp_minus_reader @si::void_reader
#define @si::sharp_less_than_reader @si::void_reader
#define @si::sharp_whitespace_reader @si::void_reader
#define @si::sharp_right_parenthesis_reader @si::void_reader

static
@(defun si::sharp_vertical_bar_reader (in ch d)
	int c;
	int level = 0;
@
	if (d != Cnil && !READsuppress)
		extra_argument('|', d);
	for (;;) {
		c = char_code(read_char(in));
	L:
		if (c == '#') {
			c = char_code(read_char(in));
			if (c == '|')
				level++;
		} else if (c == '|') {
			c = char_code(read_char(in));
			if (c == '#') {
				if (level == 0)
					break;
				else
					--level;
			} else
				goto L;
		}
	}
	@(return)
	/*  no result  */
@)

static
@(defun si::default_dispatch_macro (in c d)
@
	FEerror("Undefined dispatch macro character.", 1, c);
@)

/*
	#P" ... " returns the pathname with namestring ... .
*/
static
@(defun si::sharp_P_reader (in c d)
@
	@(return coerce_to_pathname(read_object(in)))
@)

/*
	#" ... " returns the pathname with namestring ... .
*/
static
@(defun si::sharp_double_quote_reader (in c d)
@
	if (d != Cnil && !READsuppress)
		extra_argument('"', d);
	unread_char(c, in);
	@(return coerce_to_pathname(read_object(in)))
@)

/*
	#$ fixnum returns a random-state with the fixnum
	as its content.
*/
static
@(defun si::sharp_dollar_reader (in c d)
	cl_object output;
@
	if (d != Cnil && !READsuppress)
		extra_argument('$', d);
	c = read_object(in);
	if (!FIXNUMP(c))
		FEerror("Cannot make a random-state with the value ~S.",
			1, c);
	output = alloc_object(t_random);
	output->random.value = fix(c);
	@(return output)
@)

/*
	readtable routines
*/

cl_object
copy_readtable(cl_object from, cl_object to)
{
	struct readtable_entry *rtab;
	cl_index i;

	if (Null(to)) {
		to = alloc_object(t_readtable);
		to->readtable.table = NULL;
			/*  Saving for GC.  */
		to->readtable.table
		= rtab
 		= alloc_align(RTABSIZE * sizeof(struct readtable_entry), sizeof(struct readtable_entry));
		memcpy(rtab, from->readtable.table,
			 RTABSIZE * sizeof(struct readtable_entry));
/*
		for (i = 0;  i < RTABSIZE;  i++)
			rtab[i] = from->readtable.table[i];
*/
				/*  structure assignment  */
	} else
	  rtab=to->readtable.table;
	for (i = 0;  i < RTABSIZE;  i++)
		if (from->readtable.table[i].dispatch_table != NULL) {
			rtab[i].dispatch_table
 			= alloc_align(RTABSIZE * sizeof(cl_object), sizeof(cl_object));
			memcpy(rtab[i].dispatch_table, from->readtable.table[i].dispatch_table,
			      RTABSIZE * sizeof(cl_object *));
/*
			for (j = 0;  j < RTABSIZE;  j++)
				rtab[i].dispatch_table[j]
				= from->readtable.table[i].dispatch_table[j];
*/
		}
	return(to);
}

cl_object
current_readtable(void)
{
	cl_object r;

	r = symbol_value(@'*readtable*');
	if (type_of(r) != t_readtable) {
	  SYM_VAL(@'*readtable*') = copy_readtable(standard_readtable, Cnil);
	  FEerror("The value of *READTABLE*, ~S, was not a readtable.",
		  1, r);
	}
	return(r);
}


@(defun read (&optional (strm symbol_value(@'*standard_input*'))
			(eof_errorp Ct)
			eof_value
			recursivep
	      &aux x)
@
	if (Null(strm))
		strm = symbol_value(@'*standard_input*');
	else if (strm == Ct)
		strm = symbol_value(@'*terminal_io*');
RETRY:	if (type_of(strm) == t_stream) {
          if (strm->stream.mode == (short)smm_synonym) {
	    strm = symbol_value(strm->stream.object0);
	    goto RETRY;
	  }
	  else
	    read_ch_fun = readc;
	} else
#ifdef CLOS
	  if (type_of(strm) == t_instance)
	    read_ch_fun = interactive_readc;
	  else
#endif CLOS
	    FEwrong_type_argument(@'stream', strm);
	if (Null(recursivep))
		preserving_whitespace_flag = FALSE;
	detect_eos_flag = TRUE;
	if (Null(recursivep))
		x = read_object_non_recursive(strm);
	else
		x = read_object_recursive(strm);
	if (x == OBJNULL) {
		if (Null(eof_errorp) && Null(recursivep))
			@(return eof_value)
		FEend_of_file(strm);
	}
	@(return x)
@)

@(defun read_preserving_whitespace
	(&optional (strm symbol_value(@'*standard_input*'))
		   (eof_errorp Ct)
		   eof_value
		   recursivep
	 &aux x)
	cl_object c;
@
	if (Null(strm))
		strm = symbol_value(@'*standard_input*');
	else if (strm == Ct)
		strm = symbol_value(@'*terminal_io*');
RETRY:	if (type_of(strm) == t_stream) {
          if (strm->stream.mode == (short)smm_synonym) {
	    strm = symbol_value(strm->stream.object0);
	    goto RETRY;
	  }
	  else
	    read_ch_fun = readc;
	} else
#ifdef CLOS
	  if (type_of(strm) == t_instance)
	    read_ch_fun = interactive_readc;
	  else
#endif CLOS
	    FEwrong_type_argument(@'stream', strm);
	while (!stream_at_end(strm)) {
		c = read_char(strm);
		if (cat(c) != cat_whitespace) {
			unread_char(c, strm);
			goto READ;
		}
	}
	if (Null(eof_errorp) && Null(recursivep))
		@(return eof_value)
	FEend_of_file(strm);

READ:
	if (Null(recursivep))
		preserving_whitespace_flag = TRUE;
	if (Null(recursivep))
		x = read_object_non_recursive(strm);
	else
		x = read_object_recursive(strm);
	@(return x)
@)

@(defun read_delimited_list
	(d
	 &optional (strm symbol_value(@'*standard_input*'))
		   recursivep
	 &aux l x)

	cl_object *p;
	bool e;
	volatile cl_object old_sharp_eq_context;
	volatile int old_backq_level;

@
	if (!CHARACTERP(d))
		FEtype_error_character(d);
	if (Null(strm))
		strm = symbol_value(@'*standard_input*');
	else if (strm == Ct)
		strm = symbol_value(@'*terminal_io*');
	assert_type_stream(strm);
	if (Null(recursivep)) {
		old_sharp_eq_context = sharp_eq_context;
		old_backq_level = backq_level;
		setup_READ();
		sharp_eq_context = Cnil;
		backq_level = 0;
		if (frs_push(FRS_PROTECT, Cnil)) {
			e = TRUE;
			goto L;
		}
	}
	l = Cnil;
	p = &l;
	preserving_whitespace_flag = FALSE;	/*  necessary?  */
	for (;;) {
		delimiting_char = d;
		x = read_object_recursive(strm);
		if (x == OBJNULL)
			break;
		*p = CONS(x, Cnil);
		p = &(CDR((*p)));
	}
	if (Null(recursivep)) {
		if (!Null(sharp_eq_context))
			l = patch_sharp(l);
		e = FALSE;
	L:
		frs_pop();
		sharp_eq_context = old_sharp_eq_context;
		backq_level = old_backq_level;
		if (e) unwind(nlj_fr, nlj_tag);
	}
	@(return l)
@)

@(defun read_line (&optional (strm symbol_value(@'*standard_input*'))
			     (eof_errorp Ct)
			     eof_value
			     recursivep
		   &aux c)
	cl_index i;
@
	if (Null(strm))
		strm = symbol_value(@'*standard_input*');
	else if (strm == Ct)
		strm = symbol_value(@'*terminal_io*');
RETRY:	if (type_of(strm) == t_stream) {
          if (strm->stream.mode == (short)smm_synonym) {
	    strm = symbol_value(strm->stream.object0);
	    goto RETRY;
	  }
	  else {
	    if (stream_at_end(strm)) {
	      if (Null(eof_errorp) && Null(recursivep))
		@(return eof_value Ct)
	      else
		FEend_of_file(strm);
	    }
	    i = 0;
	    for (;;) {
	      c = read_char(strm);
	      if (char_code(c) == '\n') {
		c = Cnil;
		break;
	      }
	      if (i >= cl_token->string.dim)
		too_long_string();
	      cl_token->string.self[i++] = char_code(c);
	      if (stream_at_end(strm)) {
		c = Ct;
		break;
	      }
	    }
#ifdef CRLF
	    if (i > 0 && cl_token->string.self[i-1] == '\r') i--;
#endif CRLF
	    cl_token->string.fillp = i;
	    @(return copy_simple_string(cl_token) c)
	    }
	} else
#ifdef CLOS
	if (type_of(strm) == t_instance)
	     return funcall(2, @'stream_read_line', strm);
        else
#endif
	  FEerror("~S is not a stream.", 1, strm);
@)

@(defun read_char (&optional (strm symbol_value(@'*standard_input*'))
			     (eof_errorp Ct)
			     eof_value
			     recursivep)
@
        if (Null(strm))
	    strm = symbol_value(@'*standard_input*');
	else if (strm == Ct)
	    strm = symbol_value(@'*terminal_io*');
RETRY:	if (type_of(strm) == t_stream) {
          if (strm->stream.mode == (short)smm_synonym) {
	    strm = symbol_value(strm->stream.object0);
	    goto RETRY;
	  }
	  else {
	    if (stream_at_end(strm)) {
	      if (Null(eof_errorp) && Null(recursivep))
		@(return eof_value)
		  else
		    FEend_of_file(strm);
	    }
	    @(return read_char(strm))
	    }
	} else
#ifdef CLOS
	if (type_of(strm) == t_instance)
	     return funcall(2, @'stream_read_char', strm);
	else
#endif
	  FEerror("~S is not a stream.", 1, strm);
@)

@(defun unread_char (c &optional (strm symbol_value(@'*standard_input*')))
@
	/* INV: unread_char() checks the type `c' */
	if (Null(strm))
		strm = symbol_value(@'*standard_input*');
	else if (strm == Ct)
		strm = symbol_value(@'*terminal_io*');
RETRY:	if (type_of(strm) == t_stream) {
          if (strm->stream.mode == (short)smm_synonym) {
	    strm = symbol_value(strm->stream.object0);
	    goto RETRY;
	  }
	  else {
	    unread_char(c, strm);
	    @(return Cnil)
	    }
	} else
#ifdef CLOS
	if (type_of(strm) == t_instance)
	     return funcall(3, @'stream_unread_char', strm, c);
	else
#endif
	  FEerror("~S is not a stream.", 1, strm);
@)

@(defun peek_char (&optional peek_type
			     (strm symbol_value(@'*standard_input*'))
			     (eof_errorp Ct)
			     eof_value
			     recursivep)
	cl_object c;
@
	if (Null(strm))
		strm = symbol_value(@'*standard_input*');
	else if (strm == Ct)
		strm = symbol_value(@'*terminal_io*');
RETRY:	if (type_of(strm) == t_stream) {
          if (strm->stream.mode == (short)smm_synonym) {
	    strm = symbol_value(strm->stream.object0);
	    goto RETRY;
	  }
	  else {
	    setup_READtable();
	    if (Null(peek_type)) {
	      if (stream_at_end(strm)) {
		if (Null(eof_errorp) && Null(recursivep))
		  @(return eof_value)
		    else
		      FEend_of_file(strm);
	      }
	      c = read_char(strm);
	      unread_char(c, strm);
	      @(return c)
	      }
	    if (peek_type == Ct) {
	      while (!stream_at_end(strm)) {
		c = read_char(strm);
		if (cat(c) != cat_whitespace) {
		  unread_char(c, strm);
		  @(return c)
		  }
	      }
	      if (Null(eof_errorp))
		@(return eof_value)
		  else
		    FEend_of_file(strm);
	    }
	    /* INV: char_eq() checks the type of `peek_type' */
	    while (!stream_at_end(strm)) {
	      c = read_char(strm);
	      if (char_eq(c, peek_type)) {
		unread_char(c, strm);
		@(return c)
		}
	    }
	    if (Null(eof_errorp))
	      @(return eof_value)
		else
		  FEend_of_file(strm);
	  }
	} else
#ifdef CLOS
	if (type_of(strm) == t_instance)
	     return funcall(3, @'stream_peek_char', strm, peek_type);
	else
#endif
	  FEerror("~S is not a stream.", 1, strm);
@)

@(defun listen (&optional (strm symbol_value(@'*standard_input*')))
@
	if (Null(strm))
		strm = symbol_value(@'*standard_input*');
	else if (strm == Ct)
		strm = symbol_value(@'*terminal_io*');
RETRY:	if (type_of(strm) == t_stream) {
          if (strm->stream.mode == (short)smm_synonym) {
	    strm = symbol_value(strm->stream.object0);
	    goto RETRY;
	  }
	  else {
	    if (listen_stream(strm))
	      @(return Ct)
	    else
	      @(return Cnil)
	      }
	}
	else
#ifdef CLOS
	if (type_of(strm) == t_instance)
	     return funcall(2, @'stream_listen', strm);
	else
#endif
	  FEerror("~S is not a stream.", 1, strm);
@)

@(defun read_char_no_hang (&optional (strm symbol_value(@'*standard_input*'))
			             (eof_errorp Ct)
			             eof_value
			             recursivep)
@
	if (Null(strm))
		strm = symbol_value(@'*standard_input*');
	else if (strm == Ct)
		strm = symbol_value(@'*terminal_io*');
	assert_type_stream(strm);
#if 0
	if (!listen_stream(strm))
		/* Incomplete! */
		@(return Cnil)
	@(return read_char(strm))
#else
	/*
	  This implementation fails for EOF and handles
	  CLOS streams.
	*/
RETRY:	if (type_of(strm) == t_stream) {
	  if (strm->stream.mode == (short)smm_synonym) {
	    strm = symbol_value(strm->stream.object0);
	    goto RETRY;
	  } else {
	    if (listen_stream(strm))
	      @(return read_char(strm))
	    else if (!stream_at_end(strm))
	      @(return Cnil)
	    else if (Null(eof_errorp) && Null(recursivep))
	      @(return eof_value)
	    else
	      FEend_of_file(strm);
	  }
	}
	else
#ifdef CLOS
	/* FIXME! Is this all right? */
	if (type_of(strm) == t_instance) {
	  if (_funcall(2, @'stream_listen', strm) == Cnil)
	    @(return Cnil)
	  else
	    return funcall(2, @'stream_read_char', strm);
	} else
#endif
	  FEerror("~S is not a stream.", 1, strm);
#endif
@)

@(defun clear_input (&optional (strm symbol_value(@'*standard_input*')))
@
	if (Null(strm))
		strm = symbol_value(@'*standard_input*');
	else if (strm == Ct)
		strm = symbol_value(@'*terminal_io*');
RETRY:	if (type_of(strm) == t_stream) {
          if (strm->stream.mode == (short)smm_synonym) {
	    strm = symbol_value(strm->stream.object0);
	    goto RETRY;
	  }
	  else {
	    clear_input_stream(strm);
	    @(return Cnil)
	     }
	    } else
#ifdef CLOS
	if (type_of(strm) == t_instance)
	     return funcall(2, @'stream_clear_input', strm);
  	else
#endif
	  FEerror("~S is not a stream.", 1, strm);
@)

@(defun parse_integer (strng
		       &key (start MAKE_FIXNUM(0))
			    end
			    (radix MAKE_FIXNUM(10))
			    junk_allowed
		       &aux x)
	cl_index s, e, ep;
@
	assert_type_string(strng);
	get_string_start_end(strng, start, end, &s, &e);
	if (!FIXNUMP(radix) ||
	    fix(radix) < 2 || fix(radix) > 36)
		FEerror("~S is an illegal radix.", 1, radix);
	setup_READtable();
	while (READtable->readtable.table[strng->string.self[s]].syntax_type
	       == cat_whitespace && s < e)
		s++;
	if (s >= e) {
		if (junk_allowed != Cnil)
			@(return Cnil MAKE_FIXNUM(s))
		else
			goto CANNOT_PARSE;
	}
	x = parse_integer(strng->string.self+s, e-s, &ep, fix(radix));
	if (x == OBJNULL) {
		if (junk_allowed != Cnil)
			@(return Cnil MAKE_FIXNUM(ep+s))
		else
			goto CANNOT_PARSE;
	}
	if (junk_allowed != Cnil)
		@(return x MAKE_FIXNUM(ep+s))
	for (s += ep ;  s < e;  s++)
		if (READtable->readtable.table[strng->string.self[s]].syntax_type
		    != cat_whitespace)
			goto CANNOT_PARSE;
	@(return x MAKE_FIXNUM(e))

CANNOT_PARSE:
	FEerror("Cannot parse an integer in the string ~S.", 1, strng);
@)

@(defun read_byte (binary_input_stream
		   &optional eof_errorp eof_value)
	int c;
@
	assert_type_stream(binary_input_stream);
	if (stream_at_end(binary_input_stream)) {
		if (Null(eof_errorp))
			@(return eof_value)
		else
			FEend_of_file(binary_input_stream);
	}
	c = readc_stream(binary_input_stream);
	@(return MAKE_FIXNUM(c))
@)

@(defun si::read_bytes (stream string start end)
	int is, ie, c; FILE *fp;
@
	assert_type_stream(stream);
	if (stream->stream.mode == smm_closed)
	  closed_stream(stream);

	/* FIXME! this may fail! */
        is = fix(start);
	ie = fix(end);
	fp = stream->stream.file;
	if (fp == NULL) fp = stream->stream.object0->stream.file;
	c = fread (string->string.self + is, sizeof(unsigned char),
		   ie - is,
		   fp);
	@(return MAKE_FIXNUM(c))
@)



@(defun copy_readtable (&o (from current_readtable()) to)
@
	if (Null(from)) {
		from = standard_readtable;
		if (to != Cnil)
			assert_type_readtable(to);
		to = copy_readtable(from, to);
		to->readtable.table['#'].dispatch_table['!']
		= default_dispatch_macro;
		/*  We must forget #! macro.  */
		@(return to)
	}
	assert_type_readtable(from);
	if (to != Cnil)
		assert_type_readtable(to);
	@(return copy_readtable(from, to))
@)

@(defun readtablep (readtable)
@
	@(return ((type_of(readtable) == t_readtable)? Ct : Cnil))
@)

static struct readtable_entry*
read_table_entry(cl_object rdtbl, cl_object c)
{
	/* INV: char_code() checks the type of `c' */
	assert_type_readtable(rdtbl);
	return &(rdtbl->readtable.table[char_code(c)]);
}

@(defun set_syntax_from_char (tochr fromchr
			      &o (tordtbl current_readtable())
				 fromrdtbl)
	struct readtable_entry*torte, *fromrte;
@
	/* INV: read_table_entry() checks all values */
	if (Null(fromrdtbl))
		fromrdtbl = standard_readtable;
	/* INV: char_code() checks the types of `tochar',`fromchar' */
	torte = read_table_entry(tordtbl, tochr);
	fromrte = read_table_entry(fromrdtbl, fromchr);
	torte->syntax_type = fromrte->syntax_type;
	torte->macro = fromrte->macro;
	if ((torte->dispatch_table = fromrte->dispatch_table) != NULL) {
		size_t rtab_size = RTABSIZE * sizeof(cl_object);
		torte->dispatch_table = alloc(rtab_size);
		memcpy(torte->dispatch_table, fromrte->dispatch_table, rtab_size);
	}
	@(return Ct)
@)

@(defun set_macro_character (chr fnc
			     &optional ntp
				       (rdtbl current_readtable()))
	struct readtable_entry*entry;
@
	/* INV: read_table_entry() checks our arguments */
	entry = read_table_entry(rdtbl, chr);
	if (ntp != Cnil)
		entry->syntax_type = cat_non_terminating;
	else
		entry->syntax_type = cat_terminating;
	entry->macro = fnc;
	@(return Ct)
@)

@(defun get_macro_character (chr &o (rdtbl current_readtable()))
	struct readtable_entry*entry;
	cl_object m;
@

	/* fix to allow NIL as readtable argument. Beppe */
	if (Null(rdtbl))
		rdtbl = standard_readtable;
	/* INV: read_table_entry() checks our arguments */
	entry = read_table_entry(rdtbl, chr);
	m = entry->macro;
	if (m == OBJNULL)
		@(return Cnil)
	if (entry->syntax_type = cat_non_terminating)
		@(return m Ct)
	else
		@(return m Cnil)
@)

@(defun make_dispatch_macro_character (chr
	&optional ntp (rdtbl current_readtable()))
	struct readtable_entry*entry;
	cl_object *table;
	int i;
@
	/* INV: read_table_entry() checks our arguments */
	entry = read_table_entry(rdtbl, chr);
	if (ntp != Cnil)
		entry->syntax_type = cat_non_terminating;
	else
		entry->syntax_type = cat_terminating;
	table = alloc(RTABSIZE * sizeof(cl_object));
	entry->dispatch_table = table;
	for (i = 0;  i < RTABSIZE;  i++)
		table[i] = default_dispatch_macro;
	entry->macro = dispatch_reader;
	@(return Ct)
@)

@(defun set_dispatch_macro_character (dspchr subchr fnc
	&optional (rdtbl current_readtable()))
	struct readtable_entry*entry;
	cl_fixnum subcode;
@
	entry = read_table_entry(rdtbl, dspchr);
	if (entry->macro != dispatch_reader || entry->dispatch_table == NULL)
		FEerror("~S is not a dispatch character.", 1, dspchr);
	subcode = char_code(subchr);
	if ('a' <= subcode && subcode <= 'z')
		subcode = toupper(subcode);
	entry->dispatch_table[subcode] = fnc;
	@(return Ct)
@)

@(defun get_dispatch_macro_character (dspchr subchr
	&optional (rdtbl current_readtable()))
	struct readtable_entry*entry;
	cl_fixnum subcode;
@
	if (Null(rdtbl))
		rdtbl = standard_readtable;
	entry = read_table_entry(rdtbl, dspchr);
	if (entry->macro != dispatch_reader || entry->dispatch_table == NULL)
		FEerror("~S is not a dispatch character.", 1, dspchr);
	subcode = char_code(subchr);
	if (digitp(subcode, 10) >= 0)
		@(return Cnil)
	@(return entry->dispatch_table[subcode])
@)

cl_object
string_to_object(cl_object x)
{
	cl_object in;

	in = make_string_input_stream(x, 0, x->string.fillp);
	preserving_whitespace_flag = FALSE;
	detect_eos_flag = FALSE;
	x = read_object(in);
	return(x);
}

@(defun si::string_to_object (str)
@
	assert_type_string(str);
	@(return string_to_object(str))
@)

@(defun si::standard_readtable ()
@
	@(return standard_readtable)
@)

static void
too_long_token(void)
{
	char *q;

	q = alloc_atomic(cl_token->string.dim*2);
	memcpy(q, cl_token->string.self, cl_token->string.dim);
	cl_token->string.self = q;
	cl_token->string.dim *= 2;
}

static void
too_long_string(void)
{
	char *q;

	q = alloc_atomic(cl_token->string.dim*2);
	memcpy(q, cl_token->string.self, cl_token->string.dim);
	cl_token->string.self = q;
	cl_token->string.dim *= 2;
}

static void
extra_argument(int c, cl_object d)
{
	FEerror("~S is an extra argument for the #~C readmacro.",
		2, d, code_char(c));
}


#define	make_cf(f)	make_cfun((f), Cnil, NULL)

void
init_read(void)
{
	struct readtable_entry *rtab;
	cl_object *dtab;
	int i;

	standard_readtable = alloc_object(t_readtable);
	register_root(&standard_readtable);

	standard_readtable->readtable.table
	= rtab
	= alloc(RTABSIZE * sizeof(struct readtable_entry));
	for (i = 0;  i < RTABSIZE;  i++) {
		rtab[i].syntax_type = cat_constituent;
		rtab[i].macro = OBJNULL;
		rtab[i].dispatch_table = NULL;
	}

	dispatch_reader = make_cf(@si::dispatch_reader);
	register_root(&dispatch_reader);

	rtab['\t'].syntax_type = cat_whitespace;
	rtab['\n'].syntax_type = cat_whitespace;
	rtab['\f'].syntax_type = cat_whitespace;
	rtab['\r'].syntax_type = cat_whitespace;
	rtab[' '].syntax_type = cat_whitespace;
	rtab['"'].syntax_type = cat_terminating;
	rtab['"'].macro = make_cf(@si::double_quote_reader);
	rtab['#'].syntax_type = cat_non_terminating;
	rtab['#'].macro = dispatch_reader;
	rtab['\''].syntax_type = cat_terminating;
	rtab['\''].macro = make_cf(@si::single_quote_reader);
	rtab['('].syntax_type = cat_terminating;
	rtab['('].macro = make_cf(@si::left_parenthesis_reader);
	rtab[')'].syntax_type = cat_terminating;
	rtab[')'].macro = make_cf(@si::right_parenthesis_reader);
/*
	rtab[','].syntax_type = cat_terminating;
	rtab[','].macro = make_cf(@si::comma_reader);
*/
	rtab[';'].syntax_type = cat_terminating;
	rtab[';'].macro = make_cf(@si::semicolon_reader);
	rtab['\\'].syntax_type = cat_single_escape;
/*
	rtab['`'].syntax_type = cat_terminating;
	rtab['`'].macro = make_cf(@si::backquote_reader);
*/
	rtab['|'].syntax_type = cat_multiple_escape;
/*
	rtab['|'].macro = make_cf(@si::vertical_bar_reader);
*/

	default_dispatch_macro = make_cf(@si::default_dispatch_macro);
#ifndef THREADS
	register_root(&default_dispatch_macro);
#endif

	rtab['#'].dispatch_table
	= dtab
	= alloc(RTABSIZE * sizeof(cl_object));
	for (i = 0;  i < RTABSIZE;  i++)
		dtab[i] = default_dispatch_macro;
	dtab['C'] = dtab['c'] = make_cf(@si::sharp_C_reader);
	dtab['\\'] = make_cf(@si::sharp_backslash_reader);
	dtab['\''] = make_cf(@si::sharp_single_quote_reader);
	dtab['('] = make_cf(@si::sharp_left_parenthesis_reader);
	dtab['*'] = make_cf(@si::sharp_asterisk_reader);
	dtab[':'] = make_cf(@si::sharp_colon_reader);
	dtab['.'] = make_cf(@si::sharp_dot_reader);
	dtab['!'] = make_cf(@si::sharp_exclamation_reader);
	/*  Used for fasload only. */
	dtab[','] = make_cf(@si::sharp_comma_reader);
	dtab['B'] = dtab['b'] = make_cf(@si::sharp_B_reader);
	dtab['O'] = dtab['o'] = make_cf(@si::sharp_O_reader);
	dtab['X'] = dtab['x'] = make_cf(@si::sharp_X_reader);
	dtab['R'] = dtab['r'] = make_cf(@si::sharp_R_reader);
/*
	dtab['A'] = dtab['a'] = make_cf(@si::sharp_A_reader);
	dtab['S'] = dtab['s'] = make_cf(@si::sharp_S_reader);
*/
	dtab['A'] = dtab['a'] = make_si_ordinary("SHARP-A-READER");
	dtab['S'] = dtab['s'] = make_si_ordinary("SHARP-S-READER");
	dtab['P'] = dtab['p'] = make_cf(@si::sharp_P_reader);

	dtab['='] = make_cf(@si::sharp_eq_reader);
	dtab['#'] = make_cf(@si::sharp_sharp_reader);
	dtab['+'] = make_cf(@si::sharp_plus_reader);
	dtab['-'] = make_cf(@si::sharp_minus_reader);
/*
	dtab['<'] = make_cf(@si::sharp_less_than_reader);
*/
	dtab['|'] = make_cf(@si::sharp_vertical_bar_reader);
	dtab['"'] = make_cf(@si::sharp_double_quote_reader);
	/*  This is specific to this implementation  */
	dtab['$'] = make_cf(@si::sharp_dollar_reader);
	/*  This is specific to this implimentation  */
/*
	dtab[' '] = dtab['\t'] = dtab['\n'] = dtab['\f']
	= make_cf(@si::sharp_whitespace_reader);
	dtab[')'] = make_cf(@si::sharp_right_parenthesis_reader);
*/

	init_backq();

	SYM_VAL(@'*readtable*') =
	  copy_readtable(standard_readtable, Cnil);
	SYM_VAL(@'*readtable*')->readtable.table['#'].dispatch_table['!']
	  = default_dispatch_macro; /*  We must forget #! macro.  */
	SYM_VAL(@'*read_default_float_format*')
	  = @'single-float';
	SYM_VAL(@'*read_base*') = MAKE_FIXNUM(10);
	SYM_VAL(@'*read_suppress*') = Cnil;

	READtable = symbol_value(@'*readtable*');
	register_root(&READtable);
	READdefault_float_format = 'S';
	READbase = 10;
	READsuppress = FALSE;

	sharp_eq_context = Cnil;

	delimiting_char = OBJNULL;
	register_root(&delimiting_char);

	detect_eos_flag = FALSE;
	in_list_flag = FALSE;
	dot_flag = FALSE;

	read_ch_fun = readc;
}

/*
 *----------------------------------------------------------------------
 *
 * read_VV --
 *     reads the data vector from stream into vector VV
 *
 * Results:
 *	a vector.
 *
 *----------------------------------------------------------------------
 */
void
read_VV(cl_object block, void *entry)
{
	typedef void (*entry_point_ptr)(cl_object);
	volatile cl_object x, v;
	int i;
	bool e;
	cl_object in;
	cl_object old_READtable;
	int old_READdefault_float_format;
	int old_READbase;
	int old_READsuppress;
	int old_backq_level;
	cl_object old_sharp_eq_context;
	cl_object old_package;

	entry_point_ptr entry_point = entry;
	cl_object *VV;
	int len;
#ifdef PDE
	bds_ptr old_bds_top = bds_top;
#endif

	if (block == NULL)
		block = alloc_object(t_codeblock);

	(*entry_point)(block);
	VV = block->cblock.data;
	len = block->cblock.data_size;

	old_READtable = READtable;
	old_READdefault_float_format = READdefault_float_format;
	old_READbase = READbase;
	old_READsuppress = READsuppress;
	old_sharp_eq_context = sharp_eq_context;
	old_backq_level = backq_level;

	old_package = SYM_VAL(@'*package*');
	SYM_VAL(@'*package*') = lisp_package;

	setup_standard_READ();

	in = make_string_input_stream(make_simple_string(block->cblock.data_text),
				      0, block->cblock.data_text_size);
	if (frs_push(FRS_PROTECT, Cnil))
		e = TRUE;
	else {
	  read_VV_block = block;
	  for (i = 0 ; i < len; i++) {
	    sharp_eq_context = Cnil;
	    backq_level = 0;
	    preserving_whitespace_flag = FALSE;
	    detect_eos_flag = FALSE;
	    x = read_object(in);
	    if (x == OBJNULL)
	      break;
	    if (!Null(sharp_eq_context))
	      x = patch_sharp(x);
	    VV[i] = x;
	  }
	  if (i < len)
	    FEerror("Not enough data while loading binary file",0);
	  SYM_VAL(@'*package*') = old_package;
#ifdef PDE
	  bds_bind(@'si::*source-pathname*', VV[block->cblock.source_pathname]);
#endif
	  (*entry_point)(MAKE_FIXNUM(0));
	  e = FALSE;
	}

	frs_pop();
	close_stream(in, 0);

	read_VV_block = OBJNULL;
#ifdef PDE
	bds_unwind(old_bds_top);
#endif

	READtable = old_READtable;
	READdefault_float_format = old_READdefault_float_format;
	READbase = old_READbase;
	READsuppress = old_READsuppress;
	sharp_eq_context = old_sharp_eq_context;
	backq_level = old_backq_level;
	if (e) unwind(nlj_fr, nlj_tag);
}

