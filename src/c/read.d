/*
    read.d -- Read.
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

#include <math.h>
#include <ctype.h>
#include "ecl.h"
#include "ecl-inl.h"

#ifndef CHAR_BIT
#define CHAR_BIT (sizeof(char)*8)
#endif

/******************************* EXPORTS ******************************/

cl_object standard_readtable;

#ifndef THREADS
bool preserving_whitespace_flag;
bool escape_flag;
cl_object delimiting_char;
bool detect_eos_flag;
cl_object sharp_eq_context;
#endif /* THREADS */

/******************************* ------- ******************************/

#ifndef THREADS
extern int backq_level;
#else
#define backq_level lwp->lwp_backq_level
#endif

static cl_object dispatch_reader;
static cl_object default_dispatch_macro;

#define	cat(rtbl,c)	((rtbl)->readtable.table[c].syntax_type)
#define read_suppress (SYM_VAL(@'*read-suppress*') != Cnil)

static void extra_argument (int c, cl_object d);

cl_object
read_char(cl_object stream)
{
	return CODE_CHAR(readc_stream(stream));
}

void
unread_char(cl_object c, cl_object in)
{
	/* INV: char_code() checks the type of `c' */
	unreadc_stream(char_code(c), in);
}

/*
	peek_char corresponds to COMMON Lisp function PEEK-CHAR.
	When pt is TRUE, preceeding whitespaces are ignored.
*/
cl_object
peek_char(bool pt, cl_object in)
{
	int c;
	cl_object rtbl = cl_current_readtable();

	c = readc_stream(in);
	if (pt)
		while (cat(rtbl, c) == cat_whitespace)
			c = readc_stream(in);
	unreadc_stream(c, in);
	return CODE_CHAR(c);
}

static cl_object patch_sharp(cl_object x);

cl_object
read_object_non_recursive(cl_object in)
{
	volatile cl_object x;
	bool e;
	int old_backq_level;
	cl_object old_sharp_eq_context;

	old_sharp_eq_context = sharp_eq_context;
	old_backq_level = backq_level;
	sharp_eq_context = Cnil;
	backq_level = 0;

	if (frs_push(FRS_PROTECT, Cnil))
		e = TRUE;
	else {
		x = read_object(in);
		if (!Null(sharp_eq_context))
			x = patch_sharp(x);
		e = FALSE;
	}
	frs_pop();

	sharp_eq_context = old_sharp_eq_context;
	backq_level = old_backq_level;
	if (e) unwind(nlj_fr, nlj_tag);
	return(x);
}

/*
	Read_object(in) reads an object from stream in.
	This routine corresponds to COMMON Lisp function READ.
*/
cl_object
read_object(cl_object in)
{
	cl_object x;
	int c, base;
	enum chattrib a;
	cl_object old_delimiter, p;
	cl_index length, i, colon;
	int colon_type, intern_flag;
	bool df;
	cl_object rtbl = cl_current_readtable();

	cs_check(in);

	old_delimiter = delimiting_char;
	delimiting_char = OBJNULL;
	df = detect_eos_flag;
	detect_eos_flag = FALSE;

BEGIN:
	/* Beppe: */
	do {
		if (stream_at_end(in)) {
			if (df)
				return(OBJNULL);
			else
				FEend_of_file(in);
		} else {
			c = readc_stream(in);
		}
		a = cat(rtbl, c);
	} while (a == cat_whitespace);
	delimiting_char = OBJNULL;
	if (old_delimiter != OBJNULL && old_delimiter == CODE_CHAR(c))
		return(OBJNULL);
	if (a == cat_terminating || a == cat_non_terminating) {
		cl_object x = rtbl->readtable.table[c].macro;
		cl_object o = funcall(3, x, in, CODE_CHAR(c));
		if (NValues == 0) goto BEGIN;
		if (NValues > 1) FEerror("The readmacro ~S returned ~D values.",
					 2, x, MAKE_FIXNUM(i));
		return o;
	}
	escape_flag = FALSE;
	length = 0;
	colon_type = 0;
	cl_token->string.fillp = 0;
	for (;;) {
		if (a == cat_single_escape) {
			c = readc_stream(in);
			a = cat_constituent;
			escape_flag = TRUE;
		} else if (a == cat_multiple_escape) {
			escape_flag = TRUE;
			for (;;) {
				if (stream_at_end(in))
					FEend_of_file(in);
				c = readc_stream(in);
				a = cat(rtbl, c);
				if (a == cat_single_escape) {
					c = readc_stream(in);
					a = cat_constituent;
				} else if (a == cat_multiple_escape)
					break;
				cl_string_push_extend(cl_token, c);
				length++;
			}
			goto NEXT;
		} else if (islower(c))
			c = toupper(c);
		else if (c == ':') {
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
			    cat(rtbl, c) != cat_whitespace)
				unreadc_stream(c, in);
			break;
		}
		cl_string_push_extend(cl_token, c);
		length++;
	NEXT:
		if (stream_at_end(in))
			break;
		else
			c = readc_stream(in);
		a = cat(rtbl, c);
	}

	if (read_suppress)
		return(Cnil);

	if (!escape_flag && length == 1 && cl_token->string.self[0] == '.') {
		return @'si::.';
	} else if (!escape_flag && length > 0) {
		for (i = 0;  i < length;  i++)
			if (cl_token->string.self[i] != '.')
				goto N;
		FEerror("Dots appeared illegally.", 0);
	}

N:
	base = cl_current_read_base();
	if (escape_flag || (base <= 10 && isalpha(cl_token->string.self[0])))
		goto SYMBOL;
	x = parse_number(cl_token->string.self, cl_token->string.fillp, &i, base);
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
		cl_token->string.fillp = length - (colon + 1);
		memmove(cl_token->string.self,
			cl_token->string.self + colon + 1,
			sizeof(*cl_token->string.self) * cl_token->string.fillp);
		if (colon > 0) {
			cl_token->string.self[cl_token->string.fillp] = '\0';
			x = find_symbol(cl_token, p, &intern_flag);
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
		cl_token->string.fillp = length - (colon + 2);
		memmove(cl_token->string.self,
			cl_token->string.self + colon + 2,
			sizeof(*cl_token->string.self) * cl_token->string.fillp);
	} else
		p = current_package();
	cl_token->string.self[cl_token->string.fillp] = '\0';
	x = intern(cl_token, p, &intern_flag);
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
parse_number(const char *s, cl_index end, cl_index *ep, int radix)
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
	  double po = pow(10.0, (double)(sign * d));
          if (po == 0.0) {
	    fraction *= pow(10.0, (double)(sign * (d-1)));
	    fraction /= 10.0;
	  } else     
	    fraction *= po;
	}

MAKE_FLOAT:
	/* make_{short|long}float signals an error when an overflow
	   occurred while reading the number. Thus, no safety check
	   is required here. */
	switch (exponent_marker) {

	case 'e':  case 'E':
		exponent_marker = cl_current_read_default_float_format();
		goto MAKE_FLOAT;

	case 's':  case 'S':
		x = make_shortfloat((float)fraction);
		break;

	case 'f':  case 'F':  case 'd':  case 'D':  case 'l':  case 'L':
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
parse_integer(const char *s, cl_index end, cl_index *ep, int radix)
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
@(defun "left_parenthesis_reader" (in character)
	cl_object x, y;
	cl_object *p;
	int c;
	cl_object rtbl = cl_current_readtable();
@
	y = Cnil;
	for (p = &y ; ; p = &(CDR(*p))) {
		delimiting_char = CODE_CHAR(')');
		x = read_object(in);
		if (x == OBJNULL)
			break;
		if (x == @'si::.') {
			if (p == &y)
				FEerror("A dot appeared after a left parenthesis.", 0);
			*p = read_object(in);
			if (*p == @'si::.')
				FEerror("Two dots appeared consecutively.", 0);
			c = readc_stream(in);
			while (cat(rtbl, c) == cat_whitespace)
				c = readc_stream(in);
			if (c != ')')
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
	int c;
	cl_object rtbl = cl_current_readtable();

	cl_token->string.fillp = 0;
	for (;;) {
		c = readc_stream(in);
		if (c == delim)
			break;
		else if (cat(rtbl, c) == cat_single_escape)
			c = readc_stream(in);
		cl_string_push_extend(cl_token, c);
	}
}

/*
	Read_constituent(in) reads
	a sequence of constituent characters from stream in
	and places it in cl_token.
*/
static void
read_constituent(cl_object in)
{
	int c;
	cl_object rtbl = cl_current_readtable();

	cl_token->string.fillp = 0;
	for (;;) {
		c = readc_stream(in);
		if (cat(rtbl, c) != cat_constituent) {
			unreadc_stream(c, in);
			break;
		}
		cl_string_push_extend(cl_token, c);
	}
}

static
@(defun "double_quote_reader" (in c)
@
	read_string('"', in);
	@(return copy_simple_string(cl_token))
@)

static
@(defun "dispatch_reader_fun" (in dc)
	cl_object x, y;
	int i, d, c;
	cl_object rtbl = cl_current_readtable();
@
	if (rtbl->readtable.table[char_code(dc)].dispatch_table == NULL)
		FEerror("~C is not a dispatching macro character", 1, dc);

	c = readc_stream(in);
	d = digitp(c, 10);
	if (d >= 0) {
		i = 0;
		do {
			i = 10*i + d;
			c = readc_stream(in);
			d = digitp(c, 10);
		} while (d >= 0);
		y = MAKE_FIXNUM(i);
	} else
		y = Cnil;

	x = rtbl->readtable.table[char_code(dc)].dispatch_table[c];
	return funcall(4, x, in, CODE_CHAR(c), y);
@)

static
@(defun "single_quote_reader" (in c)
@
	@(return CONS(@'quote', CONS(read_object(in), Cnil)))
@)

static
@(defun "void_reader" (in c)
@
	/*  no result  */
	@(return)
@)

#define right_parenthesis_reader void_reader

static
@(defun "semicolon_reader" (in c)
	int auxc;
@
	do
		auxc = readc_stream(in);
	while (auxc != '\n');
	/*  no result  */
	@(return)
@)

/*
	sharpmacro routines
*/

static
@(defun "sharp_C_reader" (in c d)
	cl_object x, real, imag;
@
	if (d != Cnil && !read_suppress)
		extra_argument('C', d);
	if (readc_stream(in) != '(')
		FEerror("A left parenthesis is expected.", 0);
	delimiting_char = CODE_CHAR(')');
	real = read_object(in);
	if (real == OBJNULL)
		FEerror("No real part.", 0);
	delimiting_char = CODE_CHAR(')');
	imag = read_object(in);
	if (imag == OBJNULL)
		FEerror("No imaginary part.", 0);
	delimiting_char = CODE_CHAR(')');
	x = read_object(in);
	if (x != OBJNULL)
		FEerror("A right parenthesis is expected.", 0);
	if (read_suppress)
		@(return Cnil)
	/* INV: make_complex() checks its types */
	x = make_complex(real, imag);
	@(return x)
@)

static
@(defun "sharp_backslash_reader" (in c d)
@
	if (d != Cnil && !read_suppress)
		if (!FIXNUMP(d) ||
		    fix(d) != 0)
			FEerror("~S is an illegal CHAR-FONT.", 1, d);
			/*  assuming that CHAR-FONT-LIMIT is 1  */
	unreadc_stream('\\', in);
	if (read_suppress) {
		(void)read_object(in);
		@(return Cnil)
	}
	SYM_VAL(@'*read-suppress*') = Ct;
	(void)read_object(in);
	SYM_VAL(@'*read-suppress*') = Cnil;
	c = cl_token;
	if (c->string.fillp == 1)
		c = CODE_CHAR(c->string.self[0]);
	/*	#\^x	*/
	else if (c->string.fillp == 2 && c->string.self[0] == '^')
		c = CODE_CHAR(c->string.self[1] & 037);
	else if (c->string.self[0] =='\\' && c->string.fillp > 1) {
		cl_index i, n;
		for (n = 0, i = 1;  i < c->string.fillp;  i++)
			if (c->string.self[i] < '0' ||
			    '7' < c->string.self[i])
				FEerror("Octal digit expected.", 0);
			else
				n = 8*n + c->string.self[i] - '0';
		c = CODE_CHAR(n & 0377);
	} else {
		c = cl_name_char(c);
		if (Null(c)) FEerror("~S is an illegal character name.", 1, c);
	}
	@(return c)
@)

static
@(defun "sharp_single_quote_reader" (in c d)
@
	if(d != Cnil && !read_suppress)
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

static
@(defun "sharp_left_parenthesis_reader" (in c d)
	bool fixed_size;
	cl_index dim, dimcount, i, a;
	cl_index sp = cl_stack_index();
	cl_object x, last;
	extern _cl_backq_car(cl_object *);
@
	if (Null(d) || read_suppress)
		fixed_size = FALSE;
	else {
		fixed_size = TRUE;
		dim = fixnnint(d);
	}
	if (backq_level > 0) {
		unreadc_stream('(', in);
		x = read_object(in);
		a = _cl_backq_car(&x);
		if (a == APPEND || a == NCONC)
		  FEerror(",at or ,. has appeared in an illegal position.", 0);
		if (a == QUOTE) {
		  for (dimcount = 0;  !endp(x);  x = CDR(x), dimcount++)
		    cl_stack_push(CAR(x));
		  goto L;
		}
		@(return cl_list(4, @'si::,', @'apply',
				 CONS(@'quote', CONS(@'vector', Cnil)), x))
	}
	for (dimcount = 0 ;; dimcount++) {
		delimiting_char = CODE_CHAR(')');
		x = read_object(in);
		if (x == OBJNULL)
			break;
		cl_stack_push(x);
	}
L:
	if (fixed_size) {
		if (dimcount > dim)
			FEerror("Too many elements in #(...).", 0);
		if (dimcount == 0)
			FEerror("Cannot fill the vector #().", 0);
		else last = cl_stack_top[-1];
	} else
		dim = dimcount;
	x = cl_alloc_simple_vector(dim, aet_object);
	x->vector.self.t = (cl_object *)cl_alloc_align(dim * sizeof(cl_object), sizeof(cl_object));
	for (i = 0; i < dim; i++)
		x->vector.self.t[i] = (i < dimcount) ? cl_stack[sp+i] : last;
	cl_stack_pop_n(dimcount);
	@(return x)
@)

static
@(defun "sharp_asterisk_reader" (in c d)
	bool fixed_size;
	cl_index dim, dimcount, i;
	cl_index sp = cl_stack_index();
	cl_object last, elt, x;
@
	if (read_suppress) {
		read_constituent(in);
		@(return Cnil)
	}
	if (Null(d))
		fixed_size = FALSE;
	else {
		dim = fixnnint(d);
		fixed_size = TRUE;
	}
	for (dimcount = 0 ;; dimcount++) {
	 	int x;
		if (stream_at_end(in))
			break;
		x = readc_stream(in);
		if (x != '0' && x != '1') {
			unreadc_stream(x, in);
			break;
		} else {
			cl_stack_push(MAKE_FIXNUM(x == '1'));
		}
	}	
	if (fixed_size) {
		if (dimcount > dim)
			FEerror("Too many elements in #*....", 0);
		if (dimcount == 0)
			FEerror("Cannot fill the bit-vector #*.", 0);
		else last = cl_stack_top[-1];
	} else {
		dim = dimcount;
	}
	x = cl_alloc_simple_bitvector(dim);
	x->vector.self.bit = (byte *)cl_alloc_atomic((dim + CHAR_BIT - 1)/CHAR_BIT);
	for (i = 0; i < dim; i++) {
		elt = (i < dimcount) ? cl_stack[sp+i] : last;
		if (elt == MAKE_FIXNUM(0))
			x->vector.self.bit[i/CHAR_BIT] &= ~(0200 >> i%CHAR_BIT);
		else
			x->vector.self.bit[i/CHAR_BIT] |= 0200 >> i%CHAR_BIT;
	}
	cl_stack_pop_n(dimcount);
	@(return x)
@)

static
@(defun "sharp_colon_reader" (in ch d)
	cl_object rtbl = cl_current_readtable();
	enum chattrib a;
	int c;
@
	if (d != Cnil && !read_suppress)
		extra_argument(':', d);
	c = readc_stream(in);
	a = cat(rtbl, c);
	escape_flag = FALSE;
	cl_token->string.fillp = 0;
	goto L;
	for (;;) {
		cl_string_push_extend(cl_token, c);
	K:
		if (stream_at_end(in))
			goto M;
		c = readc_stream(in);
		a = cat(rtbl, c);
	L:
		if (a == cat_single_escape) {
			c = readc_stream(in);
			a = cat_constituent;
			escape_flag = TRUE;
		} else if (a == cat_multiple_escape) {
			escape_flag = TRUE;
			for (;;) {
				if (stream_at_end(in))
					FEend_of_file(in);
				c = readc_stream(in);
				a = cat(rtbl, c);
				if (a == cat_single_escape) {
					c = readc_stream(in);
					a = cat_constituent;
				} else if (a == cat_multiple_escape)
					break;
				cl_string_push_extend(cl_token, c);
			}
			goto K;
		} else if (islower(c))
			c = toupper(c);
		if (a == cat_whitespace || a == cat_terminating)
			break;
	}
	if (preserving_whitespace_flag || cat(rtbl, c) != cat_whitespace)
		unreadc_stream(c, in);

M:
	if (read_suppress)
		@(return Cnil)
	@(return make_symbol(copy_simple_string(cl_token)))
@)

static
@(defun "sharp_dot_reader" (in c d)
@
	if(d != Cnil && !read_suppress)
		extra_argument('.', d);
	in = read_object(in);
	if (read_suppress)
		@(return Cnil)
	in = eval(in, NULL, Cnil);
	@(return in)
@)

/*
	For fasload.
*/
static
@(defun "sharp_exclamation_reader" (in c d)
	cl_fixnum code;
@
	if(d != Cnil && !read_suppress)
		extra_argument('!', d);
	if (read_suppress)
		@(return Cnil)
	code = fixint(read_object(in));
	switch (code) {
	case 0: {
		cl_object name = read_object(in);
		si_select_package(name);
		break;
	}
	case 1: {
		cl_object name = read_object(in);
		cl_object p = find_package(name);
		if (Null(p)) make_package(name,Cnil,Cnil);
		break;
	}
	default: {
		cl_object read_VV_block = SYM_VAL(@'si::*cblock*');
		code = -code - 1;
		if (code < 0 || code >= read_VV_block->cblock.data_size)
			FEerror("Bogus binary file. #!~S unknown.",1,
				MAKE_FIXNUM(code));
		@(return read_VV_block->cblock.data[code])
	}
	}
	@(return)
@)

static
@(defun "sharp_B_reader" (in c d)
	cl_index i;
	cl_object x;
@
	if(d != Cnil && !read_suppress)
		extra_argument('B', d);
	read_constituent(in);
	if (read_suppress)
		@(return Cnil)
	x = parse_number(cl_token->string.self, cl_token->string.fillp, &i, 2);
	if (x == OBJNULL || i != cl_token->string.fillp)
		FEerror("Cannot parse the #B readmacro.", 0);
	if (type_of(x) == t_shortfloat ||
	    type_of(x) == t_longfloat)
		FEerror("The float ~S appeared after the #B readmacro.",
			1, x);
	@(return x)
@)

static
@(defun "sharp_O_reader" (in c d)
	cl_index i;
	cl_object x;
@
	if(d != Cnil && !read_suppress)
		extra_argument('O', d);
	read_constituent(in);
	if (read_suppress)
		@(return Cnil)
	x = parse_number(cl_token->string.self, cl_token->string.fillp, &i, 8);
	if (x == OBJNULL || i != cl_token->string.fillp)
		FEerror("Cannot parse the #O readmacro.", 0);
	if (type_of(x) == t_shortfloat ||
	    type_of(x) == t_longfloat)
		FEerror("The float ~S appeared after the #O readmacro.",
			1, x);
	@(return x)
@)

static
@(defun "sharp_X_reader" (in c d)
	cl_index i;
	cl_object x;
@
	if(d != Cnil && !read_suppress)
		extra_argument('X', d);
	read_constituent(in);
	if (read_suppress)
		@(return Cnil)
	x = parse_number(cl_token->string.self, cl_token->string.fillp, &i, 16);
	if (x == OBJNULL || i != cl_token->string.fillp)
		FEerror("Cannot parse the #X readmacro.", 0);
	if (type_of(x) == t_shortfloat ||
	    type_of(x) == t_longfloat)
		FEerror("The float ~S appeared after the #X readmacro.",
			1, x);
	@(return x)
@)

static
@(defun "sharp_R_reader" (in c d)
	int radix;
	cl_index i;
	cl_object x;
@
	if (read_suppress)
		radix = 10;
	else if (FIXNUMP(d)) {
		radix = fix(d);
		if (radix > 36 || radix < 2)
			FEerror("~S is an illegal radix.", 1, d);
	} else
		FEerror("No radix was supplied in the #R readmacro.", 0);
	read_constituent(in);
	if (read_suppress)
		@(return Cnil)
	x = parse_number(cl_token->string.self, cl_token->string.fillp, &i, radix);
	if (x == OBJNULL || i != cl_token->string.fillp)
		FEerror("Cannot parse the #R readmacro.", 0);
	if (type_of(x) == t_shortfloat ||
	    type_of(x) == t_longfloat)
		FEerror("The float ~S appeared after the #R readmacro.",
			1, x);
	@(return x)
@)

#define sharp_A_reader void_reader
#define sharp_S_reader void_reader

static
@(defun "sharp_eq_reader" (in c d)
	cl_object pair, value;
@
	if (read_suppress) @(return)
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
@(defun "sharp_sharp_reader" (in c d)
	cl_object pair;
@
	if (read_suppress) @(return)
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

#define sharp_plus_reader void_reader
#define sharp_minus_reader void_reader
#define sharp_less_than_reader void_reader
#define sharp_whitespace_reader void_reader
#define sharp_right_parenthesis_reader void_reader

static
@(defun "sharp_vertical_bar_reader" (in ch d)
	int c;
	int level = 0;
@
	if (d != Cnil && !read_suppress)
		extra_argument('|', d);
	for (;;) {
		c = readc_stream(in);
	L:
		if (c == '#') {
			c = readc_stream(in);
			if (c == '|')
				level++;
		} else if (c == '|') {
			c = readc_stream(in);
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
@(defun "default_dispatch_macro_fun" (in c d)
@
	FEerror("Undefined dispatch macro character.", 1, c);
@)

/*
	#P" ... " returns the pathname with namestring ... .
*/
static
@(defun "sharp_P_reader" (in c d)
@
	@(return cl_pathname(read_object(in)))
@)

/*
	#" ... " returns the pathname with namestring ... .
*/
static
@(defun "sharp_double_quote_reader" (in c d)
@
	if (d != Cnil && !read_suppress)
		extra_argument('"', d);
	unread_char(c, in);
	@(return cl_pathname(read_object(in)))
@)

/*
	#$ fixnum returns a random-state with the fixnum
	as its content.
*/
static
@(defun "sharp_dollar_reader" (in c d)
	cl_object output;
@
	if (d != Cnil && !read_suppress)
		extra_argument('$', d);
	c = read_object(in);
	if (!FIXNUMP(c))
		FEerror("Cannot make a random-state with the value ~S.",
			1, c);
	output = cl_alloc_object(t_random);
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
		to = cl_alloc_object(t_readtable);
		to->readtable.table = NULL;
			/*  Saving for GC.  */
		to->readtable.table
		= rtab
 		= (struct readtable_entry *)cl_alloc_align(RTABSIZE * sizeof(struct readtable_entry), sizeof(struct readtable_entry));
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
 			= (cl_object *)cl_alloc_align(RTABSIZE * sizeof(cl_object), sizeof(cl_object));
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
cl_current_readtable(void)
{
	cl_object r;

	/* INV: *readtable* always has a value */
	r = SYM_VAL(@'*readtable*');
	if (type_of(r) != t_readtable) {
		SYM_VAL(@'*readtable*') = copy_readtable(standard_readtable, Cnil);
		FEerror("The value of *READTABLE*, ~S, was not a readtable.",
			1, r);
	}
	return(r);
}

int
cl_current_read_base(void)
{
	cl_object x;

	/* INV: *READ-BASE* always has a value */
	x = SYM_VAL(@'*read_base*');
	if (FIXNUMP(x)) {
		cl_fixnum b = fix(x);
		if (b >= 2 && b <= 36)
			return b;
	}
	SYM_VAL(@'*read_base*') = MAKE_FIXNUM(10);
	FEerror("The value of *READ-BASE*, ~S, was illegal.", 1, x);
}

char
cl_current_read_default_float_format(void)
{
	cl_object x;

	/* INV: *READ-DEFAULT-FLOAT-FORMAT* is always bound to something */
	x = SYM_VAL(@'*read-default-float-format*');
	if (x == @'single-float' || x == @'short-float')
		return 'S';
	if (x == @'double-float' || x == @'long-float')
		return 'F';
	SYM_VAL(@'*read-default-float-format*') = @'single-float';
	FEerror("The value of *READ-DEFAULT-FLOAT-FORMAT*, ~S, was illegal.",
		1, x);
}
	


static cl_object
stream_or_default_input(cl_object stream)
{
	if (Null(stream))
		return SYM_VAL(@'*standard-input*');
	if (stream == Ct)
		return SYM_VAL(@'*terminal-io*');
	return stream;
}

@(defun read (&optional (strm Cnil)
			(eof_errorp Ct)
			eof_value
			recursivep
	      &aux x)
@
	strm = stream_or_default_input(strm);
	detect_eos_flag = TRUE;
	if (Null(recursivep)) {
		preserving_whitespace_flag = FALSE;
		x = read_object_non_recursive(strm);
	} else {
		x = read_object(strm);
	}
	if (x == OBJNULL) {
		if (Null(eof_errorp))
			@(return eof_value)
		FEend_of_file(strm);
	}
	@(return x)
@)

@(defun read_preserving_whitespace
	(&optional (strm Cnil)
		   (eof_errorp Ct)
		   eof_value
		   recursivep)
	cl_object x, rtbl = cl_current_readtable();
	int c;
@
	strm = stream_or_default_input(strm);
	detect_eos_flag = TRUE;
	if (Null(recursivep)) {
		preserving_whitespace_flag = TRUE;
		x = read_object_non_recursive(strm);
	} else {
		x = read_object(strm);
	}
	if (x == OBJNULL) {
		if (Null(eof_errorp))
			@(return eof_value)
		FEend_of_file(strm);
	}
	@(return x)
@)

@(defun read_delimited_list (d &optional (strm Cnil) recursivep &aux l x)
	cl_object *p;
	bool e;
	volatile cl_object old_sharp_eq_context;
	volatile int old_backq_level;

@
	if (!CHARACTERP(d))
		FEtype_error_character(d);
	strm = stream_or_default_input(strm);
	if (Null(recursivep)) {
		old_sharp_eq_context = sharp_eq_context;
		old_backq_level = backq_level;
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
		x = read_object(strm);
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

@(defun read_line (&optional (strm Cnil) (eof_errorp Ct) eof_value recursivep)
	int c;
	cl_object eof;
@
	strm = stream_or_default_input(strm);
	if (stream_at_end(strm)) {
		if (Null(eof_errorp) && Null(recursivep))
			@(return eof_value Ct)
		else
			FEend_of_file(strm);
	}
	cl_token->string.fillp = 0;
	for (;;) {
		c = readc_stream(strm);
		if (c == '\n') {
			eof = Cnil;
			break;
		}
		cl_string_push_extend(cl_token, c);
		if (stream_at_end(strm)) {
			eof = Ct;
			break;
		}
	}
#ifdef CRLF
	if (cl_token->string.fillp > 0 &&
	    cl_token->string.self[cl_token->string.fillp-1] == '\r')
		cl_token->string.fillp--;
#endif
	@(return copy_simple_string(cl_token) eof)
@)

@(defun read_char (&optional (strm Cnil) (eof_errorp Ct) eof_value recursivep)
@
	strm = stream_or_default_input(strm);
	if (stream_at_end(strm)) {
		if (Null(eof_errorp) && Null(recursivep))
			@(return eof_value)
		else
			FEend_of_file(strm);
	}
	@(return read_char(strm))
@)

@(defun unread_char (c &optional (strm Cnil))
@
	/* INV: unread_char() checks the type `c' */
	strm = stream_or_default_input(strm);
	unread_char(c, strm);
	@(return Cnil)
@)

@(defun peek_char (&optional peek_type (strm Cnil) (eof_errorp Ct) eof_value recursivep)
	int c;
	cl_object rtbl = cl_current_readtable();
@
	strm = stream_or_default_input(strm);
	if (Null(peek_type)) {
		if (stream_at_end(strm)) {
			if (Null(eof_errorp) && Null(recursivep))
				@(return eof_value)
			else
				FEend_of_file(strm);
		}
		c = readc_stream(strm);
		unreadc_stream(c, strm);
		@(return CODE_CHAR(c))
	}
	if (peek_type == Ct) {
		while (!stream_at_end(strm)) {
			c = readc_stream(strm);
			if (cat(rtbl, c) != cat_whitespace) {
				unreadc_stream(c, strm);
				@(return CODE_CHAR(c))
			}
		}
		if (Null(eof_errorp))
			@(return eof_value)
		else
			FEend_of_file(strm);
	}
	/* INV: char_eq() checks the type of `peek_type' */
	while (!stream_at_end(strm)) {
		cl_object c = read_char(strm);
		if (char_eq(c, peek_type)) {
			unread_char(c, strm);
			@(return c)
		}
	}
	if (Null(eof_errorp))
		@(return eof_value)
	else
		FEend_of_file(strm);
@)

@(defun listen (&optional (strm Cnil))
@
	strm = stream_or_default_input(strm);
	@(return (listen_stream(strm)? Ct : Cnil))
@)

@(defun read_char_no_hang (&optional (strm Cnil) (eof_errorp Ct) eof_value recursivep)
@
	strm = stream_or_default_input(strm);
#if 0
	if (!listen_stream(strm))
		/* Incomplete! */
		@(return Cnil)
	@(return read_char(strm))
#else
	/* This implementation fails for EOF. */
	if (listen_stream(strm))
		@(return read_char(strm))
	else if (!stream_at_end(strm))
		@(return Cnil)
	else if (Null(eof_errorp) && Null(recursivep))
		@(return eof_value)
	else
		FEend_of_file(strm);
#endif
@)

@(defun clear_input (&optional (strm Cnil))
@
	strm = stream_or_default_input(strm);
	clear_input_stream(strm);
	@(return Cnil)
@)

@(defun parse_integer (strng
		       &key (start MAKE_FIXNUM(0))
			    end
			    (radix MAKE_FIXNUM(10))
			    junk_allowed
		       &aux x)
	cl_index s, e, ep;
	cl_object rtbl = cl_current_readtable();
@
	assert_type_string(strng);
	get_string_start_end(strng, start, end, &s, &e);
	if (!FIXNUMP(radix) ||
	    fix(radix) < 2 || fix(radix) > 36)
		FEerror("~S is an illegal radix.", 1, radix);
	while (rtbl->readtable.table[strng->string.self[s]].syntax_type
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
		if (rtbl->readtable.table[strng->string.self[s]].syntax_type
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

cl_object
si_read_bytes(cl_object stream, cl_object string, cl_object start, cl_object end)
{
	int is, ie, c; FILE *fp;

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
}



@(defun copy_readtable (&o (from cl_current_readtable()) to)
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

cl_object
cl_readtablep(cl_object readtable)
{
	@(return ((type_of(readtable) == t_readtable)? Ct : Cnil))
}

static struct readtable_entry*
read_table_entry(cl_object rdtbl, cl_object c)
{
	/* INV: char_code() checks the type of `c' */
	assert_type_readtable(rdtbl);
	return &(rdtbl->readtable.table[char_code(c)]);
}

@(defun set_syntax_from_char (tochr fromchr
			      &o (tordtbl cl_current_readtable())
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
		torte->dispatch_table = (cl_object *)cl_alloc(rtab_size);
		memcpy(torte->dispatch_table, fromrte->dispatch_table, rtab_size);
	}
	@(return Ct)
@)

@(defun set_macro_character (chr fnc
			     &optional ntp
				       (rdtbl cl_current_readtable()))
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

@(defun get_macro_character (chr &o (rdtbl cl_current_readtable()))
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
	&optional ntp (rdtbl cl_current_readtable()))
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
	table = (cl_object *)cl_alloc(RTABSIZE * sizeof(cl_object));
	entry->dispatch_table = table;
	for (i = 0;  i < RTABSIZE;  i++)
		table[i] = default_dispatch_macro;
	entry->macro = dispatch_reader;
	@(return Ct)
@)

@(defun set_dispatch_macro_character (dspchr subchr fnc
	&optional (rdtbl cl_current_readtable()))
	struct readtable_entry*entry;
	cl_fixnum subcode;
@
	entry = read_table_entry(rdtbl, dspchr);
	if (entry->macro != dispatch_reader || entry->dispatch_table == NULL)
		FEerror("~S is not a dispatch character.", 1, dspchr);
	subcode = char_code(subchr);
	if (islower(subcode))
		subcode = toupper(subcode);
	entry->dispatch_table[subcode] = fnc;
	@(return Ct)
@)

@(defun get_dispatch_macro_character (dspchr subchr
	&optional (rdtbl cl_current_readtable()))
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
c_string_to_object(const char *s)
{
	return si_string_to_object(make_constant_string(s));
}

cl_object
si_string_to_object(cl_object x)
{
	cl_object in;

	assert_type_string(x);
	in = make_string_input_stream(x, 0, x->string.fillp);
	preserving_whitespace_flag = FALSE;
	detect_eos_flag = FALSE;
	x = read_object(in);
	@(return x)
}

cl_object
si_standard_readtable()
{
	@(return standard_readtable)
}

static void
extra_argument(int c, cl_object d)
{
	FEerror("~S is an extra argument for the #~C readmacro.",
		2, d, CODE_CHAR(c));
}


#define	make_cf(f)	cl_make_cfun_va((cl_objectfn)(f), Cnil, NULL)

void
init_read(void)
{
	struct readtable_entry *rtab;
	cl_object *dtab;
	int i;

	standard_readtable = cl_alloc_object(t_readtable);
	register_root(&standard_readtable);

	standard_readtable->readtable.table
	= rtab
	= (struct readtable_entry *)cl_alloc(RTABSIZE * sizeof(struct readtable_entry));
	for (i = 0;  i < RTABSIZE;  i++) {
		rtab[i].syntax_type = cat_constituent;
		rtab[i].macro = OBJNULL;
		rtab[i].dispatch_table = NULL;
	}

	dispatch_reader = make_cf(dispatch_reader_fun);
	register_root(&dispatch_reader);

	rtab['\t'].syntax_type = cat_whitespace;
	rtab['\n'].syntax_type = cat_whitespace;
	rtab['\f'].syntax_type = cat_whitespace;
	rtab['\r'].syntax_type = cat_whitespace;
	rtab[' '].syntax_type = cat_whitespace;
	rtab['"'].syntax_type = cat_terminating;
	rtab['"'].macro = make_cf(double_quote_reader);
	rtab['#'].syntax_type = cat_non_terminating;
	rtab['#'].macro = dispatch_reader;
	rtab['\''].syntax_type = cat_terminating;
	rtab['\''].macro = make_cf(single_quote_reader);
	rtab['('].syntax_type = cat_terminating;
	rtab['('].macro = make_cf(left_parenthesis_reader);
	rtab[')'].syntax_type = cat_terminating;
	rtab[')'].macro = make_cf(right_parenthesis_reader);
/*
	rtab[','].syntax_type = cat_terminating;
	rtab[','].macro = make_cf(comma_reader);
*/
	rtab[';'].syntax_type = cat_terminating;
	rtab[';'].macro = make_cf(semicolon_reader);
	rtab['\\'].syntax_type = cat_single_escape;
/*
	rtab['`'].syntax_type = cat_terminating;
	rtab['`'].macro = make_cf(backquote_reader);
*/
	rtab['|'].syntax_type = cat_multiple_escape;
/*
	rtab['|'].macro = make_cf(vertical_bar_reader);
*/

	default_dispatch_macro = make_cf(default_dispatch_macro_fun);
	register_root(&default_dispatch_macro);

	rtab['#'].dispatch_table
	= dtab
	= (cl_object *)cl_alloc(RTABSIZE * sizeof(cl_object));
	for (i = 0;  i < RTABSIZE;  i++)
		dtab[i] = default_dispatch_macro;
	dtab['C'] = dtab['c'] = make_cf(sharp_C_reader);
	dtab['\\'] = make_cf(sharp_backslash_reader);
	dtab['\''] = make_cf(sharp_single_quote_reader);
	dtab['('] = make_cf(sharp_left_parenthesis_reader);
	dtab['*'] = make_cf(sharp_asterisk_reader);
	dtab[':'] = make_cf(sharp_colon_reader);
	dtab['.'] = make_cf(sharp_dot_reader);
	dtab['!'] = make_cf(sharp_exclamation_reader);
	/*  Used for fasload only. */
	dtab['B'] = dtab['b'] = make_cf(sharp_B_reader);
	dtab['O'] = dtab['o'] = make_cf(sharp_O_reader);
	dtab['X'] = dtab['x'] = make_cf(sharp_X_reader);
	dtab['R'] = dtab['r'] = make_cf(sharp_R_reader);
/*
	dtab['A'] = dtab['a'] = make_cf(sharp_A_reader);
	dtab['S'] = dtab['s'] = make_cf(sharp_S_reader);
*/
	dtab['A'] = dtab['a'] = @'si::sharp-a-reader';
	dtab['S'] = dtab['s'] = @'si::sharp-s-reader';
	dtab['P'] = dtab['p'] = make_cf(sharp_P_reader);

	dtab['='] = make_cf(sharp_eq_reader);
	dtab['#'] = make_cf(sharp_sharp_reader);
	dtab['+'] = make_cf(sharp_plus_reader);
	dtab['-'] = make_cf(sharp_minus_reader);
/*
	dtab['<'] = make_cf(sharp_less_than_reader);
*/
	dtab['|'] = make_cf(sharp_vertical_bar_reader);
	dtab['"'] = make_cf(sharp_double_quote_reader);
	/*  This is specific to this implementation  */
	dtab['$'] = make_cf(sharp_dollar_reader);
	/*  This is specific to this implimentation  */
/*
	dtab[' '] = dtab['\t'] = dtab['\n'] = dtab['\f']
	= make_cf(sharp_whitespace_reader);
	dtab[')'] = make_cf(sharp_right_parenthesis_reader);
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

	sharp_eq_context = Cnil;

	delimiting_char = OBJNULL;
	register_root(&delimiting_char);

	detect_eos_flag = FALSE;

	SYM_VAL(@'si::*cblock*') = Cnil;
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
	volatile cl_object x;
	int i, len;
	bool e;
	cl_object in;
	entry_point_ptr entry_point = (entry_point_ptr)entry;
	cl_object *VV;

	if (block == NULL)
		block = cl_alloc_object(t_codeblock);

	(*entry_point)(block);
	len = block->cblock.data_size;

#ifdef GBC_BOEHM
	VV = block->cblock.data = len? (cl_object *)cl_alloc(len * sizeof(cl_object)) : NULL;
#else
	VV = block->cblock.data;
#endif

	in = OBJNULL;
	if (frs_push(FRS_PROTECT, Cnil))
		e = TRUE;
	else {
		bds_bind(@'si::*cblock*', block);
		if (len == 0) goto NO_DATA;
		in=make_string_input_stream(make_constant_string(block->cblock.data_text),
					    0, block->cblock.data_text_size);
		bds_bind(@'*read-base*', MAKE_FIXNUM(10));
		bds_bind(@'*read-default-float-format*', @'single-float');
		bds_bind(@'*read-suppress*', Cnil);
		bds_bind(@'*package*', lisp_package);
		bds_bind(@'*readtable*', standard_readtable);
		for (i = 0 ; i < len; i++) {
			x = @read(4, in, Cnil, OBJNULL, Cnil);
			if (x == OBJNULL)
				break;
			VV[i] = x;
		}
		bds_unwind_n(5);
		if (i < len)
			FEerror("Not enough data while loading binary file",0);
	NO_DATA:
		(*entry_point)(MAKE_FIXNUM(0));
		bds_unwind1;
		e = FALSE;
	}

	frs_pop();
	if (in != OBJNULL)
		close_stream(in, 0);

	if (e) unwind(nlj_fr, nlj_tag);
}

