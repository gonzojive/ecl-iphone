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

#include <limits.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include "ecl.h"
#include "internal.h"
#include "ecl-inl.h"

#define	cat(rtbl,c)	((rtbl)->readtable.table[c].syntax_type)
#define read_suppress (SYM_VAL(@'*read-suppress*') != Cnil)

/* FIXME! *READ-EVAL* is not taken into account */

static void extra_argument (int c, cl_object stream, cl_object d);

cl_object
read_char(cl_object stream)
{
	return CODE_CHAR(ecl_getc_noeof(stream));
}

void
unread_char(cl_object c, cl_object in)
{
	ecl_ungetc(char_code(c), in);
}

static cl_object patch_sharp(cl_object x);

cl_object
read_object_non_recursive(cl_object in)
{
	cl_object x;

	bds_bind(@'si::*sharp-eq-context*', Cnil);
	bds_bind(@'si::*backq-level*', MAKE_FIXNUM(0));
	x = read_object(in);
	if (!Null(SYM_VAL(@'si::*sharp-eq-context*')))
		x = patch_sharp(x);
	bds_unwind1();
	bds_unwind1();
	return(x);
}

static cl_object
read_object_with_delimiter(cl_object in, int delimiter)
{
	cl_object x;
	int c, base;
	enum ecl_chattrib a;
	cl_object p;
	cl_index length, i, colon;
	int colon_type, intern_flag;
	bool escape_flag;
	cl_object rtbl = ecl_current_readtable();

BEGIN:
	/* Beppe: */
	do {
		c = ecl_getc(in);
		if (c == EOF || c == delimiter)
			return(OBJNULL);
		a = cat(rtbl, c);
	} while (a == cat_whitespace);
	if (a == cat_terminating || a == cat_non_terminating) {
		cl_object x = rtbl->readtable.table[c].macro;
		cl_object o = funcall(3, x, in, CODE_CHAR(c));
		if (NVALUES == 0) goto BEGIN;
		if (NVALUES > 1) FEerror("The readmacro ~S returned ~D values.",
					 2, x, MAKE_FIXNUM(i));
		return o;
	}
	escape_flag = FALSE;
	length = 0;
	colon_type = 0;
	cl_env.token->string.fillp = 0;
	for (;;) {
		if (a == cat_single_escape) {
			c = ecl_getc_noeof(in);
			a = cat_constituent;
			escape_flag = TRUE;
		} else if (a == cat_multiple_escape) {
			escape_flag = TRUE;
			for (;;) {
				c = ecl_getc_noeof(in);
				a = cat(rtbl, c);
				if (a == cat_single_escape) {
					c = ecl_getc_noeof(in);
					a = cat_constituent;
				} else if (a == cat_multiple_escape)
					break;
				cl_string_push_extend(cl_env.token, c);
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
			ecl_ungetc(c, in);
			break;
		}
		cl_string_push_extend(cl_env.token, c);
		length++;
	NEXT:
		c = ecl_getc(in);
		if (c == EOF)
			break;
		a = cat(rtbl, c);
	}

	if (read_suppress)
		return(Cnil);

	if (!escape_flag && length == 1 && cl_env.token->string.self[0] == '.') {
		return @'si::.';
	} else if (!escape_flag && length > 0) {
		for (i = 0;  i < length;  i++)
			if (cl_env.token->string.self[i] != '.')
				goto N;
		FEreader_error("Dots appeared illegally.", in, 0);
	}

N:
	base = ecl_current_read_base();
	if (escape_flag || (base <= 10 && isalpha(cl_env.token->string.self[0])))
		goto SYMBOL;
	x = parse_number(cl_env.token->string.self, cl_env.token->string.fillp, &i, base);
	if (x != OBJNULL && length == i)
		return(x);

SYMBOL:
	if (colon_type == 1 /* && length > colon + 1 */) {
		if (colon == 0)
			p = cl_core.keyword_package;
		else {
			cl_env.token->string.fillp = colon;
			p = find_package(cl_env.token);
			if (Null(p)) {
				FEerror("There is no package with the name ~A.",
						1, copy_simple_string(cl_env.token));
			}
		}
		cl_env.token->string.fillp = length - (colon + 1);
		memmove(cl_env.token->string.self,
			cl_env.token->string.self + colon + 1,
			sizeof(*cl_env.token->string.self) * cl_env.token->string.fillp);
		if (colon > 0) {
			cl_env.token->string.self[cl_env.token->string.fillp] = '\0';
			x = find_symbol(cl_env.token, p, &intern_flag);
			if (intern_flag != EXTERNAL)
			FEerror("Cannot find the external symbol ~A in ~S.",
				2, copy_simple_string(cl_env.token), p);
			return(x);
		}
	} else if (colon_type == 2 /* && colon > 0 && length > colon + 2 */) {
		cl_env.token->string.fillp = colon;
		p = find_package(cl_env.token);
		if (Null(p)) {
			/* When loading binary files, we sometimes must create
			   symbols whose package has not yet been maked. We
			   allow it, but later on in read_VV we make sure that
			   all referenced packages have been properly built.
			*/
			cl_object name = copy_simple_string(cl_env.token);
			if (cl_core.packages_to_be_created == OBJNULL) {
				FEerror("There is no package with the name ~A.",
					1, name);
			} else if (!Null(p = assoc(name, cl_core.packages_to_be_created))) {
				p = CDR(p);
			} else {
				p = make_package(name,Cnil,Cnil);
				cl_core.packages = CDR(cl_core.packages);
				cl_core.packages_to_be_created =
					cl_acons(name, p, cl_core.packages_to_be_created);
			}
		}
		cl_env.token->string.fillp = length - (colon + 2);
		memmove(cl_env.token->string.self,
			cl_env.token->string.self + colon + 2,
			sizeof(*cl_env.token->string.self) * cl_env.token->string.fillp);
	} else
		p = current_package();
	cl_env.token->string.self[cl_env.token->string.fillp] = '\0';
	x = find_symbol(cl_env.token, p, &intern_flag);
	if (intern_flag == 0)
		x = intern(copy_simple_string(cl_env.token), p, &intern_flag);
	return(x);
}

/*
	Read_object(in) reads an object from stream in.
	This routine corresponds to COMMON Lisp function READ.
*/
cl_object
read_object(cl_object in)
{
	return read_object_with_delimiter(in, EOF);
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
		exponent_marker = ecl_current_read_default_float_format();
		goto MAKE_FLOAT;

	case 'f':  case 'F':  case 's':  case 'S':
		x = make_shortfloat((float)fraction);
		break;

	case 'd':  case 'D':  case 'l':  case 'L':
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

static cl_object
left_parenthesis_reader(cl_object in, cl_object character)
{
	cl_object x, y;
	cl_object *p;
	int c;
	cl_object rtbl = ecl_current_readtable();

	y = Cnil;
	for (p = &y ; ; p = &(CDR(*p))) {
		x = read_object_with_delimiter(in, ')');
		if (x == OBJNULL)
			break;
		if (x == @'si::.') {
			if (p == &y)
				FEreader_error("A dot appeared after a left parenthesis.", in, 0);
			*p = read_object(in);
			if (*p == OBJNULL)
				FEend_of_file(in);
			if (*p == @'si::.')
				FEreader_error("Two dots appeared consecutively.", in, 0);
			c = ecl_getc_noeof(in);
			while (cat(rtbl, c) == cat_whitespace)
				c = ecl_getc_noeof(in);
			if (c != ')')
				FEreader_error("More than one object after '.' in a list", in, 0);
			break;
		}
		*p = CONS(x, Cnil);
	}
	@(return y)
}
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
	cl_object rtbl = ecl_current_readtable();

	cl_env.token->string.fillp = 0;
	for (;;) {
		c = ecl_getc_noeof(in);
		if (c == delim)
			break;
		else if (cat(rtbl, c) == cat_single_escape)
			c = ecl_getc_noeof(in);
		cl_string_push_extend(cl_env.token, c);
	}
}

/*
	Read_constituent(in) reads
	a sequence of constituent characters from stream in
	and places it in cl_env.token.
*/
static void
read_constituent(cl_object in)
{
	int c;
	cl_object rtbl = ecl_current_readtable();

	cl_env.token->string.fillp = 0;
	for (;;) {
		c = ecl_getc_noeof(in);
		if (cat(rtbl, c) != cat_constituent) {
			ecl_ungetc(c, in);
			break;
		}
		cl_string_push_extend(cl_env.token, c);
	}
}

static cl_object
double_quote_reader(cl_object in, cl_object c)
{
	read_string('"', in);
	@(return copy_simple_string(cl_env.token))
}

static cl_object
dispatch_reader_fun(cl_object in, cl_object dc)
{
	cl_object x, y;
	cl_fixnum i;
	int d, c;
	cl_object rtbl = ecl_current_readtable();

	if (rtbl->readtable.table[char_code(dc)].dispatch_table == NULL)
		FEreader_error("~C is not a dispatching macro character", in, 1, dc);

	c = ecl_getc_noeof(in);
	d = digitp(c, 10);
	if (d >= 0) {
		i = 0;
		do {
			i = 10*i + d;
			c = ecl_getc_noeof(in);
			d = digitp(c, 10);
		} while (d >= 0);
		y = MAKE_FIXNUM(i);
	} else
		y = Cnil;

	x = rtbl->readtable.table[char_code(dc)].dispatch_table[c];
	return funcall(4, x, in, CODE_CHAR(c), y);
}

static cl_object
single_quote_reader(cl_object in, cl_object c)
{
	@(return CONS(@'quote', CONS(read_object(in), Cnil)))
}

static cl_object
void_reader(cl_object in, cl_object c)
{
	/*  no result  */
	@(return)
}

#define right_parenthesis_reader void_reader

static cl_object
semicolon_reader(cl_object in, cl_object c)
{
	int auxc;

	do
		auxc = ecl_getc(in);
	while (auxc != '\n' && auxc != EOF);
	/*  no result  */
	@(return)
}

/*
	sharpmacro routines
*/

static cl_object
sharp_C_reader(cl_object in, cl_object c, cl_object d)
{
	cl_object x, real, imag;

	if (d != Cnil && !read_suppress)
		extra_argument('C', in, d);
	if (ecl_getc_noeof(in) != '(')
		FEreader_error("A left parenthesis is expected.", in, 0);
	real = read_object_with_delimiter(in, ')');
	if (real == OBJNULL)
		FEreader_error("No real part.", in, 0);
	imag = read_object_with_delimiter(in, ')');
	if (imag == OBJNULL)
		FEreader_error("No imaginary part.", in, 0);
	x = read_object_with_delimiter(in, ')');
	if (x != OBJNULL)
		FEreader_error("A right parenthesis is expected.", in, 0);
	if (read_suppress)
		@(return Cnil);
	/* INV: make_complex() checks its types. When reading circular
	   structures, we cannot check the types of the elements, and we
	   must build the complex number by hand. */
	if ((CONSP(real) || CONSP(imag)) &&
	    !Null(SYM_VAL(@'si::*sharp-eq-context*')))
	{
		x = cl_alloc_object(t_complex);
		x->complex.real = real;
		x->complex.imag = imag;
	} else {
		x = make_complex(real, imag);
	}
	@(return x)
}

static cl_object
sharp_backslash_reader(cl_object in, cl_object c, cl_object d)
{
	if (d != Cnil && !read_suppress)
		if (!FIXNUMP(d) ||
		    fix(d) != 0)
			FEreader_error("~S is an illegal CHAR-FONT.", in, 1, d);
			/*  assuming that CHAR-FONT-LIMIT is 1  */
	ecl_ungetc('\\', in);
	if (read_suppress) {
		(void)read_object(in);
		@(return Cnil)
	}
	ECL_SETQ(@'*read-suppress*', Ct);
	(void)read_object(in);
	ECL_SETQ(@'*read-suppress*', Cnil);
	c = cl_env.token;
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
				FEreader_error("Octal digit expected.", in, 0);
			else
				n = 8*n + c->string.self[i] - '0';
		c = CODE_CHAR(n & 0377);
	} else {
		cl_object nc = cl_name_char(c);
		if (Null(nc)) FEreader_error("~S is an illegal character name.", in, 1, copy_simple_string(c));
		c = nc;
	}
	@(return c)
}

static cl_object
sharp_single_quote_reader(cl_object in, cl_object c, cl_object d)
{
	if(d != Cnil && !read_suppress)
		extra_argument('#', in, d);
	@(return CONS(@'function', CONS(read_object(in), Cnil)))
}

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

static cl_object
sharp_left_parenthesis_reader(cl_object in, cl_object c, cl_object d)
{
	bool fixed_size;
	cl_index dim, dimcount, i, a;
	cl_index sp = cl_stack_index();
	cl_object x, last;
	extern int _cl_backq_car(cl_object *);

	if (Null(d) || read_suppress)
		fixed_size = FALSE;
	else {
		fixed_size = TRUE;
		dim = fixnnint(d);
	}
	if (fix(SYM_VAL(@'si::*backq-level*')) > 0) {
		ecl_ungetc('(', in);
		x = read_object(in);
		a = _cl_backq_car(&x);
		if (a == APPEND || a == NCONC)
			FEreader_error(",at or ,. has appeared in an illegal position.", in, 0);
		if (a == QUOTE) {
		  for (dimcount = 0;  !endp(x);  x = CDR(x), dimcount++)
		    cl_stack_push(CAR(x));
		  goto L;
		}
		@(return cl_list(4, @'si::,', @'apply',
				 CONS(@'quote', CONS(@'vector', Cnil)), x))
	}
	for (dimcount = 0 ;; dimcount++) {
		x = read_object_with_delimiter(in, ')');
		if (x == OBJNULL)
			break;
		cl_stack_push(x);
	}
L:
	if (fixed_size) {
		if (dimcount > dim)
			FEreader_error("Too many elements in #(...).", in, 0);
		if (dimcount == 0)
			FEreader_error("Cannot fill the vector #().", in, 0);
		else last = cl_env.stack_top[-1];
	} else
		dim = dimcount;
	x = cl_alloc_simple_vector(dim, aet_object);
	x->vector.self.t = (cl_object *)cl_alloc_align(dim * sizeof(cl_object), sizeof(cl_object));
	for (i = 0; i < dim; i++)
		x->vector.self.t[i] = (i < dimcount) ? cl_env.stack[sp+i] : last;
	cl_stack_pop_n(dimcount);
	@(return x)
}

static cl_object
sharp_asterisk_reader(cl_object in, cl_object c, cl_object d)
{
	bool fixed_size;
	cl_index dim, dimcount, i;
	cl_index sp = cl_stack_index();
	cl_object last, elt, x;

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
	 	int x = ecl_getc(in);
		if (x == EOF)
			break;
		if (x != '0' && x != '1') {
			ecl_ungetc(x, in);
			break;
		} else {
			cl_stack_push(MAKE_FIXNUM(x == '1'));
		}
	}
	if (fixed_size) {
		if (dimcount > dim)
			FEreader_error("Too many elements in #*....", in, 0);
		if (dimcount == 0)
			FEreader_error("Cannot fill the bit-vector #*.", in, 0);
		else last = cl_env.stack_top[-1];
	} else {
		dim = dimcount;
	}
	x = cl_alloc_simple_bitvector(dim);
	x->vector.self.bit = (byte *)cl_alloc_atomic((dim + CHAR_BIT - 1)/CHAR_BIT);
	for (i = 0; i < dim; i++) {
		elt = (i < dimcount) ? cl_env.stack[sp+i] : last;
		if (elt == MAKE_FIXNUM(0))
			x->vector.self.bit[i/CHAR_BIT] &= ~(0200 >> i%CHAR_BIT);
		else
			x->vector.self.bit[i/CHAR_BIT] |= 0200 >> i%CHAR_BIT;
	}
	cl_stack_pop_n(dimcount);
	@(return x)
}

static cl_object
sharp_colon_reader(cl_object in, cl_object ch, cl_object d)
{
	cl_object rtbl = ecl_current_readtable();
	enum ecl_chattrib a;
	bool escape_flag;
	int c;

	if (d != Cnil && !read_suppress)
		extra_argument(':', in, d);
	c = ecl_getc_noeof(in);
	a = cat(rtbl, c);
	escape_flag = FALSE;
	cl_env.token->string.fillp = 0;
	goto L;
	for (;;) {
		cl_string_push_extend(cl_env.token, c);
	K:
		c = ecl_getc(in);
		if (c == EOF)
			goto M;
		a = cat(rtbl, c);
	L:
		if (a == cat_single_escape) {
			c = ecl_getc_noeof(in);
			a = cat_constituent;
			escape_flag = TRUE;
		} else if (a == cat_multiple_escape) {
			escape_flag = TRUE;
			for (;;) {
				c = ecl_getc_noeof(in);
				a = cat(rtbl, c);
				if (a == cat_single_escape) {
					c = ecl_getc_noeof(in);
					a = cat_constituent;
				} else if (a == cat_multiple_escape)
					break;
				cl_string_push_extend(cl_env.token, c);
			}
			goto K;
		} else if (islower(c))
			c = toupper(c);
		if (a == cat_whitespace || a == cat_terminating)
			break;
	}
	ecl_ungetc(c, in);

M:
	if (read_suppress)
		@(return Cnil)
	@(return make_symbol(copy_simple_string(cl_env.token)))
}

static cl_object
sharp_dot_reader(cl_object in, cl_object c, cl_object d)
{
	if(d != Cnil && !read_suppress)
		extra_argument('.', in, d);
	in = read_object(in);
	if (read_suppress)
		@(return Cnil)
	in = si_eval_with_env(in, Cnil);
	@(return in)
}

static cl_object
sharp_B_reader(cl_object in, cl_object c, cl_object d)
{
	cl_index i;
	cl_object x;

	if(d != Cnil && !read_suppress)
		extra_argument('B', in, d);
	read_constituent(in);
	if (read_suppress)
		@(return Cnil)
	x = parse_number(cl_env.token->string.self, cl_env.token->string.fillp, &i, 2);
	if (x == OBJNULL || i != cl_env.token->string.fillp)
		FEreader_error("Cannot parse the #B readmacro.", in, 0);
	if (type_of(x) == t_shortfloat ||
	    type_of(x) == t_longfloat)
		FEreader_error("The float ~S appeared after the #B readmacro.",
			       in, 1, x);
	@(return x)
}

static cl_object
sharp_O_reader(cl_object in, cl_object c, cl_object d)
{
	cl_index i;
	cl_object x;

	if(d != Cnil && !read_suppress)
		extra_argument('O', in, d);
	read_constituent(in);
	if (read_suppress)
		@(return Cnil)
	x = parse_number(cl_env.token->string.self, cl_env.token->string.fillp, &i, 8);
	if (x == OBJNULL || i != cl_env.token->string.fillp)
		FEreader_error("Cannot parse the #O readmacro.", in, 0);
	if (type_of(x) == t_shortfloat ||
	    type_of(x) == t_longfloat)
		FEreader_error("The float ~S appeared after the #O readmacro.",
			       in, 1, x);
	@(return x)
}

static cl_object
sharp_X_reader(cl_object in, cl_object c, cl_object d)
{
	cl_index i;
	cl_object x;

	if(d != Cnil && !read_suppress)
		extra_argument('X', in, d);
	read_constituent(in);
	if (read_suppress)
		@(return Cnil)
	x = parse_number(cl_env.token->string.self, cl_env.token->string.fillp, &i, 16);
	if (x == OBJNULL || i != cl_env.token->string.fillp)
		FEreader_error("Cannot parse the #X readmacro.", in, 0);
	if (type_of(x) == t_shortfloat ||
	    type_of(x) == t_longfloat)
		FEreader_error("The float ~S appeared after the #X readmacro.",
			       in, 1, x);
	@(return x)
}

static cl_object
sharp_R_reader(cl_object in, cl_object c, cl_object d)
{
	int radix;
	cl_index i;
	cl_object x;

	if (read_suppress)
		radix = 10;
	else if (FIXNUMP(d)) {
		radix = fix(d);
		if (radix > 36 || radix < 2)
			FEreader_error("~S is an illegal radix.", in, 1, d);
	} else
		FEreader_error("No radix was supplied in the #R readmacro.", in, 0);
	read_constituent(in);
	if (read_suppress)
		@(return Cnil)
	x = parse_number(cl_env.token->string.self, cl_env.token->string.fillp, &i, radix);
	if (x == OBJNULL || i != cl_env.token->string.fillp)
		FEreader_error("Cannot parse the #R readmacro.", in, 0);
	if (type_of(x) == t_shortfloat ||
	    type_of(x) == t_longfloat)
		FEreader_error("The float ~S appeared after the #R readmacro.",
			       in, 1, x);
	@(return x)
}

#define sharp_A_reader void_reader
#define sharp_S_reader void_reader

static cl_object
sharp_eq_reader(cl_object in, cl_object c, cl_object d)
{
	cl_object pair, value;
	cl_object sharp_eq_context = SYM_VAL(@'si::*sharp-eq-context*');

	if (read_suppress) @(return)
	if (Null(d))
		FEreader_error("The #= readmacro requires an argument.", in, 0);
	if (assql(d, sharp_eq_context) != Cnil)
		FEreader_error("Duplicate definitions for #~D=.", in, 1, d);
	pair = CONS(d, Cnil);
	ECL_SETQ(@'si::*sharp-eq-context*', CONS(pair, sharp_eq_context));
	value = read_object(in);
	if (value == pair)
		FEreader_error("#~D# is defined by itself.", in, 1, d);
	@(return (CDR(pair) = value))
}

static cl_object
sharp_sharp_reader(cl_object in, cl_object c, cl_object d)
{
	cl_object pair;

	if (read_suppress) @(return)
	if (Null(d))
		FEreader_error("The ## readmacro requires an argument.", in, 0);
	pair = assq(d, SYM_VAL(@'si::*sharp-eq-context*'));
	if (pair != Cnil)
		@(return pair)
	FEreader_error("#~D# is undefined.", in, 1, d);
}

static cl_object
do_patch_sharp(cl_object x)
{
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
	case t_complex: {
		cl_object r = do_patch_sharp(x->complex.real);
		cl_object i = do_patch_sharp(x->complex.imag);
		if (r != x->complex.real || i != x->complex.imag) {
			cl_object c = make_complex(r, i);
			x->complex = c->complex;
		}
	}
	default:
	}
	return(x);
}

static cl_object
patch_sharp(cl_object x)
{
	cl_object pair, sharp_eq_context = SYM_VAL(@'si::*sharp-eq-context*');

	pair = sharp_eq_context;
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

static cl_object
sharp_vertical_bar_reader(cl_object in, cl_object ch, cl_object d)
{
	int c;
	int level = 0;

	if (d != Cnil && !read_suppress)
		extra_argument('|', in, d);
	for (;;) {
		c = ecl_getc_noeof(in);
	L:
		if (c == '#') {
			c = ecl_getc_noeof(in);
			if (c == '|')
				level++;
		} else if (c == '|') {
			c = ecl_getc_noeof(in);
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
}

static cl_object
default_dispatch_macro_fun(cl_object in, cl_object c, cl_object d)
{
	FEreader_error("No dispatch function defined for character ~s.", in, 1, c);
}

/*
	#P" ... " returns the pathname with namestring ... .
*/
static cl_object
sharp_P_reader(cl_object in, cl_object c, cl_object d)
{
	if (d != Cnil && !read_suppress)
		extra_argument('P', in, d);
	@(return cl_pathname(read_object(in)))
}

/*
	#$ fixnum returns a random-state with the fixnum
	as its content.
*/
static cl_object
sharp_dollar_reader(cl_object in, cl_object c, cl_object d)
{
	cl_object output;

	if (d != Cnil && !read_suppress)
		extra_argument('$', in, d);
	c = read_object(in);
	return cl_make_random_state(1, c);
}

/*
	readtable routines
*/

cl_object
copy_readtable(cl_object from, cl_object to)
{
	struct ecl_readtable_entry *rtab;
	cl_index i;

	if (Null(to)) {
		to = cl_alloc_object(t_readtable);
		to->readtable.table = NULL;
			/*  Saving for GC.  */
		to->readtable.table
		= rtab
 		= (struct ecl_readtable_entry *)cl_alloc_align(RTABSIZE * sizeof(struct ecl_readtable_entry), sizeof(struct ecl_readtable_entry));
		memcpy(rtab, from->readtable.table,
			 RTABSIZE * sizeof(struct ecl_readtable_entry));
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
ecl_current_readtable(void)
{
	cl_object r;

	/* INV: *readtable* always has a value */
	r = SYM_VAL(@'*readtable*');
	if (type_of(r) != t_readtable) {
		ECL_SETQ(@'*readtable*', copy_readtable(cl_core.standard_readtable, Cnil));
		FEerror("The value of *READTABLE*, ~S, was not a readtable.",
			1, r);
	}
	return(r);
}

int
ecl_current_read_base(void)
{
	cl_object x;

	/* INV: *READ-BASE* always has a value */
	x = SYM_VAL(@'*read_base*');
	if (FIXNUMP(x)) {
		cl_fixnum b = fix(x);
		if (b >= 2 && b <= 36)
			return b;
	}
	ECL_SETQ(@'*read_base*', MAKE_FIXNUM(10));
	FEerror("The value of *READ-BASE*, ~S, was illegal.", 1, x);
}

char
ecl_current_read_default_float_format(void)
{
	cl_object x;

	/* INV: *READ-DEFAULT-FLOAT-FORMAT* is always bound to something */
	x = SYM_VAL(@'*read-default-float-format*');
	if (x == @'single-float' || x == @'short-float')
		return 'S';
	if (x == @'double-float' || x == @'long-float')
		return 'D';
	ECL_SETQ(@'*read-default-float-format*', @'single-float');
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
			recursivep)
	cl_object x;
@
	strm = stream_or_default_input(strm);
	if (Null(recursivep)) {
		x = read_object_non_recursive(strm);
	} else {
		x = read_object(strm);
	}
	if (x == OBJNULL) {
		if (Null(eof_errorp))
			@(return eof_value)
		FEend_of_file(strm);
	}
	/* Skip whitespace characters, but stop at beginning of new line or token */
	if (Null(recursivep)) {
		cl_object rtbl = ecl_current_readtable();
		int c = ecl_getc(strm);
		if (c != EOF && (cat(rtbl, c) != cat_whitespace)) {
			ecl_ungetc(c, strm);
		}
	}
	@(return x)
@)

@(defun read_preserving_whitespace
	(&optional (strm Cnil)
		   (eof_errorp Ct)
		   eof_value
		   recursivep)
	cl_object x;
@
	strm = stream_or_default_input(strm);
	if (Null(recursivep)) {
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

static cl_object
do_read_delimited_list(int d, cl_object strm)
{
	cl_object l, x, *p;
	l = Cnil;
	p = &l;
	for (;;) {
		x = read_object_with_delimiter(strm, d);
		if (x == OBJNULL)
			break;
		*p = CONS(x, Cnil);
		p = &(CDR((*p)));
	}
	return l;
}

@(defun read_delimited_list (d &optional (strm Cnil) recursivep)
	cl_object l;
	int delimiter = char_code(d);
@
	strm = stream_or_default_input(strm);
	if (Null(recursivep))
		l = do_read_delimited_list(delimiter, strm);
	else {
		bds_bind(@'si::*sharp-eq-context*', Cnil);
		bds_bind(@'si::*backq-level*', MAKE_FIXNUM(0));
		l = do_read_delimited_list(delimiter, strm);
		if (!Null(SYM_VAL(@'si::*sharp-eq-context*')))
			l = patch_sharp(l);
		bds_unwind1();
		bds_unwind1();
	}
	@(return l)
@)

@(defun read_line (&optional (strm Cnil) (eof_errorp Ct) eof_value recursivep)
	int c;
@
	strm = stream_or_default_input(strm);
	cl_env.token->string.fillp = 0;
	for (;;) {
		c = ecl_getc(strm);
		if (c == EOF || c == '\n')
			break;
		cl_string_push_extend(cl_env.token, c);
	}
	if (c == EOF && cl_env.token->string.fillp == 0) {
		if (!Null(eof_errorp) || !Null(recursivep))
			FEend_of_file(strm);
		@(return eof_value Ct)
	}
#ifdef ECL_NEWLINE_IS_CRLF	/* From \r\n, ignore \r */
	if (cl_env.token->string.fillp > 0 &&
	    cl_env.token->string.self[cl_env.token->string.fillp-1] == '\r')
		cl_env.token->string.fillp--;
#endif
#ifdef ECL_NEWLINE_IS_LFCR	/* From \n\r, ignore \r */
	ecl_getc(strm);
#endif
	@(return copy_simple_string(cl_env.token) (c == EOF? Ct : Cnil))
@)

@(defun read_char (&optional (strm Cnil) (eof_errorp Ct) eof_value recursivep)
	int c;
	cl_object output;
@
	strm = stream_or_default_input(strm);
	c = ecl_getc(strm);
	if (c != EOF)
		output = CODE_CHAR(c);
	else if (Null(eof_errorp) && Null(recursivep))
		output = eof_value;
	else
		FEend_of_file(strm);
	@(return output)
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
	cl_object rtbl = ecl_current_readtable();
@
	strm = stream_or_default_input(strm);
	c = ecl_getc(strm);
	if (c != EOF && !Null(peek_type)) {
		if (peek_type == Ct) {
			do {
				if (cat(rtbl, c) != cat_whitespace)
					break;
				c = ecl_getc(strm);
			} while (c != EOF);
		} else {
			do {
				if (char_eq(CODE_CHAR(c), peek_type))
					break;
				c = ecl_getc(strm);
			} while (c != EOF);
		}
	}
	if (c != EOF) {
		ecl_ungetc(c, strm);
		eof_value = CODE_CHAR(c);
	} else if (!Null(eof_errorp)) {
		FEend_of_file(strm);
	}
	@(return eof_value)
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
	cl_object rtbl = ecl_current_readtable();
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
	c = ecl_getc(binary_input_stream);
	if (c == EOF) {
		if (Null(eof_errorp))
			@(return eof_value)
		else
			FEend_of_file(binary_input_stream);
	}
	@(return MAKE_FIXNUM(c))
@)

@(defun read_sequence (sequence stream &key (start MAKE_FIXNUM(0)) end)
@
	return si_do_read_sequence(sequence, stream, start, end);
@)


@(defun copy_readtable (&o (from ecl_current_readtable()) to)
@
	if (Null(from)) {
		from = cl_core.standard_readtable;
		if (to != Cnil)
			assert_type_readtable(to);
		to = copy_readtable(from, to);
		to->readtable.table['#'].dispatch_table['!']
		= cl_core.default_dispatch_macro;
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

/* FIXME! READTABLE-CASE is missing! */

static struct ecl_readtable_entry*
read_table_entry(cl_object rdtbl, cl_object c)
{
	/* INV: char_code() checks the type of `c' */
	assert_type_readtable(rdtbl);
	return &(rdtbl->readtable.table[char_code(c)]);
}

@(defun set_syntax_from_char (tochr fromchr
			      &o (tordtbl ecl_current_readtable())
				 fromrdtbl)
	struct ecl_readtable_entry*torte, *fromrte;
@
	/* INV: read_table_entry() checks all values */
	if (Null(fromrdtbl))
		fromrdtbl = cl_core.standard_readtable;
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
				       (rdtbl ecl_current_readtable()))
	struct ecl_readtable_entry*entry;
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

@(defun get_macro_character (chr &o (rdtbl ecl_current_readtable()))
	struct ecl_readtable_entry*entry;
	cl_object m;
@

	/* fix to allow NIL as readtable argument. Beppe */
	if (Null(rdtbl))
		rdtbl = cl_core.standard_readtable;
	/* INV: read_table_entry() checks our arguments */
	entry = read_table_entry(rdtbl, chr);
	m = entry->macro;
	if (m == OBJNULL)
		@(return Cnil Cnil)
	@(return m ((entry->syntax_type == cat_non_terminating)? Ct : Cnil))
@)

@(defun make_dispatch_macro_character (chr
	&optional ntp (rdtbl ecl_current_readtable()))
	struct ecl_readtable_entry*entry;
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
		table[i] = cl_core.default_dispatch_macro;
	entry->macro = cl_core.dispatch_reader;
	@(return Ct)
@)

@(defun set_dispatch_macro_character (dspchr subchr fnc
	&optional (rdtbl ecl_current_readtable()))
	struct ecl_readtable_entry*entry;
	cl_fixnum subcode;
@
	entry = read_table_entry(rdtbl, dspchr);
	if (entry->macro != cl_core.dispatch_reader || entry->dispatch_table == NULL)
		FEerror("~S is not a dispatch character.", 1, dspchr);
	subcode = char_code(subchr);
	if (islower(subcode))
		subcode = toupper(subcode);
	entry->dispatch_table[subcode] = fnc;
	@(return Ct)
@)

@(defun get_dispatch_macro_character (dspchr subchr
	&optional (rdtbl ecl_current_readtable()))
	struct ecl_readtable_entry*entry;
	cl_fixnum subcode;
@
	if (Null(rdtbl))
		rdtbl = cl_core.standard_readtable;
	entry = read_table_entry(rdtbl, dspchr);
	if (entry->macro != cl_core.dispatch_reader || entry->dispatch_table == NULL)
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
	x = read_object(in);
	if (x == OBJNULL)
		FEend_of_file(in);
	@(return x)
}

cl_object
si_standard_readtable()
{
	@(return cl_core.standard_readtable)
}

static void
extra_argument(int c, cl_object stream, cl_object d)
{
	FEreader_error("~S is an extra argument for the #~C readmacro.",
		       stream, 2, d, CODE_CHAR(c));
}


#define	make_cf2(f)	cl_make_cfun((f), Cnil, NULL, 2)
#define	make_cf3(f)	cl_make_cfun((f), Cnil, NULL, 3)

void
init_read(void)
{
	struct ecl_readtable_entry *rtab;
	cl_object readtable;
	cl_object *dtab;
	int i;

	cl_core.standard_readtable = cl_alloc_object(t_readtable);
	cl_core.standard_readtable->readtable.table
	= rtab
	= (struct ecl_readtable_entry *)cl_alloc(RTABSIZE * sizeof(struct ecl_readtable_entry));
	for (i = 0;  i < RTABSIZE;  i++) {
		rtab[i].syntax_type = cat_constituent;
		rtab[i].macro = OBJNULL;
		rtab[i].dispatch_table = NULL;
	}

	cl_core.dispatch_reader = make_cf2(dispatch_reader_fun);

	rtab['\t'].syntax_type = cat_whitespace;
	rtab['\n'].syntax_type = cat_whitespace;
	rtab['\f'].syntax_type = cat_whitespace;
	rtab['\r'].syntax_type = cat_whitespace;
	rtab[' '].syntax_type = cat_whitespace;
	rtab['"'].syntax_type = cat_terminating;
	rtab['"'].macro = make_cf2(double_quote_reader);
	rtab['#'].syntax_type = cat_non_terminating;
	rtab['#'].macro = cl_core.dispatch_reader;
	rtab['\''].syntax_type = cat_terminating;
	rtab['\''].macro = make_cf2(single_quote_reader);
	rtab['('].syntax_type = cat_terminating;
	rtab['('].macro = make_cf2(left_parenthesis_reader);
	rtab[')'].syntax_type = cat_terminating;
	rtab[')'].macro = make_cf2(right_parenthesis_reader);
/*
	rtab[','].syntax_type = cat_terminating;
	rtab[','].macro = make_cf2(comma_reader);
*/
	rtab[';'].syntax_type = cat_terminating;
	rtab[';'].macro = make_cf2(semicolon_reader);
	rtab['\\'].syntax_type = cat_single_escape;
/*
	rtab['`'].syntax_type = cat_terminating;
	rtab['`'].macro = make_cf2(backquote_reader);
*/
	rtab['|'].syntax_type = cat_multiple_escape;
/*
	rtab['|'].macro = make_cf2(vertical_bar_reader);
*/

	cl_core.default_dispatch_macro = make_cf3(default_dispatch_macro_fun);

	rtab['#'].dispatch_table
	= dtab
	= (cl_object *)cl_alloc(RTABSIZE * sizeof(cl_object));
	for (i = 0;  i < RTABSIZE;  i++)
		dtab[i] = cl_core.default_dispatch_macro;
	dtab['C'] = dtab['c'] = make_cf3(sharp_C_reader);
	dtab['\\'] = make_cf3(sharp_backslash_reader);
	dtab['\''] = make_cf3(sharp_single_quote_reader);
	dtab['('] = make_cf3(sharp_left_parenthesis_reader);
	dtab['*'] = make_cf3(sharp_asterisk_reader);
	dtab[':'] = make_cf3(sharp_colon_reader);
	dtab['.'] = make_cf3(sharp_dot_reader);
	/*  Used for fasload only. */
	dtab['B'] = dtab['b'] = make_cf3(sharp_B_reader);
	dtab['O'] = dtab['o'] = make_cf3(sharp_O_reader);
	dtab['X'] = dtab['x'] = make_cf3(sharp_X_reader);
	dtab['R'] = dtab['r'] = make_cf3(sharp_R_reader);
/*
	dtab['A'] = dtab['a'] = make_cf3(sharp_A_reader);
	dtab['S'] = dtab['s'] = make_cf3(sharp_S_reader);
*/
	dtab['A'] = dtab['a'] = @'si::sharp-a-reader';
	dtab['S'] = dtab['s'] = @'si::sharp-s-reader';
	dtab['P'] = dtab['p'] = make_cf3(sharp_P_reader);

	dtab['='] = make_cf3(sharp_eq_reader);
	dtab['#'] = make_cf3(sharp_sharp_reader);
	dtab['+'] = make_cf3(sharp_plus_reader);
	dtab['-'] = make_cf3(sharp_minus_reader);
/*
	dtab['<'] = make_cf3(sharp_less_than_reader);
*/
	dtab['|'] = make_cf3(sharp_vertical_bar_reader);
	/*  This is specific to this implementation  */
	dtab['$'] = make_cf3(sharp_dollar_reader);
	/*  This is specific to this implimentation  */
/*
	dtab[' '] = dtab['\t'] = dtab['\n'] = dtab['\f']
	= make_cf3(sharp_whitespace_reader);
	dtab[')'] = make_cf3(sharp_right_parenthesis_reader);
*/

	init_backq();

	ECL_SET(@'*readtable*',
		readtable=copy_readtable(cl_core.standard_readtable, Cnil));
	readtable->readtable.table['#'].dispatch_table['!']
	    = cl_core.default_dispatch_macro; /*  We must forget #! macro.  */
	ECL_SET(@'*read_default_float_format*', @'single-float');
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
cl_object
read_VV(cl_object block, void *entry)
{
	volatile cl_object old_eptbc = cl_core.packages_to_be_created;
	typedef void (*entry_point_ptr)(cl_object);
	volatile cl_object x;
	cl_index i, len;
	cl_object in;
	entry_point_ptr entry_point = (entry_point_ptr)entry;
	cl_object *VV;

	if (block == NULL)
		block = cl_alloc_object(t_codeblock);
	block->cblock.links = Cnil;

	in = OBJNULL;
	CL_UNWIND_PROTECT_BEGIN {
		bds_bind(@'si::*cblock*', block);
		if (cl_core.packages_to_be_created == OBJNULL)
			cl_core.packages_to_be_created = Cnil;

		/* Communicate the library which Cblock we are using, and get
		 * back the amount of data to be processed.
		 */
		(*entry_point)(block);
		len = block->cblock.data_size;
#ifdef ECL_DYNAMIC_VV
		VV = block->cblock.data = len? (cl_object *)cl_alloc(len * sizeof(cl_object)) : NULL;
#else
		VV = block->cblock.data;
#endif
		if (len == 0) goto NO_DATA;

		/* Read all data for the library */
		in=make_string_input_stream(make_constant_string(block->cblock.data_text),
					    0, block->cblock.data_text_size);
		bds_bind(@'*read-base*', MAKE_FIXNUM(10));
		bds_bind(@'*read-default-float-format*', @'single-float');
		bds_bind(@'*read-suppress*', Cnil);
		bds_bind(@'*readtable*', cl_core.standard_readtable);
		bds_bind(@'*package*', cl_core.lisp_package);
		bds_bind(@'si::*sharp-eq-context*', Cnil);
		for (i = 0 ; i < len; i++) {
			x = read_object(in);
			if (x == OBJNULL)
				break;
			VV[i] = x;
		}
		if (!Null(SYM_VAL(@'si::*sharp-eq-context*'))) {
			while (i--) {
				VV[i] = patch_sharp(VV[i]);
			}
		}
		bds_unwind_n(6);
		if (i < len)
			FEreader_error("Not enough data while loading binary file", in, 0);
	NO_DATA:
		/* Execute top-level code */
		(*entry_point)(MAKE_FIXNUM(0));
		x = cl_core.packages_to_be_created;
		loop_for_on(x) {
			if (!member(x, old_eptbc)) {
				CEerror("The following package was referenced in a~"
				"compiled file, but has not been created: ~A",
				2, block->cblock.name, CAR(x));
			}
		} end_loop_for_on;
		bds_unwind1();
	} CL_UNWIND_PROTECT_EXIT {
		if (in != OBJNULL)
			close_stream(in, 0);
	} CL_UNWIND_PROTECT_END;

	return block;
}

