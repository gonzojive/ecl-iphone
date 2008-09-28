/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    print.d -- Print.
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
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <float.h>
#include <math.h>
#ifdef _MSC_VER
# undef complex
#endif
#include <ctype.h>
#ifndef _MSC_VER
#include <unistd.h>
#endif
#if defined(HAVE_FENV_H)
# include <fenv.h>
#endif
#include <ecl/internal.h>
#include <ecl/bytecodes.h>

#if defined(ECL_CMU_FORMAT)
# define si_write_object_recursive(x,y) si_write_object(x,y)
#else
static cl_object si_write_object_recursive(cl_object, cl_object);
#endif

/**********************************************************************/
/*		 SUPPORT FOR OLD KCL PRETTY PRINTER		      */
/**********************************************************************/

#if defined(ECL_CMU_FORMAT)
#define WRITE_MARK(s)
#define WRITE_UNMARK(s)
#define WRITE_SET_INDENT(s)
#define INDENT		' '
#define INDENT1		' '
#define INDENT2		' '
#define write_ch	ecl_write_char
#define call_print_object(x,s)	funcall(3, @'print-object',(x),(s))
#define call_structure_print_function(f,x,s) funcall(4,(f),(x),(s),MAKE_FIXNUM(0))
#endif /* ECL_CMU_FORMAT */

#if !defined(ECL_CMU_FORMAT)
#define LINE_LENGTH 72
#define	MARK		0400
#define	UNMARK		0401
#define	SET_INDENT	0402
#define	INDENT		0403
#define	INDENT1		0404
#define	INDENT2		0405
#define	mod(x)		((x)%ECL_PPRINT_QUEUE_SIZE)
#define WRITE_MARK(s) write_ch(MARK,s)
#define WRITE_UNMARK(s)	write_ch(UNMARK,s)
#define WRITE_SET_INDENT(s) write_ch(SET_INDENT,s)
static void flush_queue(bool force, cl_object stream);

static void
writec_queue(int c, cl_object stream)
{
	if (cl_env.qc >= ECL_PPRINT_QUEUE_SIZE)
		flush_queue(FALSE, stream);
	if (cl_env.qc >= ECL_PPRINT_QUEUE_SIZE)
		FEerror("Can't pretty-print.", 0);
	cl_env.queue[cl_env.qt] = c;
	cl_env.qt = mod(cl_env.qt+1);
	cl_env.qc++;
}

static void
flush_queue(bool force, cl_object stream)
{
	int c, i, j, k, l, i0;

BEGIN:
	while (cl_env.qc > 0) {
		c = cl_env.queue[cl_env.qh];
		if (c < 0400) {
			ecl_write_char(c, stream);
		} else if (c == MARK)
			goto DO_MARK;
		else if (c == UNMARK)
			cl_env.isp -= 2;
		else if (c == SET_INDENT)
			cl_env.indent_stack[cl_env.isp] = ecl_file_column(stream);
		else if (c == INDENT) {
			goto DO_INDENT;
		} else if (c == INDENT1) {
			i = ecl_file_column(stream)-cl_env.indent_stack[cl_env.isp];
			if (i < 8 && cl_env.indent_stack[cl_env.isp] < LINE_LENGTH/2) {
				ecl_write_char(' ', stream);
				cl_env.indent_stack[cl_env.isp]
				= ecl_file_column(stream);
			} else {
				if (cl_env.indent_stack[cl_env.isp] < LINE_LENGTH/2) {
					cl_env.indent_stack[cl_env.isp]
					= cl_env.indent_stack[cl_env.isp-1] + 4;
				}
				goto DO_INDENT;
			}
		} else if (c == INDENT2) {
			cl_env.indent_stack[cl_env.isp] = cl_env.indent_stack[cl_env.isp-1] + 2;
			goto PUT_INDENT;
		}
		cl_env.qh = mod(cl_env.qh+1);
		--cl_env.qc;
	}
	return;

DO_MARK:
	k = LINE_LENGTH - 1 - ecl_file_column(stream);
	for (i = 1, j = 0, l = 1;  l > 0 && i < cl_env.qc && j < k;  i++) {
		c = cl_env.queue[mod(cl_env.qh + i)];
		if (c == MARK)
			l++;
		else if (c == UNMARK)
			--l;
		else if (c == INDENT || c == INDENT1 || c == INDENT2)
			j++;
		else if (c < 0400)
			j++;
	}
	if (l == 0)
		goto FLUSH;
	if (i == cl_env.qc && !force)
		return;
	cl_env.qh = mod(cl_env.qh+1);
	--cl_env.qc;
	if (cl_env.isp >= ECL_PPRINT_INDENTATION_STACK_SIZE-2)
		FEerror("Can't pretty-print.", 0);
	cl_env.isp+=2;
	cl_env.indent_stack[cl_env.isp-1] = ecl_file_column(stream);
	cl_env.indent_stack[cl_env.isp] = cl_env.indent_stack[cl_env.isp-1];
	goto BEGIN;

DO_INDENT:
	if (cl_env.iisp > cl_env.isp)
		goto PUT_INDENT;
	k = LINE_LENGTH - 1 - ecl_file_column(stream);
	for (i0 = 0, i = 1, j = 0, l = 1;  i < cl_env.qc && j < k;  i++) {
		c = cl_env.queue[mod(cl_env.qh + i)];
		if (c == MARK)
			l++;
		else if (c == UNMARK) {
			if (--l == 0)
				goto FLUSH;
		} else if (c == SET_INDENT) {
			if (l == 1)
				break;
		} else if (c == INDENT) {
			if (l == 1)
				i0 = i;
			j++;
		} else if (c == INDENT1) {
			if (l == 1)
				break;
			j++;
		} else if (c == INDENT2) {
			if (l == 1) {
				i0 = i;
				break;
			}
			j++;
		} else if (c < 0400)
			j++;
	}
	if (i == cl_env.qc && !force)
		return;
	if (i0 == 0)
		goto PUT_INDENT;
	i = i0;
	goto FLUSH;

PUT_INDENT:
	cl_env.qh = mod(cl_env.qh+1);
	--cl_env.qc;
	ecl_write_char('\n', stream);
	for (i = cl_env.indent_stack[cl_env.isp];  i > 0;  --i)
		ecl_write_char(' ', stream);
	cl_env.iisp = cl_env.isp;
	goto BEGIN;

FLUSH:
	for (j = 0;  j < i;  j++) {
		c = cl_env.queue[cl_env.qh];
		if (c == INDENT || c == INDENT1 || c == INDENT2)
			ecl_write_char(' ', stream);
		else if (c < 0400)
			ecl_write_char(c, stream);
		cl_env.qh = mod(cl_env.qh+1);
		--cl_env.qc;
	}
	goto BEGIN;
}

static void
write_ch(int c, cl_object stream)
{
	if (cl_env.print_pretty)
		writec_queue(c, stream);
	else if (c == INDENT || c == INDENT1)
		ecl_write_char(' ', stream);
	else if (c < 0400)
		ecl_write_char(c, stream);
}

static void
#ifdef CLOS
call_print_object(cl_object x, cl_object stream)
#else
call_structure_print_function(cl_object f, cl_object x, cl_object stream)
#endif
{
	short ois[ECL_PPRINT_INDENTATION_STACK_SIZE];
	volatile bool p = cl_env.print_pretty;
	volatile int oqh, oqt, oqc, oisp, oiisp;

	if ((p = cl_env.print_pretty)) {
		flush_queue(TRUE, stream);
		oqh = cl_env.qh;
		oqt = cl_env.qt;
		oqc = cl_env.qc;
		oisp = cl_env.isp;
		oiisp = cl_env.iisp;
		memcpy(ois, cl_env.indent_stack, cl_env.isp * sizeof(*ois));
	}
	CL_UNWIND_PROTECT_BEGIN {
#ifdef CLOS
		funcall(3, @'print-object', x, stream);
#else
		funcall(4, f, x, stream, MAKE_FIXNUM(0));
#endif
	} CL_UNWIND_PROTECT_EXIT {
		if ((cl_env.print_pretty = p)) {
			memcpy(cl_env.indent_stack, ois, oisp * sizeof(*ois));
			cl_env.iisp = oiisp;
			cl_env.isp = oisp;
			cl_env.qc = oqc;
			cl_env.qt = oqt;
			cl_env.qh = oqh;
		}
	} CL_UNWIND_PROTECT_END;
}
#endif /* !ECL_CMU_FORMAT */

/**********************************************************************/

#define	to_be_escaped(c) \
	(cl_core.standard_readtable->readtable.table[(c)&0377].syntax_type \
	 != cat_constituent || \
	 islower((c)&0377) || (c) == ':')

static bool object_will_print_as_hash(cl_object x);
static cl_fixnum search_print_circle(cl_object x);
static bool potential_number_p(cl_object s, int base);

static void FEprint_not_readable(cl_object x) /*__attribute__((noreturn))*/;

static void
FEprint_not_readable(cl_object x)
{
	cl_error(3, @'print-not-readable', @':object', x);
}

static cl_object
stream_or_default_output(cl_object stream)
{
	if (Null(stream))
		return SYM_VAL(@'*standard-output*');
	else if (stream == Ct)
		return SYM_VAL(@'*terminal-io*');
	return stream;
}

cl_fixnum
ecl_print_base(void)
{
	cl_object object = ecl_symbol_value(@'*print-base*');
	cl_fixnum base;
	if (!FIXNUMP(object) || (base = fix(object)) < 2 || base > 36) {
		ECL_SETQ(@'*print-base*', MAKE_FIXNUM(10));
		FEerror("~S is an illegal PRINT-BASE.", 1, object);
	}
	return base;
}

cl_fixnum
ecl_print_level(void)
{
	cl_object object = ecl_symbol_value(@'*print-level*');
	cl_fixnum level;
	if (object == Cnil) {
		level = MOST_POSITIVE_FIXNUM;
	} else if (FIXNUMP(object)) {
		level = fix(object);
		if (level < 0) {
		ERROR:	ECL_SETQ(@'*print-level*', Cnil);
			FEerror("~S is an illegal PRINT-LEVEL.", 1, object);
		}
	} else if (type_of(object) != t_bignum) {
		goto ERROR;
	} else {
		level = MOST_POSITIVE_FIXNUM;
	}
	return level;
}

cl_fixnum
ecl_print_length(void)
{
	cl_object object = ecl_symbol_value(@'*print-length*');
	cl_fixnum length;
	if (object == Cnil) {
		length = MOST_POSITIVE_FIXNUM;
	} else if (FIXNUMP(object)) {
		length = fix(object);
		if (length < 0) {
		ERROR:	ECL_SETQ(@'*print-length*', Cnil);
			FEerror("~S is an illegal PRINT-LENGTH.", 1, object);
		}
	} else if (type_of(object) != t_bignum) {
		goto ERROR;
	} else {
		length = MOST_POSITIVE_FIXNUM;
	}
	return length;
}

bool
ecl_print_radix(void)
{
	return ecl_symbol_value(@'*print-radix*') != Cnil;
}

cl_object
ecl_print_case(void)
{
	cl_object output = ecl_symbol_value(@'*print-case*');
	if (output != @':upcase' && output != @':downcase' &&
	    output != @':capitalize') {
		ECL_SETQ(@'*print-case*', @':downcase');
		FEerror("~S is an illegal PRINT-CASE.", 1, output);
	}
	return output;
}

bool
ecl_print_gensym(void)
{
	return ecl_symbol_value(@'*print-gensym*') != Cnil;
}

bool
ecl_print_array(void)
{
	return ecl_symbol_value(@'*print-array*') != Cnil;
}

bool
ecl_print_readably(void)
{
	return ecl_symbol_value(@'*print-readably*') != Cnil;
}

bool
ecl_print_escape(void)
{
	return ecl_symbol_value(@'*print-escape*') != Cnil;
}

bool
ecl_print_circle(void)
{
	return ecl_symbol_value(@'*print-circle*') != Cnil;
}

static void
write_str(const char *s, cl_object stream)
{
	while (*s != '\0')
		write_ch(*s++, stream);
}

static void
write_positive_fixnum(cl_index i, int base, cl_index len, cl_object stream)
{
	/* The maximum number of digits is achieved for base 2 and it
	   is always < FIXNUM_BITS, since we use at least one bit for
	   tagging */
	short digits[FIXNUM_BITS];
	int j = 0;
	if (i == 0) {
		digits[j++] = '0';
	} else do {
		digits[j++] = ecl_digit_char(i % base, base);
		i /= base;
	} while (i > 0);
	while (len-- > j)
		write_ch('0', stream);
	while (j-- > 0)
		write_ch(digits[j], stream);
}

static void
write_decimal(cl_fixnum i, cl_object stream)
{
	write_positive_fixnum(i, 10, 0, stream);
}

static void
write_addr(cl_object x, cl_object stream)
{
	cl_fixnum i, j;

	i = (cl_index)x;
	for (j = sizeof(i)*8-4;  j >= 0;  j -= 4) {
		int k = (i>>j) & 0xf;
		if (k < 10)
			write_ch('0' + k, stream);
		else
			write_ch('a' + k - 10, stream);
	}
}

static void
write_base(int base, cl_object stream)
{
	if (base == 2)
		write_str("#b", stream);
	else if (base == 8)
		write_str("#o", stream);
	else if (base == 16)
		write_str("#x", stream);
	else if (base >= 10) {
		write_ch('#', stream);
		write_ch(base/10+'0', stream);
		write_ch(base%10+'0', stream);
		write_ch('r', stream);
	} else {
		write_ch('#', stream);
		write_ch(base+'0', stream);
		write_ch('r', stream);
	}
}

/* The floating point precision is required to make the
   most-positive-long-float printed expression readable.
   If this is too small, then the rounded off fraction, may be too big
   to read */

/* Maximum number of significant digits required to represent accurately
 * a double or single float. */

#define LOG10_2 0.30103
#define DBL_SIG ((int)(DBL_MANT_DIG * LOG10_2 + 1))
#define FLT_SIG ((int)(FLT_MANT_DIG * LOG10_2 + 1))

/* This is the maximum number of decimal digits that our numbers will have.
 * Notice that we leave some extra margin, to ensure that reading the number
 * again will produce the same floating point number.
 */
#ifdef ECL_LONG_FLOAT
# define LDBL_SIG ((int)(LDBL_MANT_DIG * LOG10_2 + 1))
# define DBL_MAX_DIGITS (LDBL_SIG + 3)
# define DBL_EXPONENT_SIZE (1 + 1 + 4)
#else
# define DBL_MAX_DIGITS (DBL_SIG + 3)
# define DBL_EXPONENT_SIZE (1 + 1 + 3) /* Exponent marker 'e' + sign + digits .*/
#endif

/* The sinificant digits + the possible sign + the decimal dot. */
#define DBL_MANTISSA_SIZE (DBL_MAX_DIGITS + 1 + 1)
/* Total estimated size that a floating point number can take. */
#define DBL_SIZE (DBL_MANTISSA_SIZE + DBL_EXPONENT_SIZE)

#ifdef ECL_LONG_FLOAT
#define EXP_STRING "Le"
#define G_EXP_STRING "Lg"
#define DBL_TYPE long double
#define strtod strtold
extern long double strtold(const char *nptr, char **endptr);
#else
#define EXP_STRING "e"
#define G_EXP_STRING "g"
#define DBL_TYPE double
#endif

int edit_double(int n, DBL_TYPE d, int *sp, char *s, int *ep)
{
	char *exponent, *p, buff[DBL_SIZE + 1];
	int length;
#if defined(HAVE_FENV_H) || defined(_MSC_VER) || defined(mingw32)
	fenv_t env;
	feholdexcept(&env);
#endif
	if (isnan(d) || !isfinite(d))
		FEerror("Can't print a non-number.", 0);
	if (n < -DBL_MAX_DIGITS)
		n = DBL_MAX_DIGITS;
	if (n < 0) {
		DBL_TYPE aux;
		n = -n;
		do {
			sprintf(buff, "%- *.*" EXP_STRING, n + 1 + 1 + DBL_EXPONENT_SIZE, n-1, d);
			aux = strtod(buff, NULL);
#ifdef ECL_LONG_FLOAT
			if (n < LDBL_SIG)
				aux = (double) aux;
#endif
			if (n < DBL_SIG)
				aux = (float)aux;
			n++;
		} while (d != aux && n <= DBL_MAX_DIGITS);
		n--;
	} else {
		sprintf(buff, "%- *.*" EXP_STRING, DBL_SIZE,
			(n <= DBL_MAX_DIGITS)? (n-1) : (DBL_MAX_DIGITS-1), d);
	}
	exponent = strchr(buff, 'e');

	/* Get the exponent */
	*ep = strtol(exponent+1, NULL, 10);

	/* Get the sign */
	*sp = (buff[0] == '-') ? -1 : +1;

	/* Get the digits of the mantissa */
	buff[2] = buff[1];

	/* Get the actual number of digits in the mantissa */
	length = exponent - (buff + 2);

	/* The output consists of a string {d1,d2,d3,...,dn}
	   with all N digits of the mantissa. If we ask for more
	   digits than there are, the last ones are set to zero. */
	if (n <= length) {
		memcpy(s, buff+2, n);
	} else {
		cl_index i;
		memcpy(s, buff+2, length);
		for (i = length;  i < n;  i++)
			s[i] = '0';
	}
	s[n] = '\0';
#if defined(HAVE_FENV_H) || defined(_MSC_VER) || defined(mingw32)
	fesetenv(&env);
#endif
	return length;
}

static void
write_double(DBL_TYPE d, int e, int n, cl_object stream)
{
	int exp;
#if defined(HAVE_FENV_H) || defined(_MSC_VER) || defined(mingw32)
	fenv_t env;
	feholdexcept(&env);
#endif
	if (d < 0) {
		write_ch('-', stream);
		d = -d;
	}
	if (d == 0.0) {
#ifdef signbit
		if (signbit(d))
			write_str("-0.0", stream);
		else
#endif
			write_str("0.0", stream);
		exp = 0;
	} else if (d < 1e-3 || d > 1e7) {
		int sign;
		char buff[DBL_MANTISSA_SIZE + 1];
		n = edit_double(-n, d, &sign, buff, &exp);
		write_ch(buff[0], stream);
		write_ch('.', stream);
		for (;  --n > 1; ) {
			if (buff[n] != '0') {
				break;
			}
			buff[n] = '\0';
		}
		write_str(buff+1, stream);
	} else {
		char buff[DBL_MANTISSA_SIZE + 1];
		int i;
		DBL_TYPE aux;
		/* Print in fixed point notation with enough number of
		 * digits to preserve all information when reading again
		 */
		do {
			sprintf(buff, "%0*.*" G_EXP_STRING, DBL_MANTISSA_SIZE, n, d);
			aux = strtod(buff, NULL);
#ifdef LDBL_SIG
			if (n < LDBL_SIG) aux = (double)aux;
#endif
			if (n < DBL_SIG) aux = (float)aux;
			n++;
		} while (aux != d && n <= DBL_MAX_DIGITS);
		n--;
		/* We look for the first nonzero character. There is
		 * always one because our floating point number is not
		 * zero.*/
		for (i = 0; buff[i] == '0' && buff[i+1] != '.'; i++)
			;
		write_str(buff + i, stream);
		if (strchr(buff, '.') == 0) {
			write_str(".0", stream);
		}
		exp = 0;
	}
	if (exp || e) {
		if (e == 0)
			e = 'E';
		write_ch(e, stream);
		if (exp < 0) {
			write_ch('-', stream);
			exp = -exp;
		}
		write_decimal(exp, stream);
	}
#if defined(HAVE_FENV_H) || defined(_MSC_VER) || defined(mingw32)
	fesetenv(&env);
#endif
}


#ifdef WITH_GMP

struct powers {
	cl_object number;
	cl_index n_digits;
	int base;
};

static void
do_write_integer(cl_object x, struct powers *powers, cl_index len,
		 cl_object stream)
{
	cl_object left;
	do {
		if (FIXNUMP(x)) {
			write_positive_fixnum(fix(x), powers->base, len, stream);
			return;
		}
		while (ecl_number_compare(x, powers->number) < 0) {
			if (len)
				write_positive_fixnum(0, powers->base, len, stream);
			powers--;
		}
		left = ecl_floor2(x, powers->number);
		x = VALUES(1);
		if (len) len -= powers->n_digits;
		do_write_integer(left, powers-1, len, stream);
		len = powers->n_digits;
		powers--;
	} while(1);
}

static void
write_bignum(cl_object x, cl_object stream)
{
	int base = ecl_print_base();
	cl_index str_size = mpz_sizeinbase(x->big.big_num, base);
	cl_fixnum num_powers = ecl_fixnum_bit_length(str_size-1);
#ifdef __GNUC__
	struct powers powers[num_powers];
#else
	struct powers *powers = (struct powers*)malloc(sizeof(struct powers)*num_powers);
	CL_UNWIND_PROTECT_BEGIN {
#endif
		cl_object p;
		cl_index i, n_digits;
		powers[0].number = p = MAKE_FIXNUM(base);
		powers[0].n_digits = n_digits = 1;
		powers[0].base = base;
		for (i = 1; i < num_powers; i++) {
			powers[i].number = p = ecl_times(p, p);
			powers[i].n_digits = n_digits = 2*n_digits;
			powers[i].base = base;
		}
		if (ecl_minusp(x)) {
			write_ch('-', stream);
			x = ecl_negate(x);
		}
		do_write_integer(x, &powers[num_powers-1], 0, stream);
#ifndef __GNUC__
	} CL_UNWIND_PROTECT_EXIT {
		free(powers);
	} CL_UNWIND_PROTECT_END;
#endif
}

#else  /* WITH_GMP */

static void
write_positive_bignum(big_num_t x, cl_object stream)
{
	/* The maximum number of digits is achieved for base 2 and it
	   is always < 8*sizeof(big_num_t) */
	int base = ecl_print_base();
	short digits[8*sizeof(big_num_t)];
	int j = 0;
	if (x == (big_num_t)0) {
		digits[j++] = '0';
	} else do {
		digits[j++] = ecl_digit_char((cl_fixnum)(x % (big_num_t)base), base);
		x /= base;
	} while (x > (big_num_t)0);
	/* while (len-- > j)
           write_ch('0', stream); */
	while (j-- > 0)
		write_ch(digits[j], stream);
}
#endif /* WITH_GMP */

static bool
all_dots(cl_object s)
{
	cl_index i;
	for (i = 0;  i < s->base_string.fillp;  i++)
		if (s->base_string.self[i] != '.')
			return 0;
	return 1;
}

static bool
needs_to_be_escaped(cl_object s, cl_object readtable, cl_object print_case)
{
	int action = readtable->readtable.read_case;
	bool all_dots;
	cl_index i;
	if (potential_number_p(s, ecl_print_base()))
		return 1;
	/* The value of *PRINT-ESCAPE* is T. We need to check whether the
	 * symbol name S needs to be escaped. This will happen if it has some
	 * strange character, or if it has a lowercase character (because such
	 * a character cannot be read with the standard readtable) or if the
	 * string has to be escaped according to readtable case and the rules
	 * of 22.1.3.3.2. */
	for (i = 0; i < s->base_string.fillp;  i++) {
		int c = s->base_string.self[i] & 0377;
		int syntax = readtable->readtable.table[c].syntax_type;
		if (syntax != cat_constituent || ecl_invalid_character_p(c) || (c) == ':')
			return 1;
		if ((action == ecl_case_downcase) && isupper(c))
			return 1;
		if (islower(c))
			return 1;
	}
	return 0;
}

#define needs_to_be_inverted(s) (ecl_string_case(s) != 0)

static void
write_symbol_string(cl_object s, int action, cl_object print_case,
		    cl_object stream, bool escape)
{
	cl_index i;
	bool capitalize;
	if (action == ecl_case_invert) {
		if (!needs_to_be_inverted(s))
			action = ecl_case_preserve;
	}
	if (escape)
		write_ch('|', stream);
	capitalize = 1;
	for (i = 0;  i < s->base_string.fillp;  i++) {
		int c = s->base_string.self[i];
		if (escape) {
			if (c == '|' || c == '\\') {
				write_ch('\\', stream);
			}
		} else if (action != ecl_case_preserve) {
			if (isupper(c)) {
				if ((action == ecl_case_invert) ||
				    ((action == ecl_case_upcase) &&
				     ((print_case == @':downcase') ||
				      ((print_case == @':capitalize') && !capitalize))))
				{
					c = tolower(c);
				}
				capitalize = 0;
			} else if (islower(c)) {
				if ((action == ecl_case_invert) ||
				    ((action == ecl_case_downcase) &&
				     ((print_case == @':upcase') ||
				      ((print_case == @':capitalize') && capitalize))))
				{
					c = toupper(c);
				}
				capitalize = 0;
			} else {
				capitalize = !isdigit(c);
			}
		}
		write_ch(c, stream);
	}
	if (escape)
		write_ch('|', stream);
}

static void
write_symbol(cl_object x, cl_object stream)
{
	cl_object print_package = ecl_symbol_value(@'si::*print-package*');
	cl_object readtable = ecl_current_readtable();
	cl_object print_case = ecl_print_case();
	cl_object package;
	cl_object name;
	int intern_flag;
	bool print_readably = ecl_print_readably();

	if (Null(x)) {
		package = Cnil_symbol->symbol.hpack;
		name = Cnil_symbol->symbol.name;
	} else {
		package = x->symbol.hpack;
		name = x->symbol.name;
	}

	if (!print_readably && !ecl_print_escape()) {
		write_symbol_string(name, readtable->readtable.read_case,
				    print_case, stream, 0);
		return;
	}
	/* From here on, print-escape is true which means that it should
	 * be possible to recover the same symbol by reading it with
	 * the standard readtable (which has readtable-case = :UPCASE)
	 */
	if (Null(package)) {
		if (ecl_print_gensym() || print_readably)
			write_str("#:", stream);
	} else if (package == cl_core.keyword_package) {
		write_ch(':', stream);
	} else if ((print_package != Cnil && package != print_package)
		   || ecl_find_symbol(x, ecl_current_package(), &intern_flag)!=x
		   || intern_flag == 0)
	{
		cl_object name = package->pack.name;
		write_symbol_string(name, readtable->readtable.read_case,
				    print_case, stream,
				    needs_to_be_escaped(name, readtable, print_case));
		if (ecl_find_symbol(x, package, &intern_flag) != x)
			ecl_internal_error("can't print symbol");
		if ((print_package != Cnil && package != print_package)
		    || intern_flag == INTERNAL) {
			write_str("::", stream);
		} else if (intern_flag == EXTERNAL) {
			write_ch(':', stream);
		} else {
			FEerror("Pathological symbol --- cannot print.", 0);
		}
	}
	write_symbol_string(name, readtable->readtable.read_case, print_case, stream,
			    needs_to_be_escaped(name, readtable, print_case) ||
			    all_dots(name));
}

static void
write_character(int i, cl_object stream)
{
	if (!ecl_print_escape() && !ecl_print_readably()) {
		write_ch(i, stream);
	} else {
		write_str("#\\", stream);
		if (i < 32 || i == 127) {
			cl_object name = cl_char_name(CODE_CHAR(i));
			write_str(name->base_string.self, stream);
		} else if (i >= 128) {
                        int  index = 0;
			char name[20];
			sprintf(name, "U%04x", i); /* cleanup */
                        while(name[index])
				write_ch(name[index++], stream);
		} else {
			write_ch(i, stream);
		}
	}
}

static void
write_array(bool vector, cl_object x, cl_object stream)
{
	const cl_index *adims;
	cl_index subscripts[ARANKLIM];
	cl_fixnum n, j, m, k, i;
	cl_fixnum print_length;
	cl_fixnum print_level;
	bool readably = ecl_print_readably();

	if (vector) {
		adims = &x->vector.fillp;
		n = 1;
	} else {
		adims = x->array.dims;
		n = x->array.rank;
	}
	if (readably) {
		print_length = MOST_POSITIVE_FIXNUM;
		print_level = MOST_POSITIVE_FIXNUM;
	} else {
		if (!ecl_print_array()) {
			write_str(vector? "#<vector " : "#<array ", stream);
			write_addr(x, stream);
			write_ch('>', stream);
			return;
		}
		print_level = ecl_print_level();
		print_length = ecl_print_length();
	}
	write_ch('#', stream);
	if (print_level == 0)
		return;
	if (readably) {
		write_ch('A', stream);
		write_ch('(', stream);
		si_write_object_recursive(ecl_elttype_to_symbol(ecl_array_elttype(x)), stream);
		write_ch(INDENT, stream);
		if (n > 0) {
			write_ch('(', stream);
			for (j=0; j<n; j++) {
				si_write_object_recursive(MAKE_FIXNUM(adims[j]), stream);
				if (j < n-1)
					write_ch(INDENT, stream);
			}
			write_ch(')', stream);
		} else {
			si_write_object_recursive(Cnil, stream);
		}
		write_ch(INDENT, stream);
	} else if (!vector) {
		write_decimal(n, stream);
		write_ch('A', stream);
	}
	if (print_level >= n) {
		/* We can write the elements of the array */
		print_level -= n;
		bds_bind(@'*print-level*', MAKE_FIXNUM(print_level));
	} else {
		/* The elements of the array are not printed */
		n = print_level;
		print_level = -1;
	}
	for (j = 0;  j < n;  j++)
		subscripts[j] = 0;
	for (m = 0, j = 0;;) {
		for (i = j;  i < n;  i++) {
			if (subscripts[i] == 0) {
				WRITE_MARK(stream);
				write_ch('(', stream);
				WRITE_SET_INDENT(stream);
				if (adims[i] == 0) {
					write_ch(')', stream);
					WRITE_UNMARK(stream);
					j = i-1;
					k = 0;
					goto INC;
				}
			}
			if (subscripts[i] > 0)
				write_ch(INDENT, stream);
			if (subscripts[i] >= print_length) {
				write_str("...)", stream);
				WRITE_UNMARK(stream);
				k=adims[i]-subscripts[i];
				subscripts[i] = 0;
				for (j = i+1;  j < n;  j++)
					k *= adims[j];
				j = i-1;
				goto INC;
			}
		}
		/* FIXME: This conses! */
		if (print_level >= 0)
			si_write_object_recursive(ecl_aref(x, m), stream);
		else
			write_ch('#', stream);
		j = n-1;
		k = 1;

	INC:
		while (j >= 0) {
			if (++subscripts[j] < adims[j])
				break;
			subscripts[j] = 0;
			write_ch(')', stream);
			WRITE_UNMARK(stream);
			--j;
		}
		if (j < 0)
			break;
		m += k;
	}
	if (print_level >= 0) {
		bds_unwind1();
	}
	if (readably) {
		write_ch(')', stream);
	}
}

cl_object
si_write_ugly_object(cl_object x, cl_object stream)
{
	cl_object r, y;
	cl_fixnum i, j;
	cl_index ndx, k;

	if (x == OBJNULL) {
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str("#<OBJNULL>", stream);
		goto OUTPUT;
	}
	switch (type_of(x)) {

	case FREE:
		write_str("#<FREE OBJECT ", stream);
		write_addr(x, stream);
		write_ch('>', stream);
		break;

	case t_fixnum: {
		bool print_radix = ecl_print_radix();
		int print_base = ecl_print_base();
		if (print_radix && print_base != 10)
			write_base(print_base, stream);
		if (x == MAKE_FIXNUM(0)) {
			write_ch('0', stream);
		} else if (FIXNUM_MINUSP(x)) {
			write_ch('-', stream);
			write_positive_fixnum(-fix(x), print_base, 0, stream);
		} else {
			write_positive_fixnum(fix(x), print_base, 0, stream);
		}
		if (print_radix && print_base == 10) {
			write_ch('.', stream);
		}
		break;
	}
	case t_bignum: {
		bool print_radix = ecl_print_radix();
		int print_base = ecl_print_base();
		if (print_radix && print_base != 10)
			write_base(print_base, stream);
#ifdef WITH_GMP
		write_bignum(x, stream);
#else  /* WITH_GMP */
                if ( big_zerop(x) ) {
                        write_ch('0', stream);
                } else if ( big_sign(x) < 0 ) {
                        write_ch('-', stream);
                        write_positive_bignum(-(x->big.big_num), stream);
                } else {
                        write_positive_bignum(x->big.big_num, stream);
                }
#endif /* WITH_GMP */

		if (print_radix && print_base == 10)
			write_ch('.', stream);
		break;
	}
	case t_ratio:
		if (ecl_print_radix()) {
			write_base(ecl_print_base(), stream);
		}
		bds_bind(@'*print-radix*', Cnil);
		si_write_ugly_object(x->ratio.num, stream);
		write_ch('/', stream);
		si_write_ugly_object(x->ratio.den, stream);
		bds_unwind1();
		break;
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat:
		r = ecl_symbol_value(@'*read-default-float-format*');
		write_double(ecl_short_float(x), (r == @'short-float')? 0 : 'f', FLT_SIG, stream);
		break;
	case t_singlefloat:
		r = ecl_symbol_value(@'*read-default-float-format*');
		write_double(sf(x), (r == @'single-float')? 0 : 's', FLT_SIG, stream);
		break;
#else
	case t_singlefloat:
		r = ecl_symbol_value(@'*read-default-float-format*');
		write_double(sf(x), (r == @'single-float' || r == @'short-float')? 0 : 's', FLT_SIG, stream);
		break;
#endif
#ifdef ECL_LONG_FLOAT
	case t_doublefloat:
		r = ecl_symbol_value(@'*read-default-float-format*');
		write_double(df(x), (r == @'double-float')? 0 : 'd', DBL_SIG, stream);
		break;
	case t_longfloat:
		r = ecl_symbol_value(@'*read-default-float-format*');
		write_double(ecl_long_float(x), (r == @'long-float')? 0 : 'l', LDBL_SIG, stream);
		break;
#else
	case t_doublefloat:
		r = ecl_symbol_value(@'*read-default-float-format*');
		write_double(df(x), (r == @'double-float' || r == @'long-float')? 0 : 'd', DBL_SIG, stream);
		break;
#endif
	case t_complex:
		write_str("#C(", stream);
		si_write_ugly_object(x->complex.real, stream);
		write_ch(' ', stream);
		si_write_ugly_object(x->complex.imag, stream);
		write_ch(')', stream);
		break;

	case t_character: {
		write_character(CHAR_CODE(x), stream);
		}
		break;

	case t_symbol:
		write_symbol(x, stream);
		break;

	case t_array:
		write_array(0, x, stream);
		break;

#ifdef ECL_UNICODE
	case t_string:
		if (!ecl_print_escape() && !ecl_print_readably()) {
			for (ndx = 0;  ndx < x->string.fillp;  ndx++)
				write_ch(CHAR_CODE(x->string.self[ndx]), stream);
			break;
		}
		write_ch('"', stream);
		for (ndx = 0;  ndx < x->string.fillp;  ndx++) {
			int c = CHAR_CODE(x->string.self[ndx]);
			if (c == '"' || c == '\\')
				write_ch('\\', stream);
			write_ch(c, stream);
		}
		write_ch('"', stream);
		break;
#endif
	case t_vector:
		write_array(1, x, stream);
		break;

	case t_base_string:
		if (!ecl_print_escape() && !ecl_print_readably()) {
			for (ndx = 0;  ndx < x->base_string.fillp;  ndx++)
				write_ch(x->base_string.self[ndx], stream);
			break;
		}
		write_ch('"', stream);
		for (ndx = 0;  ndx < x->base_string.fillp;  ndx++) {
			int c = x->base_string.self[ndx];
			if (c == '"' || c == '\\')
				write_ch('\\', stream);
			write_ch(c, stream);
		}
		write_ch('"', stream);
		break;

	case t_bitvector:
		if (!ecl_print_array() && !ecl_print_readably()) {
			write_str("#<bit-vector ", stream);
			write_addr(x, stream);
			write_ch('>', stream);
			break;
		}
		write_str("#*", stream);
		for (ndx = 0;  ndx < x->vector.fillp;  ndx++)
			if (x->vector.self.bit[(ndx+x->vector.offset)/8] & (0200 >> (ndx+x->vector.offset)%8))
				write_ch('1', stream);
			else
				write_ch('0', stream);
		break;

	case t_list: {
		bool circle;
		cl_fixnum print_level, print_length;
		if (Null(x)) {
			write_symbol(x, stream);
			break;
		}
		if (CAR(x) == @'si::#!') {
			write_str("#!", stream);
			x = CDR(x);
			return si_write_object_recursive(x, stream);
		}
		if (CONSP(CDR(x)) && Null(CDDR(x))) {
			if (CAR(x) == @'quote') {
				write_ch('\'', stream);
				x = CADR(x);
				return si_write_object_recursive(x, stream);
			}
			if (CAR(x) == @'function') {
				write_ch('#', stream);
				write_ch('\'', stream);
				x = CADR(x);
				return si_write_object_recursive(x, stream);
			}
			if (CAR(x) == @'si::quasiquote') {
				write_ch('`', stream);
				x = CADR(x);
				return si_write_object_recursive(x, stream);
			}
			if (CAR(x) == @'si::unquote') {
				write_ch(',', stream);
				x = CADR(x);
				return si_write_object_recursive(x, stream);
			}
			if (CAR(x) == @'si::unquote-splice') {
				write_str(",@@", stream);
				x = CADR(x);
				return si_write_object_recursive(x, stream);
			}
			if (CAR(x) == @'si::unquote-nsplice') {
				write_str(",.", stream);
				x = CADR(x);
				return si_write_object_recursive(x, stream);
			}
		}
		circle = ecl_print_circle();
		if (ecl_print_readably()) {
			print_level = MOST_POSITIVE_FIXNUM;
			print_length = MOST_POSITIVE_FIXNUM;
		} else {
			print_level = ecl_print_level();
			print_length = ecl_print_length();
		}
		if (print_level == 0) {
			write_ch('#', stream);
			break;
		}
		bds_bind(@'*print-level*', MAKE_FIXNUM(print_level-1));
		WRITE_MARK(stream);
		write_ch('(', stream);
		WRITE_SET_INDENT(stream);
#if !defined(ECL_CMU_FORMAT)
		if (cl_env.print_pretty && CAR(x) != OBJNULL &&
		    type_of(CAR(x)) == t_symbol &&
		    (r = si_get_sysprop(CAR(x), @'si::pretty-print-format')) != Cnil)
			goto PRETTY_PRINT_FORMAT;
#endif
		for (i = 0;  ;  i++) {
			if (i >= print_length) {
				write_str("...", stream);
				break;
			}
			y = CAR(x);
			x = CDR(x);
			si_write_object_recursive(y, stream);
			/* FIXME! */
			if (x == OBJNULL || ATOM(x) ||
			    (circle && object_will_print_as_hash(x)))
			{
				if (x != Cnil) {
					write_ch(INDENT, stream);
					write_str(". ", stream);
					si_write_object_recursive(x, stream);
				}
				break;
			}
			if (i == 0 && y != OBJNULL && type_of(y) == t_symbol)
				write_ch(INDENT1, stream);
			else
				write_ch(INDENT, stream);
		}
	RIGHT_PAREN:
		write_ch(')', stream);
		WRITE_UNMARK(stream);
		bds_unwind1();
		break;
#if !defined(ECL_CMU_FORMAT)
	PRETTY_PRINT_FORMAT:
		j = fixint(r);
		for (i = 0;  ;  i++) {
			if (i >= print_length) {
				write_str("...", stream);
				break;
			}
			y = CAR(x);
			x = CDR(x);
			if (i <= j && Null(y))
				write_str("()", stream);
			else
				si_write_object_recursive(y, stream);
			/* FIXME! */
			if (x == OBJNULL || ATOM(x) ||
			    (circle && object_will_print_as_hash(x))) {
				if (x != Cnil) {
					write_ch(INDENT, stream);
					write_str(". ", stream);
					si_write_object_recursive(x, stream);
				}
				break;
			}
			if (i >= j)
				write_ch(INDENT2, stream);
			else if (i == 0)
				write_ch(INDENT1, stream);
			else
				write_ch(INDENT, stream);
		}
		goto RIGHT_PAREN;
#endif
	}
	case t_package:
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str("#<", stream);
		si_write_ugly_object(x->pack.name, stream);
 		write_str(" package>", stream);
		break;

	case t_hashtable:
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str("#<hash-table ", stream);
		write_addr(x, stream);
		write_ch('>', stream);
		break;

	case t_stream:
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str(x->stream.closed? "#<closed " : "#<", stream);
		switch ((enum ecl_smmode)x->stream.mode) {
		case smm_input:
			write_str("input stream ", stream);
			si_write_ugly_object(x->stream.object1, stream);
			break;

		case smm_output:
			write_str("output stream ", stream);
			si_write_ugly_object(x->stream.object1, stream);
			break;

#ifdef _MSC_VER
		case smm_input_wsock:
			write_str("input win32 socket stream ", stream);
			si_write_ugly_object(x->stream.object1, stream);
			break;

		case smm_output_wsock:
			write_str("output win32 socket stream ", stream);
			si_write_ugly_object(x->stream.object1, stream);
			break;

		case smm_io_wsock:
			write_str("i/o win32 socket stream ", stream);
			si_write_ugly_object(x->stream.object1, stream);
			break;
#endif

		case smm_io:
			write_str("io stream ", stream);
			si_write_ugly_object(x->stream.object1, stream);
			break;

		case smm_probe:
			write_str("probe stream ", stream);
			si_write_ugly_object(x->stream.object1, stream);
			break;

		case smm_synonym:
			write_str("synonym stream to ", stream);
			si_write_ugly_object(x->stream.object0, stream);
			break;

		case smm_broadcast:
			write_str("broadcast stream ", stream);
			write_addr(x, stream);
			break;

		case smm_concatenated:
			write_str("concatenated stream ", stream);
			write_addr(x, stream);
			break;

		case smm_two_way:
			write_str("two-way stream ", stream);
			write_addr(x, stream);
			break;

		case smm_echo:
			write_str("echo stream ", stream);
			write_addr(x, stream);
			break;

		case smm_string_input:
			write_str("string-input stream from \"", stream);
			y = x->stream.object0;
			k = y->base_string.fillp;
			for (ndx = 0;  ndx < k && ndx < 16;  ndx++)
				write_ch(y->base_string.self[ndx], stream);
			if (k > 16)
				write_str("...", stream);
			write_ch('"', stream);
			break;

		case smm_string_output:
			write_str("string-output stream ", stream);
			write_addr(x, stream);
			break;

		default:
			ecl_internal_error("illegal stream mode");
		}
		write_ch('>', stream);
		break;

	case t_random:
		if (ecl_print_readably()) {
			write_str("#$", stream);
			write_array(1, x->random.value, stream);
		} else {
			write_str("#<random-state ", stream);
			write_addr(x->random.value, stream);
			write_str("#>", stream);
		}
		break;

#ifndef CLOS
	case t_structure: {
		cl_object print_function;
		if (type_of(x->str.name) != t_symbol)
			FEwrong_type_argument(@'symbol', x->str.name);
		print_function = si_get_sysprop(x->str.name, @'si::structure-print-function');
		if (Null(print_function) || !ecl_print_structure())
		{
			write_str("#S", stream);
/* structure_to_list conses slot names and values into a list to be printed.
 * print shouldn't allocate memory - Beppe
 */
			x = structure_to_list(x);
			si_write_object_recursive(x, stream);
		} else {
			call_structure_print_function(print_function, x, stream);
		}
		break;
	}
#endif /* CLOS */
	case t_readtable:
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str("#<readtable ", stream);
		write_addr(x, stream);
		write_ch('>', stream);
		break;

	case t_pathname: {
		cl_object namestring = ecl_namestring(x, 0);
		if (namestring == Cnil) {
			if (ecl_print_readably())
				FEprint_not_readable(x);
			namestring = ecl_namestring(x, 1);
			if (namestring == Cnil) {
				write_str("#<Unprintable pathname>", stream);
				break;
			}
		}
		if (ecl_print_escape() || ecl_print_readably())
			write_str("#P", stream);
		si_write_ugly_object(namestring, stream);
		break;
	}
	case t_bclosure:
                if ( ecl_print_readably() ) {
	                cl_index i;
			cl_object lex = x->bclosure.lex;
                        cl_object code_l=Cnil, data_l=Cnil;
			x = x->bclosure.code;
                        for ( i=x->bytecodes.code_size-1 ; i<(cl_index)(-1l) ; i-- )
                             code_l = ecl_cons(MAKE_FIXNUM(((cl_opcode*)(x->bytecodes.code))[i]), code_l);
                        for ( i=x->bytecodes.data_size-1 ; i<(cl_index)(-1l) ; i-- )
                             data_l = ecl_cons(x->bytecodes.data[i], data_l);

                        write_str("#Y", stream);
                        si_write_ugly_object(
			    cl_list(6, x->bytecodes.name, lex,
				    x->bytecodes.specials, Cnil /* x->bytecodes.definition */,
				    code_l, data_l),
			    stream);
			break;
                }
	case t_bytecodes:
                if ( ecl_print_readably() ) {
	                cl_index i;
			cl_object lex = Cnil;
                        cl_object code_l=Cnil, data_l=Cnil;
                        for ( i=x->bytecodes.code_size-1 ; i<(cl_index)(-1l) ; i-- )
                             code_l = ecl_cons(MAKE_FIXNUM(((cl_opcode*)(x->bytecodes.code))[i]), code_l);
                        for ( i=x->bytecodes.data_size-1 ; i<(cl_index)(-1l) ; i-- )
                             data_l = ecl_cons(x->bytecodes.data[i], data_l);

                        write_str("#Y", stream);
                        si_write_ugly_object(
			    cl_list(6, x->bytecodes.name, lex,
				    x->bytecodes.specials, Cnil /* x->bytecodes.definition */,
				    code_l, data_l),
			    stream);
			break;
                } else {
                        cl_object name = x->bytecodes.name;
                        write_str("#<bytecompiled-function ", stream);
                        if (name != Cnil)
                             si_write_ugly_object(name, stream);
                        else
                             write_addr(x, stream);
                        write_ch('>', stream);
                }
                break;
        case t_cfun:
	case t_cfunfixed:
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str("#<compiled-function ", stream);
		if (x->cfun.name != Cnil)
			si_write_ugly_object(x->cfun.name, stream);
		else
			write_addr(x, stream);
		write_ch('>', stream);
		break;
	case t_codeblock:
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str("#<codeblock ", stream);
		if (x->cblock.name != Cnil)
			si_write_ugly_object(x->cblock.name, stream);
		else
			write_addr(x, stream);
		write_ch('>', stream);
		break;
	case t_cclosure:
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str("#<compiled-closure ", stream);
		write_addr(x, stream);
		write_ch('>', stream);
		break;
#ifdef CLOS
	case t_instance:
		if (!ECL_INSTANCEP(CLASS_OF(x)))
			FEwrong_type_argument(@'ext::instance', CLASS_OF(x));
		call_print_object(x, stream);
		break;
#endif /* CLOS */
	case t_foreign:
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str("#<foreign ", stream);
		si_write_ugly_object(x->foreign.tag, stream);
		write_ch(' ', stream);
		write_addr((cl_object)x->foreign.data, stream);
		write_ch('>', stream);
		break;
	case t_frame:
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str("#<frame ", stream);
		write_decimal(x->frame.top - x->frame.bottom, stream);
		write_ch(' ', stream);
		write_decimal(x->frame.bottom, stream);
		write_ch('>', stream);
		break;
#ifdef ECL_THREADS
	case t_process:
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str("#<process ", stream);
                si_write_object_recursive(x->process.name, stream);
		write_ch(' ', stream);
		write_addr(x, stream);
		write_ch('>', stream);
		break;
	case t_lock:
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str("#<lock ", stream);
                if (!x->lock.recursive)
 		    write_str("(nonrecursive) ", stream);
                si_write_object_recursive(x->lock.name, stream);
		write_ch(' ', stream);
		write_addr(x, stream);
		write_ch('>', stream);
		break;
	case t_condition_variable:
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str("#<condition-variable ", stream);
		write_addr(x, stream);
		write_ch('>', stream);
		break;
#endif /* ECL_THREADS */
	default:
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str("#<illegal pointer ", stream);
		write_addr(x, stream);
		write_ch('>', stream);
	}
 OUTPUT:
	@(return x)
}

#if defined(ECL_CMU_FORMAT)
#undef si_write_object_recursive
cl_object
si_write_object(cl_object x, cl_object stream)
#else
static cl_object
si_write_object_recursive(cl_object x, cl_object stream)
#endif
{
	bool circle;
#if defined(ECL_CMU_FORMAT)
	if (ecl_symbol_value(@'*print-pretty*') != Cnil) {
		cl_object f = funcall(2, @'pprint-dispatch', x);
		if (VALUES(1) != Cnil) {
			funcall(3, f, stream, x);
			return x;
		}
	}
#endif /* ECL_CMU_FORMAT */
	circle = ecl_print_circle();
	if (circle && !Null(x) && !FIXNUMP(x) && !CHARACTERP(x) &&
	    (LISTP(x) || (x->d.t != t_symbol) || (Null(x->symbol.hpack))))
	{
		cl_object circle_counter;
		cl_fixnum code;
		bool print;
		circle_counter = ecl_symbol_value(@'si::*circle-counter*');
		if (circle_counter == Cnil) {
			cl_object hash =
				cl__make_hash_table(@'eq',
						    MAKE_FIXNUM(1024),
						    ecl_make_singlefloat(1.5f),	
						    ecl_make_singlefloat(0.75f), Cnil);
			bds_bind(@'si::*circle-counter*', Ct);
			bds_bind(@'si::*circle-stack*', hash);
			si_write_object(x, cl_core.null_stream);
			ECL_SETQ(@'si::*circle-counter*', MAKE_FIXNUM(0));
			si_write_object(x, stream);
			cl_clrhash(hash);
			bds_unwind_n(2);
			return x;
		}
		code = search_print_circle(x);
		if (!FIXNUMP(circle_counter)) {
			/* We are only inspecting the object to be printed. */
			/* Only run X if it was not referenced before */
			if (code != 0) return x;
		} else if (code == 0) {
			/* Object is not referenced twice */
		} else if (code < 0) {
			/* Object is referenced twice. We print its definition */
			write_ch('#', stream);
			write_decimal(-code, stream);
			write_ch('=', stream);
		} else {
			/* Second reference to the object */
			write_ch('#', stream);
			write_decimal(code, stream);
			write_ch('#', stream);
			return x;
		}
	}
	return si_write_ugly_object(x, stream);
}
 
#if !defined(ECL_CMU_FORMAT)
cl_object
si_write_object(cl_object x, cl_object stream) {
	if (ecl_symbol_value(@'*print-pretty*') == Cnil) {
		cl_env.print_pretty = 0;
	} else {
		cl_env.print_pretty = 1;
		cl_env.qh = cl_env.qt = cl_env.qc = 0;
		cl_env.isp = cl_env.iisp = 0;
		cl_env.indent_stack[0] = 0;
	}
	si_write_object_recursive(x, stream);
	if (cl_env.print_pretty)
		flush_queue(TRUE, stream);
}
#endif /* !ECL_CMU_FORMAT */

static bool
object_will_print_as_hash(cl_object x)
{
	cl_object circle_counter = ecl_symbol_value(@'si::*circle-counter*');
	cl_object circle_stack = ecl_symbol_value(@'si::*circle-stack*');
	cl_object code = ecl_gethash_safe(x, circle_stack, OBJNULL);
	if (FIXNUMP(circle_counter)) {
		return !(code == OBJNULL || code == Cnil);
	} else if (code == OBJNULL) {
		/* Was not found before */
		ecl_sethash(x, circle_stack, Cnil);
		return 0;
	} else {
		return 1;
	}
}

/* To print circular structures, we traverse the structure by adding
   a pair <element, flag> to the interpreter stack for each element visited.
   flag is initially NIL and becomes T if the element is visited again.
   After the visit we squeeze out all the non circular elements.
   The flags is used during printing to distinguish between the first visit
   to the element.
 */

static cl_fixnum
search_print_circle(cl_object x)
{
	cl_object circle_counter = ecl_symbol_value(@'si::*circle-counter*');
	cl_object circle_stack = ecl_symbol_value(@'si::*circle-stack*');
	cl_object code;

	if (!FIXNUMP(circle_counter)) {
		code = ecl_gethash_safe(x, circle_stack, OBJNULL);
		if (code == OBJNULL) {
			/* Was not found before */
			ecl_sethash(x, circle_stack, Cnil);
			return 0;
		} else if (code == Cnil) {
			/* This object is referenced twice */
			ecl_sethash(x, circle_stack, Ct);
			return 1;
		} else {
			return 2;
		}
	} else {
		code = ecl_gethash_safe(x, circle_stack, OBJNULL);
		if (code == OBJNULL || code == Cnil) {
			/* Is not referenced or was not found before */
			/* ecl_sethash(x, circle_stack, Cnil); */
			return 0;
		} else if (code == Ct) {
			/* This object is referenced twice, but has no code yet */
			cl_fixnum new_code = fix(circle_counter) + 1;
			circle_counter = MAKE_FIXNUM(new_code);
			ecl_sethash(x, circle_stack, circle_counter);
			ECL_SETQ(@'si::*circle-counter*', circle_counter);
			return -new_code;
		} else {
			return fix(code);
		}
	}
}

#define	ecl_exponent_marker_p(i)	\
	((i) == 'e' || (i) == 'E' ||	\
	 (i) == 's' || (i) == 'S' || (i) == 'f' || (i) == 'F' || \
	 (i) == 'd' || (i) == 'D' || (i) == 'l' || (i) == 'L' || \
	 (i) == 'b' || (i) == 'B')

static bool
potential_number_p(cl_object strng, int base)
{
	/* See ANSI 2.3.1.1 */
	int i, l, c;
	char *s;

	l = strng->base_string.fillp;
	if (l == 0)
		return FALSE;
	s = strng->base_string.self;
	c = s[0];

	/* A potential number must begin with a digit, sign or extension character (^ _) */
	if ((ecl_digitp(c, base) < 0) && c != '+' && c != '-' && c != '^' && c != '_')
		return FALSE;

	/* A potential number cannot end with a sign */
	if (s[l-1] == '+' || s[l-1] == '-')
		return FALSE;

	for (i = 1;  i < l;  i++) {
		c = s[i];
		/* It can only contain digits, signs, ratio markers, extension characters and
		 * number markers. Number markers are letters, but two adjacent letters fail
		 * to be a number marker. */
		if (ecl_digitp(c, base) >= 0 || c == '+' && c == '-' && c == '/' && c == '.' &&
		    c == '^' && c == '_') {
			continue;
		}
		if (isalpha(c) && ((i+1) >= l) || !isalpha(s[i+1])) {
			continue;
		}
		return FALSE;
	}
	return TRUE;
}

@(defun write (x
	       &key ((:stream strm) Cnil)
		    (array ecl_symbol_value(@'*print-array*'))
		    (base ecl_symbol_value(@'*print-base*'))
		    ((:case cas) ecl_symbol_value(@'*print-case*'))
		    (circle ecl_symbol_value(@'*print-circle*'))
		    (escape ecl_symbol_value(@'*print-escape*'))
		    (gensym ecl_symbol_value(@'*print-gensym*'))
		    (length ecl_symbol_value(@'*print-length*'))
		    (level ecl_symbol_value(@'*print-level*'))
		    (lines ecl_symbol_value(@'*print-lines*'))
		    (miser_width ecl_symbol_value(@'*print-miser-width*'))
		    (pprint_dispatch ecl_symbol_value(@'*print-pprint-dispatch*'))
		    (pretty ecl_symbol_value(@'*print-pretty*'))
		    (radix ecl_symbol_value(@'*print-radix*'))
		    (readably ecl_symbol_value(@'*print-readably*'))
		    (right_margin ecl_symbol_value(@'*print-right-margin*')))
@{
	bds_bind(@'*print-array*', array);
	bds_bind(@'*print-base*', base);
	bds_bind(@'*print-case*', cas);
	bds_bind(@'*print-circle*', circle);
	bds_bind(@'*print-escape*', escape);
	bds_bind(@'*print-gensym*', gensym);
	bds_bind(@'*print-level*', level);
	bds_bind(@'*print-length*', length);
	bds_bind(@'*print-lines*', lines);
	bds_bind(@'*print-miser-width*', miser_width);
	bds_bind(@'*print-pprint-dispatch*', pprint_dispatch);
	bds_bind(@'*print-pretty*', pretty);
	bds_bind(@'*print-radix*', radix);
	bds_bind(@'*print-readably*', readably);
	bds_bind(@'*print-right-margin*', right_margin);

	strm = stream_or_default_output(strm);
	si_write_object(x, strm);
	ecl_force_output(strm);

	bds_unwind_n(15);
	@(return x)
@)

@(defun prin1 (obj &optional strm)
@
	ecl_prin1(obj, strm);
	@(return obj)
@)

@(defun print (obj &optional strm)
@
	ecl_print(obj, strm);
	@(return obj)
@)

@(defun pprint (obj &optional strm)
@
	strm = stream_or_default_output(strm);
	bds_bind(@'*print-escape*', Ct);
	bds_bind(@'*print-pretty*', Ct);
	ecl_write_char('\n', strm);
	si_write_object(obj, strm);
	ecl_force_output(strm);
	bds_unwind_n(2);
	@(return)
@)

@(defun princ (obj &optional strm)
@
	ecl_princ(obj, strm);
	@(return obj)
@)

@(defun write-char (c &optional strm)
@
	/* INV: ecl_char_code() checks the type of `c' */
 	strm = stream_or_default_output(strm);
	ecl_write_char(ecl_char_code(c), strm);
	@(return c)
@)

@(defun write-string (strng &o strm &k (start MAKE_FIXNUM(0)) end)
@
	strng = ecl_check_type_string(@'write-string', strng);
	strm = stream_or_default_output(strm);
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) != t_stream)
		funcall(5, @'gray::stream-write-string', strm, strng, start, end);
	else
#endif
		si_do_write_sequence(strng, strm, start, end);
	@(return strng)
@)

@(defun write-line (strng &o strm &k (start MAKE_FIXNUM(0)) end)
@
	strng = ecl_check_type_string(@'write-line', strng);
	strm = stream_or_default_output(strm);
	si_do_write_sequence(strng, strm, start, end);
	ecl_write_char('\n', strm);
	ecl_force_output(strm);
	@(return strng)
@)

@(defun terpri (&optional strm)
@
	ecl_terpri(strm);
	@(return Cnil)
@)

@(defun fresh-line (&optional strm)
@
 	strm = stream_or_default_output(strm);
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) != t_stream) {
		return funcall(2, @'gray::stream-fresh-line', strm);
	}
#endif
	if (ecl_file_column(strm) == 0)
		@(return Cnil)
	ecl_write_char('\n', strm);
	ecl_force_output(strm);
	@(return Ct)
@)

@(defun finish-output (&o strm)
@
 	strm = stream_or_default_output(strm);
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) != t_stream) {
		return funcall(2, @'gray::stream-finish-output', strm);
	}
#endif
	ecl_force_output(strm);
	@(return Cnil)
@)

@(defun force-output (&o strm)
@
 	strm = stream_or_default_output(strm);
	ecl_force_output(strm);
	@(return Cnil)
@)

@(defun clear-output (&o strm)
@
 	strm = stream_or_default_output(strm);
	ecl_clear_output(strm);
	@(return Cnil)
@)

cl_object
cl_write_byte(cl_object integer, cl_object binary_output_stream)
{
	ecl_write_byte(integer, binary_output_stream);
	@(return integer)
}

@(defun write-sequence (sequence stream &key (start MAKE_FIXNUM(0)) end)
@
#ifdef ECL_CLOS_STREAMS
	if (type_of(stream) != t_stream)
		return funcall(5, @'gray::stream-write-sequence', stream, sequence, start, end);
	else
#endif
		return si_do_write_sequence(sequence, stream, start, end);
@)

cl_object
ecl_princ(cl_object obj, cl_object strm)
{
	strm = stream_or_default_output(strm);
	bds_bind(@'*print-escape*', Cnil);
	bds_bind(@'*print-readably*', Cnil);
	si_write_object(obj, strm);
	bds_unwind_n(2);
	return obj;
}

cl_object
ecl_prin1(cl_object obj, cl_object strm)
{
	strm = stream_or_default_output(strm);
	bds_bind(@'*print-escape*', Ct);
	si_write_object(obj, strm);
	ecl_force_output(strm);
	bds_unwind1();
	return obj;
}

cl_object
ecl_print(cl_object obj, cl_object strm)
{
	strm = stream_or_default_output(strm);
	ecl_terpri(strm);
	ecl_prin1(obj, strm);
	ecl_princ_char(' ', strm);
	return obj;
}

cl_object
ecl_terpri(cl_object strm)
{
	strm = stream_or_default_output(strm);
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) != t_stream) {
		return funcall(2, @'gray::stream-terpri', strm);
	}
#endif
	ecl_write_char('\n', strm);
	ecl_force_output(strm);
	return(Cnil);
}

void
ecl_write_string(cl_object strng, cl_object strm)
{
	cl_index i;

	strm = stream_or_default_output(strm);
	switch(type_of(strng)) {
#ifdef ECL_UNICODE
	case t_string:
		for (i = 0;  i < strng->string.fillp;  i++)
			ecl_write_char(CHAR_CODE(strng->string.self[i]), strm);
		break;
#endif
	case t_base_string:
		for (i = 0;  i < strng->base_string.fillp;  i++)
			ecl_write_char(strng->base_string.self[i], strm);
		break;
	default:
		FEtype_error_string(strng);
	}
		
	ecl_force_output(strm);
}

/*
	THE ULTRA-SPECIAL-DINNER-SERVICE OPTIMIZATION
*/
void
ecl_princ_str(const char *s, cl_object strm)
{
	strm = stream_or_default_output(strm);
	writestr_stream(s, strm);
}

void
ecl_princ_char(int c, cl_object strm)
{
	strm = stream_or_default_output(strm);
	ecl_write_char(c, strm);
	if (c == '\n') {
		ecl_force_output(strm);
	}
}
