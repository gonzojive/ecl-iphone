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

#include <string.h>
#include "ecl.h"
#include <math.h>
#include <ctype.h>
#include <unistd.h>
#include "internal.h"

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
#define write_ch	writec_stream
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
			writec_stream(c, stream);
		} else if (c == MARK)
			goto DO_MARK;
		else if (c == UNMARK)
			cl_env.isp -= 2;
		else if (c == SET_INDENT)
			cl_env.indent_stack[cl_env.isp] = file_column(stream);
		else if (c == INDENT) {
			goto DO_INDENT;
		} else if (c == INDENT1) {
			i = file_column(stream)-cl_env.indent_stack[cl_env.isp];
			if (i < 8 && cl_env.indent_stack[cl_env.isp] < LINE_LENGTH/2) {
				writec_stream(' ', stream);
				cl_env.indent_stack[cl_env.isp]
				= file_column(stream);
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
	k = LINE_LENGTH - 1 - file_column(stream);
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
	cl_env.indent_stack[cl_env.isp-1] = file_column(stream);
	cl_env.indent_stack[cl_env.isp] = cl_env.indent_stack[cl_env.isp-1];
	goto BEGIN;

DO_INDENT:
	if (cl_env.iisp > cl_env.isp)
		goto PUT_INDENT;
	k = LINE_LENGTH - 1 - file_column(stream);
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
	writec_stream('\n', stream);
	for (i = cl_env.indent_stack[cl_env.isp];  i > 0;  --i)
		writec_stream(' ', stream);
	cl_env.iisp = cl_env.isp;
	goto BEGIN;

FLUSH:
	for (j = 0;  j < i;  j++) {
		c = cl_env.queue[cl_env.qh];
		if (c == INDENT || c == INDENT1 || c == INDENT2)
			writec_stream(' ', stream);
		else if (c < 0400)
			writec_stream(c, stream);
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
		writec_stream(' ', stream);
	else if (c < 0400)
		writec_stream(c, stream);
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
extern cl_fixnum search_print_circle(cl_object x);
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
	cl_object object = symbol_value(@'*print-base*');
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
	cl_object object = symbol_value(@'*print-level*');
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
	cl_object object = symbol_value(@'*print-length*');
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
	return symbol_value(@'*print-radix*') != Cnil;
}

cl_object
ecl_print_case(void)
{
	cl_object output = symbol_value(@'*print-case*');
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
	return symbol_value(@'*print-gensym*') != Cnil;
}

bool
ecl_print_array(void)
{
	return symbol_value(@'*print-array*') != Cnil;
}

bool
ecl_print_readably(void)
{
	return symbol_value(@'*print-readably*') != Cnil;
}

bool
ecl_print_escape(void)
{
	return symbol_value(@'*print-escape*') != Cnil;
}

bool
ecl_print_circle(void)
{
	return symbol_value(@'*print-circle*') != Cnil;
}

static void
write_str(const char *s, cl_object stream)
{
	while (*s != '\0')
		write_ch(*s++, stream);
}

static void
write_decimal1(cl_object stream, cl_fixnum i)
{
	if (i == 0)
		return;
	write_decimal1(stream, i/10);
	write_ch(i%10 + '0', stream);
}

static void
write_decimal(cl_fixnum i, cl_object stream)
{
	if (i == 0) {
		write_ch('0', stream);
		return;
	}
	write_decimal1(stream, i);
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

#define FPRC 17

void
edit_double(int n, double d, int *sp, char *s, int *ep)
{
	char *p, buff[FPRC + 9];
	int i;

	if (isnan(d) || !finite(d))
		FEerror("Can't print a non-number.", 0);
	else
		sprintf(buff, "%*.*e",FPRC+8,FPRC, d);
	if (buff[FPRC+3] != 'e') {
		sprintf(buff, "%*.*e",FPRC+7,FPRC,d);
		*ep = (buff[FPRC+5]-'0')*10 + (buff[FPRC+6]-'0');
	} else
		*ep = (buff[FPRC+5]-'0')*100 +
		  (buff[FPRC+6]-'0')*10 + (buff[FPRC+7]-'0');
	*sp = 1;
	if (buff[0] == '-')
		*sp *= -1;

	if (buff[FPRC+4] == '-')
		*ep *= -1;
	buff[2] = buff[1];
	p = buff + 2;
	if (n < FPRC+1) {
		if (p[n] >= '5') {
			for (i = n - 1;  i >= 0;  --i)
				if (p[i] == '9')
					p[i] = '0';
				else {
					p[i]++;
					break;
				}
			if (i < 0) {
				*--p = '1';
				(*ep)++;
			}
		}
		for (i = 0;  i < n;  i++)
			s[i] = p[i];
	} else {
		for (i = 0;  i < FPRC+1;  i++)
			s[i] = p[i];
		for (;  i < n;  i++)
			s[i] = '0';
	}
	s[n] = '\0';
}


static void
write_double(double d, int e, bool shortp, cl_object stream)
{
	int sign;
	char buff[FPRC+5];
	int exp;
	int i;
	int n = FPRC;		/* was FPRC+1 */

	if (shortp)
		n = 8;
	edit_double(n, d, &sign, buff, &exp);
	if (sign==2) {
		write_str("#<", stream);
		write_str(buff, stream);
		write_ch('>', stream);
		return;
	      }
	if (sign < 0)
		write_ch('-', stream);
	if (-3 <= exp && exp < 7) {
		if (exp < 0) {
			write_ch('0', stream);
			write_ch('.', stream);
			exp = (-exp) - 1;
			for (i = 0;  i < exp;  i++)
				write_ch('0', stream);
			for (;  n > 0;  --n)
				if (buff[n-1] != '0')
					break;
			if (exp == 0 && n == 0)
				n = 1;
			for (i = 0;  i < n;  i++)
				write_ch(buff[i], stream);
		} else {
			exp++;
			for (i = 0;  i < exp;  i++)
				if (i < n)
					write_ch(buff[i], stream);
				else
					write_ch('0', stream);
			write_ch('.', stream);
			if (i < n)
				write_ch(buff[i], stream);
			else
				write_ch('0', stream);
			i++;
			for (;  n > i;  --n)
				if (buff[n-1] != '0')
					break;
			for (;  i < n;  i++)
				write_ch(buff[i], stream);
		}
		exp = 0;
	} else {
		write_ch(buff[0], stream);
		write_ch('.', stream);
		write_ch(buff[1], stream);
		for (;  n > 2;  --n)
			if (buff[n-1] != '0')
				break;
		for (i = 2;  i < n;  i++)
			write_ch(buff[i], stream);
	}
	if (exp == 0 && e == 0)
		return;
	if (e == 0)
		e = 'E';
	write_ch(e, stream);
	if (exp < 0) {
		write_ch('-', stream);
		exp *= -1;
	}
	write_decimal(exp, stream);
}


static void
write_positive_fixnum(cl_index i, cl_object stream)
{
	/* The maximum number of digits is achieved for base 2 and it
	   is always < FIXNUM_BITS, since we use at least one bit for
	   tagging */
	short digits[FIXNUM_BITS];
	int j, base = ecl_print_base();
	for (j = 0;  i != 0;  i /= base)
		digits[j++] = ecl_digit_char(i % base, base);
	while (j-- > 0)
		write_ch(digits[j], stream);
}

static void
write_bignum(cl_object x, cl_object stream)
{
	int base = ecl_print_base();
	cl_fixnum str_size = mpz_sizeinbase(x->big.big_num, base);
	char str[str_size+2];
	char *s = str;
	mpz_get_str(str, base, x->big.big_num);
	while (*s)
		write_ch(*s++, stream);
}

static void
write_symbol(cl_object x, cl_object stream)
{
	bool escaped;
	cl_index i;
	cl_object s = x->symbol.name;
	cl_object print_package = symbol_value(@'si::*print-package*');
	cl_object print_case = ecl_print_case();
	int intern_flag;

	if (!ecl_print_escape() && !ecl_print_readably()) {
		for (i = 0;  i < s->string.fillp;  i++) {
			int c = s->string.self[i];
			if (isupper(c) &&
			    (print_case == @':downcase' ||
			     (print_case == @':capitalize' && i != 0)))
				c = tolower(c);
			write_ch(c, stream);
		}
		return;
	}
	if (Null(x->symbol.hpack)) {
		if (ecl_print_gensym())
			write_str("#:", stream);
	} else if (x->symbol.hpack == cl_core.keyword_package)
		write_ch(':', stream);
	else if ((print_package != Cnil && x->symbol.hpack != print_package)
		 || ecl_find_symbol(x, current_package(), &intern_flag)!=x
		 || intern_flag == 0) {
		escaped = 0;
		for (i = 0;
		     i < x->symbol.hpack->pack.name->string.fillp;
		     i++) {
			int c = x->symbol.hpack->pack.name->string.self[i];
			if (to_be_escaped(c))
				escaped = 1;
		}
		if (escaped)
			write_ch('|', stream);
		for (i = 0;
		     i < x->symbol.hpack->pack.name->string.fillp;
		     i++) {
			int c = x->symbol.hpack->pack.name->string.self[i];
			if (c == '|' || c == '\\')
				write_ch('\\', stream);
			if (escaped == 0 && isupper(c) &&
			    (print_case == @':downcase' ||
			     (print_case == @':capitalize' && i!=0)))
				c = tolower(c);
			write_ch(c, stream);
		}
		if (escaped)
			write_ch('|', stream);
		if (ecl_find_symbol(x, x->symbol.hpack, &intern_flag) != x)
			error("can't print symbol");
		if ((print_package != Cnil && x->symbol.hpack != print_package)
		    || intern_flag == INTERNAL)
			write_str("::", stream);
		else if (intern_flag == EXTERNAL)
			write_ch(':', stream);
		else
			FEerror("Pathological symbol --- cannot print.", 0);
	}
	escaped = 0;
	if (potential_number_p(s, ecl_print_base()))
		escaped = 1;
	for (i = 0;  i < s->string.fillp;  i++) {
		int c = s->string.self[i];
		if (to_be_escaped(c))
			escaped = 1;
	}
	for (i = 0;  i < s->string.fillp;  i++)
		if (s->string.self[i] != '.')
			goto NOT_DOT;
	escaped = 1;

 NOT_DOT:
	if (escaped)
		write_ch('|', stream);
	for (i = 0;  i < s->string.fillp;  i++) {
		int c = s->string.self[i];
		if (c == '|' || c == '\\')
			write_ch('\\', stream);
		if (escaped == 0 && isupper(c) &&
		    (print_case == @':downcase' ||
		     (print_case == @':capitalize' && i != 0)))
			c = tolower(c);
		write_ch(c, stream);
	}
	if (escaped)
		write_ch('|', stream);
}

static void
write_character(int i, cl_object stream)
{
	if (!ecl_print_escape() && !ecl_print_readably()) {
		write_ch(i, stream);
	} else {
		write_str("#\\", stream);
		if (i <= 32 || i == 127) {
			cl_object name = cl_char_name(CODE_CHAR(i));
			write_str(name->string.self, stream);
		} else if (i >= 128) {
			write_ch('\\', stream);
			write_ch(((i>>6)&7) + '0', stream);
			write_ch(((i>>3)&7) + '0', stream);
			write_ch(((i>>0)&7) + '0', stream);
		} else {
			write_ch(i, stream);
		}
	}
}

cl_object
si_write_ugly_object(cl_object x, cl_object stream)
{
	cl_object r, y;
	cl_fixnum i, j;
	cl_index ndx, k;
	bool circle = ecl_print_circle();

 BEGIN:
	if (x == OBJNULL) {
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str("#<OBJNULL>", stream);
		return;
	}
	if (circle) {
		cl_object circle_counter;
		cl_fixnum code;
		bool print;
		if (IMMEDIATE(x) ||
		    (type_of(x) == t_symbol && !Null(x->symbol.hpack)))
			goto DOPRINT;
		circle_counter = symbol_value(@'si::*circle-counter*');
		if (circle_counter == Cnil) {
			cl_object hash =
				cl__make_hash_table(@'eq',
						    MAKE_FIXNUM(1024),
						    make_shortfloat(1.5f),	
						    make_shortfloat(0.75f), Cnil);
			bds_bind(@'si::*circle-counter*', Ct);
			bds_bind(@'si::*circle-stack*', hash);
			si_write_ugly_object(x, cl_core.null_stream);
			ECL_SETQ(@'si::*circle-counter*', MAKE_FIXNUM(0));
			si_write_ugly_object(x, stream);
			cl_clrhash(hash);
			bds_unwind_n(2);
			return;
		}
		code = search_print_circle(x);
		if (!FIXNUMP(circle_counter)) {
			/* We are only inspecting the object to be printed. */
			/* Only run X if it was not referenced before */
			if (code != 0) return;
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
			return;
		}
	}
 DOPRINT:
	switch (type_of(x)) {

	case FREE:
		write_str("#<FREE OBJECT ", stream);
		write_addr(x, stream);
		write_ch('>', stream);
		return;

	case t_fixnum: {
		bool print_radix = ecl_print_radix();
		int print_base = ecl_print_base();
		if (print_radix && print_base != 10)
			write_base(print_base, stream);
		if (x == MAKE_FIXNUM(0)) {
			write_ch('0', stream);
		} else if (FIXNUM_MINUSP(x)) {
			write_ch('-', stream);
			write_positive_fixnum(-fix(x), stream);
		} else {
			write_positive_fixnum(fix(x), stream);
		}
		if (print_radix && print_base == 10) {
			write_ch('.', stream);
		}
		return;
	}
	case t_bignum: {
		bool print_radix = ecl_print_radix();
		int print_base = ecl_print_base();
		if (print_radix && print_base != 10)
			write_base(print_base, stream);
		write_bignum(x, stream);
		if (print_radix && print_base == 10)
			write_ch('.', stream);
		return;
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
		return;

	case t_shortfloat:
		r = symbol_value(@'*read-default-float-format*');
		if (r == @'single-float' || r == @'short-float')
			write_double((double)sf(x), 0, TRUE, stream);
		else
			write_double((double)sf(x), 'f', TRUE, stream);
		return;

	case t_longfloat:
		r = symbol_value(@'*read-default-float-format*');
		if (r == @'long-float' || r == @'double-float')
			write_double(lf(x), 0, FALSE, stream);
		else
			write_double(lf(x), 'd', FALSE, stream);
		return;

	case t_complex:
		write_str("#C(", stream);
		si_write_ugly_object(x->complex.real, stream);
		write_ch(' ', stream);
		si_write_ugly_object(x->complex.imag, stream);
		write_ch(')', stream);
		return;

	case t_character:
		write_character(CHAR_CODE(x), stream);
		return;

	case t_symbol:
		write_symbol(x, stream);
		return;

	case t_array: {
		cl_index subscripts[ARANKLIM];
		cl_fixnum n, j, m, k, i;
		cl_fixnum print_length;
		cl_fixnum print_level;
		bool readably = ecl_print_readably();

		if (readably) {
			if (!ecl_print_array()) {
				write_str("#<array ", stream);
				write_addr(x, stream);
				write_ch('>', stream);
				return;
			}
			print_length = MOST_POSITIVE_FIXNUM;
			print_level = MOST_POSITIVE_FIXNUM;
		} else {
			print_level = ecl_print_level();
			print_length = ecl_print_length();
		}
		write_ch('#', stream);
		if (print_level == 0)
			return;
		n = x->array.rank;
		write_decimal(n, stream);
		write_ch('A', stream);
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
					if (x->array.dims[i] == 0) {
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
					write_ch(INDENT, stream);
					k=x->array.dims[i]-subscripts[i];
					subscripts[i] = 0;
					for (j = i+1;  j < n;  j++)
						k *= x->array.dims[j];
					j = i-1;
					goto INC;
				}
			}
			/* FIXME: This conses! */
			if (print_level >= 0)
				si_write_ugly_object(aref(x, m), stream);
			else
				write_ch('#', stream);
			j = n-1;
			k = 1;

		INC:
			while (j >= 0) {
				if (++subscripts[j] < x->array.dims[j])
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
		return;
	}
	case t_vector: {
		cl_fixnum print_length, print_level;
		bool readably = ecl_print_readably();
		cl_index n = x->vector.fillp;

		if (readably) {
			if (!ecl_print_array()) {
				write_str("#<vector ", stream);
				write_decimal(x->vector.dim, stream);
				write_ch(' ', stream);
				write_addr(x, stream);
				write_ch('>', stream);
				return;
			}
			print_length = MOST_POSITIVE_FIXNUM;
			print_level = MOST_POSITIVE_FIXNUM;
		} else {
			print_level = ecl_print_level();
			print_length = ecl_print_length();
		}
		write_ch('#', stream);
		if (print_level == 0)
			return;
		WRITE_MARK(stream);
		write_ch('(', stream);
		WRITE_SET_INDENT(stream);
		if (n > 0) {
			if (print_length == 0) {
				write_str("...)", stream);
				return;
			}
			bds_bind(@'*print-level*', MAKE_FIXNUM(print_level-1));
			si_write_ugly_object(aref(x, 0), stream);
			for (ndx = 1;  ndx < x->vector.fillp;  ndx++) {
				write_ch(INDENT, stream);
				if (ndx >= print_length) {
					write_str("...", stream);
					break;
				}
				si_write_ugly_object(aref(x, ndx), stream);
			}
			bds_unwind1();
		}
		write_ch(')', stream);
		WRITE_UNMARK(stream);
		return;
	}
	case t_string:
		if (!ecl_print_escape() && !ecl_print_readably()) {
			for (ndx = 0;  ndx < x->string.fillp;  ndx++)
				write_ch(x->string.self[ndx], stream);
			return;
		}
		write_ch('"', stream);
		for (ndx = 0;  ndx < x->string.fillp;  ndx++) {
			int c = x->string.self[ndx];
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
			if (x->vector.self.bit[ndx/8] & (0200 >> ndx%8))
				write_ch('1', stream);
			else
				write_ch('0', stream);
		break;

	case t_cons: {
		cl_fixnum print_level, print_length;
		if (CAR(x) == @'si::#!') {
			write_str("#!", stream);
			x = CDR(x);
			goto BEGIN;
		}
		if (CAR(x) == @'quote' && CONSP(CDR(x)) && Null(CDDR(x))) {
			write_ch('\'', stream);
			x = CADR(x);
			goto BEGIN;
		}
		if (CAR(x) == @'function' && CONSP(CDR(x)) && Null(CDDR(x))) {
			write_ch('#', stream);
			write_ch('\'', stream);
			x = CADR(x);
			goto BEGIN;
		}
		if (ecl_print_readably()) {
			print_level = MOST_POSITIVE_FIXNUM;
			print_length = MOST_POSITIVE_FIXNUM;
		} else {
			print_level = ecl_print_level();
			print_length = ecl_print_length();
		}
		if (print_level == 0) {
			write_ch('#', stream);
			return;
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
			si_write_ugly_object(y, stream);
			/* FIXME! */
			if (x == OBJNULL || ATOM(x) ||
			    (circle && object_will_print_as_hash(x)))
			{
				if (x != Cnil) {
					write_ch(INDENT, stream);
					write_str(". ", stream);
					si_write_ugly_object(x, stream);
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
		return;
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
				si_write_ugly_object(y, stream);
			/* FIXME! */
			if (x == OBJNULL || ATOM(x) ||
			    (circle && object_will_print_as_hash(x))) {
				if (x != Cnil) {
					write_ch(INDENT, stream);
					write_str(". ", stream);
					si_write_ugly_object(x, stream);
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
		switch ((enum ecl_smmode)x->stream.mode) {
		case smm_closed:
			write_str("#<closed stream ", stream);
			si_write_ugly_object(x->stream.object1, stream);
			break;

		case smm_input:
			write_str("#<input stream ", stream);
			si_write_ugly_object(x->stream.object1, stream);
			break;

		case smm_output:
			write_str("#<output stream ", stream);
			si_write_ugly_object(x->stream.object1, stream);
			break;

		case smm_io:
			write_str("#<io stream ", stream);
			si_write_ugly_object(x->stream.object1, stream);
			break;

		case smm_probe:
			write_str("#<probe stream ", stream);
			si_write_ugly_object(x->stream.object1, stream);
			break;

		case smm_synonym:
			write_str("#<synonym stream to ", stream);
			si_write_ugly_object(x->stream.object0, stream);
			break;

		case smm_broadcast:
			write_str("#<broadcast stream ", stream);
			write_addr(x, stream);
			break;

		case smm_concatenated:
			write_str("#<concatenated stream ", stream);
			write_addr(x, stream);
			break;

		case smm_two_way:
			write_str("#<two-way stream ", stream);
			write_addr(x, stream);
			break;

		case smm_echo:
			write_str("#<echo stream ", stream);
			write_addr(x, stream);
			break;

		case smm_string_input:
			write_str("#<string-input stream from \"", stream);
			y = x->stream.object0;
			k = y->string.fillp;
			for (ndx = 0;  ndx < k && ndx < 16;  ndx++)
				write_ch(y->string.self[ndx], stream);
			if (k > 16)
				write_str("...", stream);
			write_ch('"', stream);
			break;

		case smm_string_output:
			write_str("#<string-output stream ", stream);
			write_addr(x, stream);
			break;

		default:
			error("illegal stream mode");
		}
		write_ch('>', stream);
		break;

	case t_random:
		write_str("#$", stream);
		si_write_ugly_object(MAKE_FIXNUM(x->random.value), stream);
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
			si_write_ugly_object(x, stream);
		} else {
			call_structure_print_function(print_function, x, stream);
		}
		break;
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
		}
		if (ecl_print_escape() || ecl_print_readably())
			write_str("#P", stream);
		si_write_ugly_object(namestring, stream);
		break;
	}
	case t_bytecodes: {
		cl_object name = x->bytecodes.name;
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str("#<bytecompiled-function ", stream);
		if (name != Cnil)
			si_write_ugly_object(name, stream);
		else
			write_addr(x, stream);
		write_ch('>', stream);
		break;
	}
	case t_cfun:
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
		if (type_of(CLASS_OF(x)) != t_instance)
			FEwrong_type_argument(@'ext::instance', CLASS_OF(x));
		call_print_object(x, stream);
		break;
#endif /* CLOS */
	case t_foreign:
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str("#<foreign ", stream);
		/*_write_ugly_object(x->foreign.tag, level);*/
		write_addr(x->foreign.data, stream);
		write_ch('>', stream);
		break;
#ifdef ECL_THREADS
	case t_process:
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str("#<process ", stream);
		write_addr(x, stream);
		write_ch('>', stream);
		break;
	case t_lock:
		if (ecl_print_readably()) FEprint_not_readable(x);
		write_str("#<lock ", stream);
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
	@(return)
}

cl_object
si_write_object(cl_object x, cl_object stream)
{
#if defined(ECL_CMU_FORMAT)
	if (symbol_value(@'*print-pretty*') != Cnil) {
		cl_object f = funcall(2, @'pprint-dispatch', x);
		if (VALUES(1) != Cnil) {
			funcall(3, f, stream, x);
			return;
		}
	}
	return si_write_ugly_object(x, stream);
#else /* !ECL_CMU_FORMAT */
	if (symbol_value(@'*print-pretty*') == Cnil) {
		cl_env.print_pretty = 0;
	} else {
		cl_env.print_pretty = 1;
		cl_env.qh = cl_env.qt = cl_env.qc = 0;
		cl_env.isp = cl_env.iisp = 0;
		cl_env.indent_stack[0] = 0;
	}
	si_write_ugly_object(x, stream);
	if (cl_env.print_pretty)
		flush_queue(TRUE, stream);
#endif /* !ECL_CMU_FORMAT */
}

static bool
object_will_print_as_hash(cl_object x)
{
	cl_object circle_counter = symbol_value(@'si::*circle-counter*');
	cl_object circle_stack = symbol_value(@'si::*circle-stack*');
	cl_object code = gethash_safe(x, circle_stack, OBJNULL);
	if (FIXNUMP(circle_counter)) {
		return !(code == OBJNULL || code == Cnil);
	} else if (code == OBJNULL) {
		/* Was not found before */
		sethash(x, circle_stack, Cnil);
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
	cl_object circle_counter = symbol_value(@'si::*circle-counter*');
	cl_object circle_stack = symbol_value(@'si::*circle-stack*');
	cl_object code;

	if (!FIXNUMP(circle_counter)) {
		code = gethash_safe(x, circle_stack, OBJNULL);
		if (code == OBJNULL) {
			/* Was not found before */
			sethash(x, circle_stack, Cnil);
			return 0;
		} else if (code == Cnil) {
			/* This object is referenced twice */
			sethash(x, circle_stack, Ct);
			return 1;
		} else {
			return 2;
		}
	} else {
		code = gethash_safe(x, circle_stack, OBJNULL);
		if (code == OBJNULL || code == Cnil) {
			/* Is not referenced or was not found before */
			/* sethash(x, circle_stack, Cnil); */
			return 0;
		} else if (code == Ct) {
			/* This object is referenced twice, but has no code yet */
			cl_fixnum new_code = fix(circle_counter) + 1;
			circle_counter = MAKE_FIXNUM(new_code);
			sethash(x, circle_stack, circle_counter);
			ECL_SETQ(@'si::*circle-counter*', circle_counter);
			return -new_code;
		} else {
			return fix(code);
		}
	}
}

static bool
potential_number_p(cl_object strng, int base)
{
	int i, l, c; bool dc;
	char *s;

	l = strng->string.fillp;
	if (l == 0)
		return(FALSE);
	s = strng->string.self;
	dc = FALSE;
	c = s[0];
	if (digitp(c, base) >= 0)
		dc = TRUE;
	else if (c != '+' && c != '-' && c != '^' && c != '_')
		return(FALSE);
	if (s[l-1] == '+' || s[l-1] == '-')
		return(FALSE);
	for (i = 1;  i < l;  i++) {
		c = s[i];
		if (digitp(c, base) >= 0) {
			dc = TRUE;
			continue;
		}
		if (c != '+' && c != '-' && c != '/' && c != '.' &&
		    c != '^' && c != '_' &&
		    c != 'e' && c != 'E' &&
		    c != 's' && c != 'S' && c != 'l' && c != 'L')
			return(FALSE);
	}
	return(dc);
}

@(defun write (x
	       &key ((:stream strm) Cnil)
		    (escape symbol_value(@'*print-escape*'))
		    (readably symbol_value(@'*print-readably*'))
		    (radix symbol_value(@'*print-radix*'))
		    (base symbol_value(@'*print-base*'))
		    (circle symbol_value(@'*print-circle*'))
		    (pretty symbol_value(@'*print-pretty*'))
		    (level symbol_value(@'*print-level*'))
		    (length symbol_value(@'*print-length*'))
		    ((:case cas) symbol_value(@'*print-case*'))
		    (gensym symbol_value(@'*print-gensym*'))
		    (array symbol_value(@'*print-array*')))
@{
	bds_bind(@'*print-escape*', escape);
	bds_bind(@'*print-readably*', readably);
	bds_bind(@'*print-radix*', radix);
	bds_bind(@'*print-base*', base);
	bds_bind(@'*print-circle*', circle);
	bds_bind(@'*print-pretty*', pretty);
	bds_bind(@'*print-level*', level);
	bds_bind(@'*print-length*', length);
	bds_bind(@'*print-case*', cas);
	bds_bind(@'*print-gensym*', gensym);
	bds_bind(@'*print-array*', array);

	strm = stream_or_default_output(strm);
	si_write_object(x, strm);
	flush_stream(strm);

	bds_unwind_n(11);
	@(return x)
@)

@(defun prin1 (obj &optional strm)
@
	prin1(obj, strm);
	@(return obj)
@)

@(defun print (obj &optional strm)
@
	print(obj, strm);
	@(return obj)
@)

@(defun pprint (obj &optional strm)
@
	strm = stream_or_default_output(strm);
	bds_bind(@'*print-escape*', Ct);
	bds_bind(@'*print-pretty*', Ct);
	writec_stream('\n', strm);
	si_write_object(obj, strm);
	flush_stream(strm);
	bds_unwind_n(2);
	@(return)
@)

@(defun princ (obj &optional strm)
@
	princ(obj, strm);
	@(return obj)
@)

@(defun write-char (c &optional strm)
@
	/* INV: char_code() checks the type of `c' */
 	strm = stream_or_default_output(strm);
	writec_stream(char_code(c), strm);
	@(return c)
@)

@(defun write-string (strng &o strm &k (start MAKE_FIXNUM(0)) end)
@
	assert_type_string(strng);
	si_do_write_sequence(strng, stream_or_default_output(strm),
			     start, end);
	@(return strng)
@)

@(defun write-line (strng &o strm &k (start MAKE_FIXNUM(0)) end)
@
	assert_type_string(strng);
	strm = stream_or_default_output(strm);
	si_do_write_sequence(strng, strm, start, end);
	writec_stream('\n', strm);
	flush_stream(strm);
	@(return strng)
@)

@(defun terpri (&optional strm)
@
	terpri(strm);
	@(return Cnil)
@)

@(defun fresh-line (&optional strm)
@
 	strm = stream_or_default_output(strm);
	if (file_column(strm) == 0)
		@(return Cnil)
	writec_stream('\n', strm);
	flush_stream(strm);
	@(return Ct)
@)

@(defun force-output (&o strm)
@
 	strm = stream_or_default_output(strm);
	flush_stream(strm);
	@(return Cnil)
@)

@(defun clear-output (&o strm)
@
 	strm = stream_or_default_output(strm);
	clear_output_stream(strm);
	@(return Cnil)
@)

cl_object
cl_write_byte(cl_object integer, cl_object binary_output_stream)
{
	if (!FIXNUMP(integer))
		FEerror("~S is not a byte.", 1, integer);
	writec_stream(fix(integer), binary_output_stream);
	@(return integer)
}

@(defun write-sequence (sequence stream &key (start MAKE_FIXNUM(0)) end)
@
	return si_do_write_sequence(sequence, stream, start, end);
@)

cl_object
princ(cl_object obj, cl_object strm)
{
	strm = stream_or_default_output(strm);
	bds_bind(@'*print-escape*', Cnil);
	bds_bind(@'*print-readably*', Cnil);
	si_write_object(obj, strm);
	bds_unwind_n(2);
	return obj;
}

cl_object
prin1(cl_object obj, cl_object strm)
{
	strm = stream_or_default_output(strm);
	bds_bind(@'*print-escape*', Ct);
	si_write_object(obj, strm);
	flush_stream(strm);
	bds_unwind1();
	return obj;
}

cl_object
print(cl_object obj, cl_object strm)
{
	strm = stream_or_default_output(strm);
	terpri(strm);
	prin1(obj, strm);
	princ_char(' ', strm);
	return obj;
}

cl_object
terpri(cl_object strm)
{
	strm = stream_or_default_output(strm);
	writec_stream('\n', strm);
	flush_stream(strm);
	return(Cnil);
}

void
write_string(cl_object strng, cl_object strm)
{
	cl_index i;

	strm = stream_or_default_output(strm);
	assert_type_string(strng);
	for (i = 0;  i < strng->string.fillp;  i++)
		writec_stream(strng->string.self[i], strm);
	flush_stream(strm);
}

/*
	THE ULTRA-SPECIAL-DINNER-SERVICE OPTIMIZATION
*/
void
princ_str(const char *s, cl_object strm)
{
	strm = stream_or_default_output(strm);
	writestr_stream(s, strm);
}

void
princ_char(int c, cl_object strm)
{
	strm = stream_or_default_output(strm);
	writec_stream(c, strm);
	if (c == '\n')
		flush_stream(strm);
}
