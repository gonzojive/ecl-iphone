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

/******************************* EXPORTS ******************************/

#ifndef THREADS
bool PRINTreadably;
bool PRINTescape;
bool PRINTpretty;
bool PRINTcircle;
int PRINTbase;
bool PRINTradix;
cl_object PRINTcase;
bool PRINTgensym;
int PRINTlevel;
int PRINTlength;
bool PRINTarray;
cl_object PRINTpackage;
bool PRINTstructure;
cl_object PRINTstream;
#endif /* THREADS */

/******************************* ------- ******************************/

#define	LINE_LENGTH	72

#define	to_be_escaped(c) \
	(standard_readtable->readtable.table[(c)&0377].syntax_type \
	 != cat_constituent || \
	 islower((c)&0377) || (c) == ':')


#define	MARK		0400
#define	UNMARK		0401
#define	SET_INDENT	0402
#define	INDENT		0403
#define	INDENT1		0404
#define	INDENT2		0405

#define	mod(x)		((x)%ECL_PPRINT_QUEUE_SIZE)

#ifdef THREADS

#define queue             clwp->lwp_queue
#define indent_stack      clwp->lwp_indent_stack
#define qh         clwp->lwp_qh
#define qt         clwp->lwp_qt
#define qc         clwp->lwp_qc
#define isp        clwp->lwp_isp
#define iisp       clwp->lwp_iisp

#define CIRCLEstack clwp->lwp_CIRCLEstack
#define CIRCLEcounter clwp->lwp_CIRCLEcounter

#else
static short queue[ECL_PPRINT_QUEUE_SIZE];
static short indent_stack[ECL_PPRINT_INDENTATION_STACK_SIZE];

static int qh;
static int qt;
static int qc;
static int isp;
static int iisp;

static cl_object CIRCLEstack;
static cl_fixnum CIRCLEcounter = -2;
#endif /* THREADS */

static cl_object no_stream;

static void flush_queue (bool force);
static void write_decimal1 (cl_fixnum i);
static cl_fixnum search_print_circle(cl_object x);
static bool do_print_circle(cl_fixnum mark);
static bool potential_number_p(cl_object s, int base);

static void FEprint_not_readable(cl_object x) __attribute__((noreturn));

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

static void
writec_PRINTstream(int c)
{
	if (c == INDENT || c == INDENT1)
		writec_stream(' ', PRINTstream);
	else if (c < 0400)
		writec_stream(c, PRINTstream);
}

static void
writec_queue(int c)
{
	if (qc >= ECL_PPRINT_QUEUE_SIZE)
		flush_queue(FALSE);
	if (qc >= ECL_PPRINT_QUEUE_SIZE)
		FEerror("Can't pretty-print.", 0);
	queue[qt] = c;
	qt = mod(qt+1);
	qc++;
}

static void
flush_queue(bool force)
{
	int c, i, j, k, l, i0;

BEGIN:
	while (qc > 0) {
		c = queue[qh];
		if (c == MARK)
			goto DO_MARK;
		else if (c == UNMARK)
			isp -= 2;
		else if (c == SET_INDENT)
			indent_stack[isp] = file_column(PRINTstream);
		else if (c == INDENT) {
			goto DO_INDENT;
		} else if (c == INDENT1) {
			i = file_column(PRINTstream)-indent_stack[isp];
			if (i < 8 && indent_stack[isp] < LINE_LENGTH/2) {
				writec_PRINTstream(' ');
				indent_stack[isp]
				= file_column(PRINTstream);
			} else {
				if (indent_stack[isp] < LINE_LENGTH/2) {
					indent_stack[isp]
					= indent_stack[isp-1] + 4;
				}
				goto DO_INDENT;
			}
		} else if (c == INDENT2) {
			indent_stack[isp] = indent_stack[isp-1] + 2;
			goto PUT_INDENT;
		} else if (c < 0400)
			writec_PRINTstream(c);
		qh = mod(qh+1);
		--qc;
	}
	return;

DO_MARK:
	k = LINE_LENGTH - 1 - file_column(PRINTstream);
	for (i = 1, j = 0, l = 1;  l > 0 && i < qc && j < k;  i++) {
		c = queue[mod(qh + i)];
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
	if (i == qc && !force)
		return;
	qh = mod(qh+1);
	--qc;
	if (++isp >= ECL_PPRINT_INDENTATION_STACK_SIZE-1)
		FEerror("Can't pretty-print.", 0);
	indent_stack[isp++] = file_column(PRINTstream);
	indent_stack[isp] = indent_stack[isp-1];
	goto BEGIN;

DO_INDENT:
	if (iisp > isp)
		goto PUT_INDENT;
	k = LINE_LENGTH - 1 - file_column(PRINTstream);
	for (i0 = 0, i = 1, j = 0, l = 1;  i < qc && j < k;  i++) {
		c = queue[mod(qh + i)];
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
	if (i == qc && !force)
		return;
	if (i0 == 0)
		goto PUT_INDENT;
	i = i0;
	goto FLUSH;

PUT_INDENT:
	qh = mod(qh+1);
	--qc;
	writec_PRINTstream('\n');
	for (i = indent_stack[isp];  i > 0;  --i)
		writec_PRINTstream(' ');
	iisp = isp;
	goto BEGIN;

FLUSH:
	for (j = 0;  j < i;  j++) {
		c = queue[qh];
		if (c == INDENT || c == INDENT1 || c == INDENT2)
			writec_PRINTstream(' ');
		else if (c < 0400)
			writec_PRINTstream(c);
		qh = mod(qh+1);
		--qc;
	}
	goto BEGIN;
}

static void
write_ch(int c)
{
	if (PRINTpretty)
		writec_queue(c);
	else if (c == INDENT || c == INDENT1)
		writec_stream(' ', PRINTstream);
	else if (c < 0400)
		writec_stream(c, PRINTstream);
}

static void
write_str(char *s)
{
	while (*s != '\0')
		write_ch(*s++);
}

static void
write_decimal(cl_fixnum i)
{
	if (i == 0) {
		write_ch('0');
		return;
	}
	write_decimal1(i);
}

static void
write_decimal1(cl_fixnum i)
{
	if (i == 0)
		return;
	write_decimal1(i/10);
	write_ch(i%10 + '0');
}

static void
write_addr(cl_object x)
{
	cl_fixnum i, j;

	i = (cl_index)x;
	for (j = sizeof(i)*8-4;  j >= 0;  j -= 4) {
		int k = (i>>j) & 0xf;
		if (k < 10)
			write_ch('0' + k);
		else
			write_ch('a' + k - 10);
	}
}

static void
write_base(void)
{
	if (PRINTbase == 2)
		write_str("#b");
	else if (PRINTbase == 8)
		write_str("#o");
	else if (PRINTbase == 16)
		write_str("#x");
	else if (PRINTbase >= 10) {
		write_ch('#');
		write_ch(PRINTbase/10+'0');
		write_ch(PRINTbase%10+'0');
		write_ch('r');
	} else {
		write_ch('#');
		write_ch(PRINTbase+'0');
		write_ch('r');
	}
}

/* The floating point precision is required to make the
   most-positive-long-float printed expression readable.
   If this is too small, then the rounded off fraction, may be too big
   to read */

#ifndef FPRC 
#define FPRC 16
#endif

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
write_double(double d, int e, bool shortp)
{
	int sign;
	char buff[FPRC+5];
	int exp;
	int i;
	int n = FPRC;		/* was FPRC+1 */

	if (shortp)
		n = 7;
	edit_double(n, d, &sign, buff, &exp);
	if (sign==2) {
		write_str("#<");
		write_str(buff);
		write_ch('>');
		return;
	      }
	if (sign < 0)
		write_ch('-');
	if (-3 <= exp && exp < 7) {
		if (exp < 0) {
			write_ch('0');
			write_ch('.');
			exp = (-exp) - 1;
			for (i = 0;  i < exp;  i++)
				write_ch('0');
			for (;  n > 0;  --n)
				if (buff[n-1] != '0')
					break;
			if (exp == 0 && n == 0)
				n = 1;
			for (i = 0;  i < n;  i++)
				write_ch(buff[i]);
		} else {
			exp++;
			for (i = 0;  i < exp;  i++)
				if (i < n)
					write_ch(buff[i]);
				else
					write_ch('0');
			write_ch('.');
			if (i < n)
				write_ch(buff[i]);
			else
				write_ch('0');
			i++;
			for (;  n > i;  --n)
				if (buff[n-1] != '0')
					break;
			for (;  i < n;  i++)
				write_ch(buff[i]);
		}
		exp = 0;
	} else {
		write_ch(buff[0]);
		write_ch('.');
		write_ch(buff[1]);
		for (;  n > 2;  --n)
			if (buff[n-1] != '0')
				break;
		for (i = 2;  i < n;  i++)
			write_ch(buff[i]);
	}
	if (exp == 0 && e == 0)
		return;
	if (e == 0)
		e = 'E';
	write_ch(e);
	if (exp < 0) {
		write_ch('-');
		exp *= -1;
	}
	write_decimal(exp);
}


static void
#ifdef CLOS
call_print_object(cl_object x, int level)
#else
call_structure_print_function(cl_object x, int level)
#endif
{
	bool re = PRINTreadably;
	bool e = PRINTescape;
	bool r = PRINTradix;
	int b = PRINTbase;
	bool c = PRINTcircle;
	bool p = PRINTpretty;
	int lv = PRINTlevel;
	int ln = PRINTlength;
	bool g = PRINTgensym;
	bool a = PRINTarray;
	cl_object ps = PRINTstream;
	cl_object pc = PRINTcase;

	short ois[ECL_PPRINT_INDENTATION_STACK_SIZE];

	int oqh;
	int oqt;
	int oqc;
	int oisp;
	int oiisp;

	while (interrupt_flag) {
		interrupt_flag = FALSE;
#ifdef unix
		alarm(0);
#endif
		terminal_interrupt(TRUE);
	}

	if (PRINTpretty)
		flush_queue(TRUE);

	oqh = qh;
	oqt = qt;
	oqc = qc;
	oisp = isp;
	oiisp = iisp;
	memcpy(ois, indent_stack, isp * sizeof(*ois));

	CL_UNWIND_PROTECT_BEGIN {
		bds_bind(@'*print-readably*', PRINTreadably?Ct:Cnil);
		bds_bind(@'*print-escape*', PRINTescape?Ct:Cnil);
		bds_bind(@'*print-radix*', PRINTradix?Ct:Cnil);
		bds_bind(@'*print-base*', MAKE_FIXNUM(PRINTbase));
		bds_bind(@'*print-circle*', PRINTcircle?Ct:Cnil);
		bds_bind(@'*print-pretty*', PRINTpretty?Ct:Cnil);
		bds_bind(@'*print-level*', PRINTlevel<0?Cnil:MAKE_FIXNUM(PRINTlevel));
		bds_bind(@'*print-length*', PRINTlength<0?Cnil:MAKE_FIXNUM(PRINTlength));
		bds_bind(@'*print-gensym*', PRINTgensym?Ct:Cnil);
		bds_bind(@'*print-array*', PRINTarray?Ct:Cnil);
		bds_bind(@'*print-case*', PRINTcase);
#ifdef CLOS
		funcall(3, @'print-object', x, PRINTstream);
#else
		funcall(4, si_get_sysprop(x->str.name, @'si::structure-print-function'),
			x, PRINTstream, MAKE_FIXNUM(level));
#endif
		bds_unwind_n(11);
	} CL_UNWIND_PROTECT_EXIT {
		memcpy(indent_stack, ois, oisp * sizeof(*ois));
		iisp = oiisp;
		isp = oisp;
		qc = oqc;
		qt = oqt;
		qh = oqh;

		PRINTcase = pc;
		PRINTstream = ps;
		PRINTarray = a;
		PRINTgensym = g;
		PRINTlength = ln;
		PRINTlevel = lv;
		PRINTpretty = p;
		PRINTcircle = c;
		PRINTbase = b;
		PRINTradix = r;
		PRINTescape = e;
		PRINTreadably = re;
	} CL_UNWIND_PROTECT_END;
}

static void
write_fixnum(cl_fixnum i)
{
	/* The maximum number of digits is achieved for base 2 and it
	   is always < FIXNUM_BITS, since we use at least one bit for
	   tagging */
	short digits[FIXNUM_BITS];
	int j;
	for (j = 0;  j < FIXNUM_BITS && i != 0;  i /= PRINTbase)
		digits[j++] = digit_weight(i%PRINTbase, PRINTbase);
	while (j-- > 0)
		write_ch(digits[j]);
}

static void
write_bignum(cl_object x)
{
	cl_fixnum str_size = mpz_sizeinbase(x->big.big_num, PRINTbase);
	char str[str_size];		/* __GNUC__ */
	char *s = str;
	mpz_get_str(str, PRINTbase, x->big.big_num);
	while (*s)
	  write_ch(*s++);
}

static void
write_symbol(register cl_object x)
{
	bool escaped;
	cl_index i;
	cl_object s = x->symbol.name;
	int intern_flag;

	if (!PRINTescape && !PRINTreadably) {
		for (i = 0;  i < s->string.fillp;  i++) {
			int c = s->string.self[i];
			if (isupper(c) &&
			    (PRINTcase == @':downcase' ||
			     (PRINTcase == @':capitalize' && i != 0)))
				c = tolower(c);
			write_ch(c);
		}
		return;
	}
	if (Null(x->symbol.hpack)) {
		if (PRINTgensym)
			write_str("#:");
	} else if (x->symbol.hpack == keyword_package)
		write_ch(':');
	else if ((PRINTpackage != OBJNULL && x->symbol.hpack != PRINTpackage)
		 || find_symbol(x, current_package(), &intern_flag)!=x
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
			write_ch('|');
		for (i = 0;
		     i < x->symbol.hpack->pack.name->string.fillp;
		     i++) {
			int c = x->symbol.hpack->pack.name->string.self[i];
			if (c == '|' || c == '\\')
				write_ch('\\');
			if (escaped == 0 && isupper(c) &&
			    (PRINTcase == @':downcase' ||
			     (PRINTcase == @':capitalize' && i!=0)))
				c = tolower(c);
			write_ch(c);
		}
		if (escaped)
			write_ch('|');
		if (find_symbol(x, x->symbol.hpack, &intern_flag) != x)
			error("can't print symbol");
		if ((PRINTpackage != OBJNULL &&
		     x->symbol.hpack != PRINTpackage)
		    || intern_flag == INTERNAL)
			write_str("::");
		else if (intern_flag == EXTERNAL)
			write_ch(':');
		else
			FEerror("Pathological symbol --- cannot print.", 0);
	}
	escaped = 0;
	if (potential_number_p(s, PRINTbase))
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
		write_ch('|');
	for (i = 0;  i < s->string.fillp;  i++) {
		int c = s->string.self[i];
		if (c == '|' || c == '\\')
			write_ch('\\');
		if (escaped == 0 && isupper(c) &&
		    (PRINTcase == @':downcase' ||
		     (PRINTcase == @':capitalize' && i != 0)))
			c = tolower(c);
		write_ch(c);
	}
	if (escaped)
		write_ch('|');
}

static void
write_character(register int i)
{
	if (!PRINTescape && !PRINTreadably) {
		write_ch(i);
		return;
	}
	write_str("#\\");
	switch (i) {
	case '\r':	write_str("Return"); break;
	case ' ':	write_str("Space"); break;
	case '\177':	write_str("Rubout"); break;
	case '\f':	write_str("Page"); break;
	case '\t':	write_str("Tab"); break;
	case '\b':	write_str("Backspace");	break;
	case '\n':	write_str("Newline"); break;
	case '\0':	write_str("Null"); break;
	default:
		if (i & 0200) {
			write_ch('\\');
			write_ch(((i>>6)&7) + '0');
			write_ch(((i>>3)&7) + '0');
			write_ch(((i>>0)&7) + '0');
		} else if (i < 040) {
			write_ch('^');
			i += 0100;
			if (i == '\\')
				write_ch('\\');
			write_ch(i);
		} else
			write_ch(i);
		break;
	}
}

static void
_write_object(cl_object x, int level)
{
	cl_object r, y;
	cl_fixnum i, j;
	cl_index ndx, k;

	cs_check(x);

 BEGIN:
	if (x == OBJNULL) {
		write_str("#<OBJNULL>");
		return;
	}

	if (PRINTcircle &&
	    !IMMEDIATE(x) &&
	    !(type_of(x) == t_symbol && !Null(x->symbol.hpack)))
	{
		cl_fixnum code = search_print_circle(x);
		if (code) {
			if (!do_print_circle(code))
				return;
		}
	}

	switch (type_of(x)) {

	case FREE:
		write_str("#<FREE OBJECT ");
		write_addr(x);
		write_ch('>');
		return;

	case t_fixnum:
		if (PRINTradix && PRINTbase != 10)
			write_base();
		if (x == MAKE_FIXNUM(0)) {
			write_ch('0');
		} else if (FIXNUM_MINUSP(x)) {
			write_ch('-');
			write_fixnum(-fix(x));
		} else
			write_fixnum(fix(x));
		if (PRINTradix && PRINTbase == 10)
			write_ch('.');
		return;

	case t_bignum:
		if (PRINTradix && PRINTbase != 10)
			write_base();
		write_bignum(x);
		if (PRINTradix && PRINTbase == 10)
			write_ch('.');
		return;

	case t_ratio:
		if (PRINTradix) {
			write_base();
			PRINTradix = FALSE;
			_write_object(x->ratio.num, level);
			write_ch('/');
			_write_object(x->ratio.den, level);
			PRINTradix = TRUE;
		} else {
			_write_object(x->ratio.num, level);
			write_ch('/');
			_write_object(x->ratio.den, level);
		}
		return;

	case t_shortfloat:
		r = symbol_value(@'*read-default-float-format*');
		if (r == @'single-float' || r == @'short-float')
			write_double((double)sf(x), 0, TRUE);
		else
			write_double((double)sf(x), 'f', TRUE);
		return;

	case t_longfloat:
		r = symbol_value(@'*read-default-float-format*');
		if (r == @'long-float' || r == @'double-float')
			write_double(lf(x), 0, FALSE);
		else
			write_double(lf(x), 'd', FALSE);
		return;

	case t_complex:
		write_str("#C(");
		_write_object(x->complex.real, level);
		write_ch(' ');
		_write_object(x->complex.imag, level);
		write_ch(')');
		return;

	case t_character:
		write_character(CHAR_CODE(x));
		return;

	case t_symbol:
		write_symbol(x);
		return;

	case t_array: {
		cl_index subscripts[ARANKLIM];
		cl_index n, m, k, i;

		if (!PRINTarray && !PRINTreadably) {
			write_str("#<array ");
			write_addr(x);
			write_ch('>');
			return;
		}
		if (!PRINTreadably && PRINTlevel >= 0 && level >= PRINTlevel) {
			write_ch('#');
			return;
		}
		n = x->array.rank;
		write_ch('#');
		write_decimal(n);
		write_ch('A');
		if (!PRINTreadably && PRINTlevel >= 0 && level+n >= PRINTlevel)
			n = PRINTlevel - level;
		for (j = 0;  j < n;  j++)
			subscripts[j] = 0;
		for (m = 0, j = 0;;) {
			for (i = j;  i < n;  i++) {
				if (subscripts[i] == 0) {
					write_ch(MARK);
					write_ch('(');
					write_ch(SET_INDENT);
					if (x->array.dims[i] == 0) {
						write_ch(')');
						write_ch(UNMARK);
						j = i-1;
						k = 0;
						goto INC;
					}
				}
				if (subscripts[i] > 0)
					write_ch(INDENT);
				if (!PRINTreadably &&
				    PRINTlength >= 0 &&
				    subscripts[i] >= PRINTlength) {
					write_str("...)");
					write_ch(UNMARK);
					k=x->array.dims[i]-subscripts[i];
					subscripts[i] = 0;
					for (j = i+1;  j < n;  j++)
						k *= x->array.dims[j];
					j = i-1;
					goto INC;
				}
			}
			/* FIXME: This conses! */
			if (n == x->array.rank)
				_write_object(aref(x, m), level+n);
			else
				write_ch('#');
			j = n-1;
			k = 1;

		INC:
			while (j >= 0) {
				if (++subscripts[j] < x->array.dims[j])
					break;
				subscripts[j] = 0;
				write_ch(')');
				write_ch(UNMARK);
				--j;
			}
			if (j < 0)
				break;
			m += k;
		}
		return;
	}

	case t_vector:
		if (!PRINTarray && !PRINTreadably) {
			write_str("#<vector ");
			write_decimal(x->vector.dim);
			write_ch(' ');
			write_addr(x);
			write_ch('>');
			return;
		}
		if (!PRINTreadably && PRINTlevel >= 0 && level >= PRINTlevel) {
			write_ch('#');
			return;
		}
		write_ch('#');
		write_ch(MARK);
		write_ch('(');
		write_ch(SET_INDENT);
		if (x->vector.fillp > 0) {
			if (!PRINTreadably && PRINTlength == 0) {
				write_str("...)");
				write_ch(UNMARK);
				return;
			}
			_write_object(aref(x, 0), level+1);
			for (ndx = 1;  ndx < x->vector.fillp;  ndx++) {
				write_ch(INDENT);
				if (!PRINTreadably && PRINTlength>=0 && ndx>=PRINTlength){
					write_str("...");
					break;
				}
				_write_object(aref(x, ndx), level+1);
			}
		}
		write_ch(')');
		write_ch(UNMARK);
		return;

	case t_string:
		if (!PRINTescape && !PRINTreadably) {
			for (ndx = 0;  ndx < x->string.fillp;  ndx++)
				write_ch(x->string.self[ndx]);
			return;
		}
		write_ch('"');
		for (ndx = 0;  ndx < x->string.fillp;  ndx++) {
			int c = x->string.self[ndx];
			if (c == '"' || c == '\\')
				write_ch('\\');
			write_ch(c);
		}
		write_ch('"');
		break;

	case t_bitvector:
		if (!PRINTarray && !PRINTreadably) {
			write_str("#<bit-vector ");
			write_addr(x);
			write_ch('>');
			break;
		}
		write_str("#*");
		for (ndx = 0;  ndx < x->vector.fillp;  ndx++)
			if (x->vector.self.bit[ndx/8] & (0200 >> ndx%8))
				write_ch('1');
			else
				write_ch('0');
		break;

	case t_cons:
		if (CAR(x) == @'si::#!') {
			write_str("#!");
			x = CDR(x);
			goto BEGIN;
		}
		if (CAR(x) == @'quote' && CONSP(CDR(x)) && Null(CDDR(x))) {
			write_ch('\'');
			x = CADR(x);
			goto BEGIN;
		}
		if (CAR(x) == @'function' && CONSP(CDR(x)) && Null(CDDR(x))) {
			write_ch('#');
			write_ch('\'');
			x = CADR(x);
			goto BEGIN;
		}
		if (!PRINTreadably && PRINTlevel >= 0 && level >= PRINTlevel) {
			write_ch('#');
			return;
		}
		write_ch(MARK);
		write_ch('(');
		write_ch(SET_INDENT);
		if (PRINTpretty && CAR(x) != OBJNULL &&
		    type_of(CAR(x)) == t_symbol &&
		    (r = si_get_sysprop(CAR(x), @'si::pretty-print-format')) != Cnil)
			goto PRETTY_PRINT_FORMAT;
		for (i = 0;  ;  i++) {
			if (!PRINTreadably && PRINTlength >= 0 && i >= PRINTlength) {
				write_str("...");
				break;
			}
			y = CAR(x);
			x = CDR(x);
			_write_object(y, level+1);
			/* FIXME! */
			if (x == OBJNULL || ATOM(x) ||
			    (PRINTcircle && search_print_circle(x))) {
				if (x != Cnil) {
					write_ch(INDENT);
					write_str(". ");
					_write_object(x, level);
				}
				break;
			}
			if (i == 0 && y != OBJNULL && type_of(y) == t_symbol)
				write_ch(INDENT1);
			else
				write_ch(INDENT);
		}

	RIGHT_PAREN:
		write_ch(')');
		write_ch(UNMARK);
		return;

	PRETTY_PRINT_FORMAT:
		j = fixint(r);
		for (i = 0;  ;  i++) {
			if (!PRINTreadably && PRINTlength >= 0 && i >= PRINTlength) {
				write_str("...");
				break;
			}
			y = CAR(x);
			x = CDR(x);
			if (i <= j && Null(y))
				write_str("()");
			else
				_write_object(y, level+1);
			/* FIXME! */
			if (x == OBJNULL || ATOM(x) ||
			    (PRINTcircle && search_print_circle(x))) {
				if (x != Cnil) {
					write_ch(INDENT);
					write_str(". ");
					_write_object(x, level);
				}
				break;
			}
			if (i >= j)
				write_ch(INDENT2);
			else if (i == 0)
				write_ch(INDENT1);
			else
				write_ch(INDENT);
		}
		goto RIGHT_PAREN;

	case t_package:
		if (PRINTreadably) FEprint_not_readable(x);
		write_str("#<");
		_write_object(x->pack.name, level);
 		write_str(" package>");
		break;

	case t_hashtable:
		if (PRINTreadably) FEprint_not_readable(x);
		write_str("#<hash-table ");
		write_addr(x);
		write_ch('>');
		break;

	case t_stream:
		if (PRINTreadably) FEprint_not_readable(x);
		switch ((enum smmode)x->stream.mode) {
		case smm_closed:
			write_str("#<closed stream ");
			_write_object(x->stream.object1, level);
			break;

		case smm_input:
			write_str("#<input stream ");
			_write_object(x->stream.object1, level);
			break;

		case smm_output:
			write_str("#<output stream ");
			_write_object(x->stream.object1, level);
			break;

		case smm_io:
			write_str("#<io stream ");
			_write_object(x->stream.object1, level);
			break;

		case smm_probe:
			write_str("#<probe stream ");
			_write_object(x->stream.object1, level);
			break;

		case smm_synonym:
			write_str("#<synonym stream to ");
			_write_object(x->stream.object0, level);
			break;

		case smm_broadcast:
			write_str("#<broadcast stream ");
			write_addr(x);
			break;

		case smm_concatenated:
			write_str("#<concatenated stream ");
			write_addr(x);
			break;

		case smm_two_way:
			write_str("#<two-way stream ");
			write_addr(x);
			break;

		case smm_echo:
			write_str("#<echo stream ");
			write_addr(x);
			break;

		case smm_string_input:
			write_str("#<string-input stream from \"");
			y = x->stream.object0;
			k = y->string.fillp;
			for (ndx = 0;  ndx < k && ndx < 16;  ndx++)
				write_ch(y->string.self[ndx]);
			if (k > 16)
				write_str("...");
			write_ch('"');
			break;

		case smm_string_output:
			write_str("#<string-output stream ");
			write_addr(x);
			break;

		default:
			error("illegal stream mode");
		}
		write_ch('>');
		break;

	case t_random:
		write_str("#$");
		_write_object(MAKE_FIXNUM(x->random.value), level);
		break;

#ifndef CLOS
	case t_structure:
		if (!PRINTreadably && PRINTlevel >= 0 && level >= PRINTlevel) {
			write_ch('#');
			break;
		}
		if (type_of(x->str.name) != t_symbol)
			FEwrong_type_argument(@'symbol', x->str.name);
		if (PRINTstructure ||
		    Null(si_get_sysprop(x->str.name, @'si::structure-print-function')))
		{
			write_str("#S");
/* structure_to_list conses slot names and values into a list to be printed.
 * print shouldn't allocate memory - Beppe
 */
			x = structure_to_list(x);
			_write_object(x, level);
		} else
			call_structure_print_function(x, level);
		break;
#endif /* CLOS */

	case t_readtable:
		if (PRINTreadably) FEprint_not_readable(x);
		write_str("#<readtable ");
		write_addr(x);
		write_ch('>');
		break;

	case t_pathname:
		if (PRINTescape || PRINTreadably)
			write_str("#P");
		_write_object(cl_namestring(x), level);
		break;

	case t_bytecodes: {
		cl_object name = x->bytecodes.name;
		if (PRINTreadably) FEprint_not_readable(x);
		write_str("#<interpreted-function ");
		if (name != Cnil)
			_write_object(name, level);
		else
			write_addr(x);
		write_ch('>');
		break;
	}
	case t_cfun:
		if (PRINTreadably) FEprint_not_readable(x);
		write_str("#<compiled-function ");
		if (x->cfun.name != Cnil)
			_write_object(x->cfun.name, level);
		else
			write_addr(x);
		write_ch('>');
		break;
	case t_codeblock:
		if (PRINTreadably) FEprint_not_readable(x);
		write_str("#<codeblock ");
		if (x->cblock.name != Cnil)
			_write_object(x->cblock.name, level);
		else
			write_addr(x);
		write_ch('>');
		break;
	case t_cclosure:
		if (PRINTreadably) FEprint_not_readable(x);
		write_str("#<compiled-closure ");
		write_addr(x);
		write_ch('>');
		break;
#ifdef THREADS
      	case t_cont:
		if (PRINTreadably) FEprint_not_readable(x);
		write_str("#<cont ");
		_write_object(x->cn.cn_thread, level);
		write_ch('>');
		break;

	case t_thread:
		if (PRINTreadably) FEprint_not_readable(x);
		write_str("#<thread ");
		_write_object(x->thread.entry, level);
		write_ch(' ');
		write_addr(x);
		write_ch('>');
		break;
#endif /* THREADS */
#ifdef CLOS
	case t_instance:
		if (type_of(CLASS_OF(x)) != t_instance)
			FEwrong_type_argument(@'instance', CLASS_OF(x));
		call_print_object(x, level);
		break;

	case t_gfun:
		if (PRINTreadably) FEprint_not_readable(x);
		write_str("#<dispatch-function ");
		if (x->gfun.name != Cnil)
			_write_object(x->gfun.name, level);
		else
			write_addr(x);
		write_ch('>');
		break;
#endif /* CLOS */
	default:
		if (PRINTreadably) FEprint_not_readable(x);
		write_str("#<illegal pointer ");
		write_addr(x);
		write_ch('>');
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
	cl_object code;

	if (CIRCLEcounter < 0) {
		code = gethash_safe(x, CIRCLEstack, OBJNULL);
		if (code == OBJNULL) {
			/* Was not found before */
			sethash(x, CIRCLEstack, Cnil);
			return 0;
		} else if (code == Cnil) {
			/* This object is referenced twice */
			sethash(x, CIRCLEstack, Ct);
			return 1;
		} else {
			return 2;
		}
	} else {
		code = gethash_safe(x, CIRCLEstack, OBJNULL);
		if (code == OBJNULL || code == Cnil) {
			/* Is not referenced or was not found before */
			sethash(x, CIRCLEstack, Cnil);
			return 0;
		} else if (code == Ct) {
			/* This object is referenced twice, but has no code yet */
			cl_fixnum new_code = ++CIRCLEcounter;
			sethash(x, CIRCLEstack, MAKE_FIXNUM(new_code));
			return -new_code;
		} else {
			return fix(code);
		}
	}
}

static bool
do_print_circle(cl_fixnum code)
{
	if (CIRCLEcounter < 0) {
		/* Only run through object when it was not referenced before */
		return (code == 0);
	} else if (code == 0) {
		/* Object is not referenced twice */
		return TRUE;
	} else if (code < 0) {
		/* Object is referenced twice. We print its definition */
		write_ch('#');
		write_decimal(-code);
		write_ch('=');
		return TRUE;
	} else {
		/* Second reference to the object */
		write_ch('#');
		write_decimal(code);
		write_ch('#');
		return FALSE;
	}
}

void
cl_setup_printer(cl_object strm)
{
	cl_object y;

	PRINTstream = stream_or_default_output(strm);
	PRINTreadably = symbol_value(@'*print-readably*') != Cnil;
	PRINTescape = symbol_value(@'*print-escape*') != Cnil;
	PRINTpretty = symbol_value(@'*print-pretty*') != Cnil;
	PRINTcircle = symbol_value(@'*print-circle*') != Cnil;
	y = symbol_value(@'*print-base*');
	if (!FIXNUMP(y) || fix(y) < 2 || fix(y) > 36) {
		SYM_VAL(@'*print-base*') = MAKE_FIXNUM(10);
		FEerror("~S is an illegal PRINT-BASE.", 1, y);
	} else
		PRINTbase = fix(y);
	PRINTradix = symbol_value(@'*print-radix*') != Cnil;
	PRINTcase = symbol_value(@'*print-case*');
	if (PRINTcase != @':upcase' && PRINTcase != @':downcase' &&
	    PRINTcase != @':capitalize') {
		SYM_VAL(@'*print-case*') = @':downcase';
		FEerror("~S is an illegal PRINT-CASE.", 1, PRINTcase);
	}
	PRINTgensym = symbol_value(@'*print-gensym*') != Cnil;
	y = symbol_value(@'*print-level*');
	if (Null(y))
		PRINTlevel = -1;
	else if (!FIXNUMP(y) || fix(y) < 0) {
		SYM_VAL(@'*print-level*') = Cnil;
		FEerror("~S is an illegal PRINT-LEVEL.", 1, y);
	} else
		PRINTlevel = fix(y);
	y = symbol_value(@'*print-length*');
	if (Null(y))
		PRINTlength = -1;
	else if (!FIXNUMP(y) || fix(y) < 0) {
		SYM_VAL(@'*print-length*') = Cnil;
		FEerror("~S is an illegal PRINT-LENGTH.", 1, y);
	} else
		PRINTlength = fix(y);
	PRINTarray = symbol_value(@'*print-array*') != Cnil;
	PRINTpackage = symbol_value(@'si::*print-package*');
	if (PRINTpackage == Cnil) PRINTpackage = OBJNULL;
	PRINTstructure = symbol_value(@'si::*print-structure*') != Cnil;
}

void
cl_write_object(cl_object x)
{
	if (PRINTcircle && CIRCLEcounter == -2) {
		cl_object real_stream = PRINTstream;

		CIRCLEcounter = -1;
		PRINTstream = no_stream;
		cl_write_object(x);

		CIRCLEcounter = 0;
		PRINTstream = real_stream;
		cl_write_object(x);

		cl_clrhash(CIRCLEstack);
		CIRCLEcounter = -2;
		return;
	}
	if (PRINTpretty) {
		qh = qt = qc = 0;
		isp = iisp = 0;
		indent_stack[0] = 0;
	}
	_write_object(x, 0);
	if (PRINTpretty)
		flush_queue(TRUE);
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
@
	PRINTstream = stream_or_default_output(strm);
	PRINTreadably = readably != Cnil;
	PRINTescape = escape != Cnil;
	PRINTpretty = pretty != Cnil;
	PRINTcircle = circle != Cnil;
	if (!FIXNUMP(base) || fix((base))<2 || fix((base))>36)
		FEerror("~S is an illegal PRINT-BASE.", 1, base);
	else
		PRINTbase = fix((base));
	PRINTradix = radix != Cnil;
	PRINTcase = cas;
	if (PRINTcase != @':upcase' && PRINTcase != @':downcase' &&
	    PRINTcase != @':capitalize')
		FEerror("~S is an illegal PRINT-CASE.", 1, cas);
	PRINTgensym = gensym != Cnil;
	if (Null(level))
		PRINTlevel = -1;
	else if (!FIXNUMP(level) || fix((level)) < 0)
		FEerror("~S is an illegal PRINT-LEVEL.", 1, level);
	else
		PRINTlevel = fix((level));
	if (Null(length))
		PRINTlength = -1;
	else if (!FIXNUMP(length) || fix((length)) < 0)
		FEerror("~S is an illegal PRINT-LENGTH.", 1, length);
	else
		PRINTlength = fix((length));
	PRINTarray = array != Cnil;
	PRINTpackage = symbol_value(@'si::*print-package*');
	if (PRINTpackage == Cnil) PRINTpackage = OBJNULL;
	PRINTstructure = symbol_value(@'si::*print-structure*') != Cnil;

	cl_write_object(x);
	flush_stream(PRINTstream);
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
	cl_setup_printer(strm);
	PRINTescape = TRUE;
	PRINTpretty = TRUE;
	writec_PRINTstream('\n');
	cl_write_object(obj);
	flush_stream(PRINTstream);
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
	cl_index s, e, i;
@
	get_string_start_end(strng, start, end, &s, &e);
 	strm = stream_or_default_output(strm);
	assert_type_string(strng);

	for (i = s;  i < e;  i++)
		writec_stream(strng->string.self[i], strm);
	flush_stream(strm);
	@(return strng)
@)

@(defun write-line (strng &o strm &k (start MAKE_FIXNUM(0)) end)
	cl_index s, e, i;
@
	get_string_start_end(strng, start, end, &s, &e);
 	strm = stream_or_default_output(strm);
	assert_type_string(strng);

	for (i = s;  i < e;  i++)
		writec_stream(strng->string.self[i], strm);
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
	assert_type_stream(binary_output_stream);
	writec_stream(fix(integer), binary_output_stream);
	@(return integer)
}

cl_object
si_write_bytes(cl_object stream, cl_object string, cl_object start, cl_object end)
{
	FILE *fp;
        cl_index is, ie;
	cl_fixnum written, sofarwritten, towrite;

	assert_type_stream(stream);
	if (stream->stream.mode == smm_closed)
		FEclosed_stream(stream);

        is = fix(start);	/* FIXME: Unsafe! */
	ie = fix(end);
        sofarwritten = is;
	towrite = ie-is;
        fp = stream->stream.file;
	if (fp == NULL) fp = stream->stream.object1->stream.file;
	while (towrite > 0) {
	  written = write(fileno(fp),
			  string->string.self+sofarwritten, towrite);
	  if (written != -1) {
	    towrite -= written;
	    sofarwritten += written;
	  }
	  else @(return Cnil)
	}
	@(return MAKE_FIXNUM(sofarwritten - is))
}

void
init_print(void)
{
	SYM_VAL(@'*print-readably*') = Cnil;
	SYM_VAL(@'*print-escape*') = Ct;
	SYM_VAL(@'*print-pretty*') = Ct;
	SYM_VAL(@'*print-circle*') = Cnil;
	SYM_VAL(@'*print-base*') = MAKE_FIXNUM(10);
	SYM_VAL(@'*print-radix*') = Cnil;
	SYM_VAL(@'*print-case*') = @':upcase';
	SYM_VAL(@'*print-gensym*') = Ct;
	SYM_VAL(@'*print-level*') = Cnil;
	SYM_VAL(@'*print-length*') = Cnil;
	SYM_VAL(@'*print-array*') = Ct;

	SYM_VAL(@'si::*print-package*') = Cnil;
	SYM_VAL(@'si::*print-structure*') = Cnil;

	PRINTstream = Cnil;
	ecl_register_static_root(&PRINTstream);
	PRINTescape = TRUE;
	PRINTpretty = FALSE;
	PRINTcircle = FALSE;
	PRINTbase = 10;
	PRINTradix = FALSE;
	PRINTcase = @':upcase';
	ecl_register_static_root(&PRINTcase);
	PRINTgensym = TRUE;
	PRINTlevel = -1;
	PRINTlength = -1;
	PRINTarray = FALSE;

	CIRCLEstack = cl__make_hash_table(@'eq', MAKE_FIXNUM(1024), make_shortfloat(1.5),	
					  make_shortfloat(0.7));
	ecl_register_static_root(&CIRCLEstack);

	no_stream = @make_broadcast_stream(0);
	ecl_register_static_root(&no_stream);
}

cl_object
princ(cl_object obj, cl_object strm)
{
	cl_setup_printer(strm);
	PRINTescape = FALSE;
	PRINTreadably = FALSE;
	cl_write_object(obj);
	return obj;
}

cl_object
prin1(cl_object obj, cl_object strm)
{
	cl_setup_printer(strm);
	PRINTescape = TRUE;
	cl_write_object(obj);
	flush_stream(PRINTstream);
	return obj;
}

cl_object
print(cl_object obj, cl_object strm)
{
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

