/*
    format.c -- Format.
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
#include <ctype.h>

cl_object @'si::*indent-formatted-output*';

/******************* WITH CLOS ************************/
#ifdef CLOS
#define	WRITEC_STREAM(c, strm)		(*fmt_writec)(c, strm)
#define WRITESTR_STREAM(s, strm) \
  { if (type_of(strm) == t_stream) \
       writestr_stream(s, strm);\
    else {\
       const char* t=s; \
       while (*t != '\0') interactive_writec_stream(*t++, strm); \
  }}
#define FLUSH_STREAM(strm) {\
  if (type_of(strm) == t_stream) flush_stream(strm); \
     else flush_interactive_stream(strm);\
  }
#define FILE_COLUMN(strm) \
  ((type_of(strm) == t_instance) ? -1 : file_column(strm))
#endif CLOS

/******************* WITHOUT CLOS *********************/
#ifndef CLOS
#define	WRITEC_STREAM(c, strm)		writec_stream(c, strm)
#define WRITESTR_STREAM(s, strm)	writestr_stream(s, strm);
#define FLUSH_STREAM(strm)		flush_stream(strm)
#define FILE_COLUMN(strm) 		file_column(strm)
#endif !CLOS

/******************* WITH THREADS *********************/
#ifdef THREADS
#define fmt_writec	clwp->lwp_fmt_ch_fun
#define fmt_stream	clwp->lwp_fmt_stream
#define ctl_origin	clwp->lwp_ctl_origin
#define ctl_index	clwp->lwp_ctl_index
#define ctl_end		clwp->lwp_ctl_end
#define fmt_base	clwp->lwp_fmt_base
#define fmt_index	clwp->lwp_fmt_index
#define fmt_end		clwp->lwp_fmt_end
#define fmt_jmp_buf	clwp->lwp_fmt_jmp_buf
#define fmt_indents	clwp->lwp_fmt_indents
#define fmt_string	clwp->lwp_fmt_string
#define fmt_temporary_stream clwp->lwp_fmt_temporary_stream
#define fmt_temporary_string clwp->lwp_fmt_temporary_string
#define fmt_nparam	clwp->lwp_fmt_nparam
#define fmt_param	clwp->lwp_fmt_param
#define fmt_spare_spaces clwp->lwp_fmt_spare_spaces
#define fmt_line_length clwp->lwp_fmt_line_length
#endif

/******************* WITHOUT THREADS ******************/
#ifndef THREADS
static int (*fmt_writec)();
static cl_object fmt_stream;
static int ctl_origin;
static int ctl_index;
static int ctl_end;
static cl_object *fmt_base;
static int fmt_index;
static int fmt_end;
static int *fmt_jmp_buf;
static int fmt_indents;
static cl_object fmt_string;
static cl_object fmt_temporary_stream;
static cl_object fmt_temporary_string;
static int fmt_nparam;
struct {
	int fmt_param_type;
	int fmt_param_value;
} fmt_param[100];
static int fmt_spare_spaces;
static int fmt_line_length;
#endif !THREADS

/******************* COMMON ***************************/

#define	ctl_string	(fmt_string->string.self + ctl_origin)

#define	fmt_old		volatile cl_object old_fmt_stream; \
			volatile int old_ctl_origin; \
			volatile int old_ctl_index; \
			volatile int old_ctl_end; \
			cl_object * volatile old_fmt_base; \
			volatile int old_fmt_index; \
			volatile int old_fmt_end; \
			int * volatile old_fmt_jmp_buf; \
			volatile int old_fmt_indents; \
			volatile cl_object old_fmt_string
#define	fmt_save	old_fmt_stream = fmt_stream; \
			old_ctl_origin = ctl_origin; \
			old_ctl_index = ctl_index; \
			old_ctl_end = ctl_end; \
			old_fmt_base = fmt_base; \
			old_fmt_index = fmt_index; \
			old_fmt_end = fmt_end; \
			old_fmt_jmp_buf = fmt_jmp_buf; \
			old_fmt_indents = fmt_indents; \
			old_fmt_string = fmt_string
#define	fmt_restore	fmt_stream = old_fmt_stream; \
			ctl_origin = old_ctl_origin; \
			ctl_index = old_ctl_index; \
			ctl_end = old_ctl_end; \
			fmt_base = old_fmt_base; \
			fmt_index = old_fmt_index; \
			fmt_end = old_fmt_end; \
			fmt_jmp_buf = old_fmt_jmp_buf; \
			fmt_indents = old_fmt_indents; \
			fmt_string = old_fmt_string
#define	fmt_restore1	fmt_stream = old_fmt_stream; \
			ctl_origin = old_ctl_origin; \
			ctl_index = old_ctl_index; \
			ctl_end = old_ctl_end; \
			fmt_jmp_buf = old_fmt_jmp_buf; \
			fmt_indents = old_fmt_indents; \
			fmt_string = old_fmt_string

#define NONE	0
#define	INT	1
#define	CHAR	2

static const char *fmt_big_numeral[] = {
	"thousand",
	"million",
	"billion",
	"trillion",
	"quadrillion",
	"quintillion",
	"sextillion",
	"septillion",
	"octillion"
};

static const char *fmt_numeral[] = {
	"zero", "one", "two", "three", "four",
	"five", "six", "seven", "eight", "nine",
	"ten", "eleven", "twelve", "thirteen", "fourteen",
	"fifteen", "sixteen", "seventeen", "eighteen", "nineteen",
	"zero", "ten", "twenty", "thirty", "forty",
	"fifty", "sixty", "seventy", "eighty", "ninety"
};

static const char *fmt_ordinal[] = {
	"zeroth", "first", "second", "third", "fourth",
	"fifth", "sixth", "seventh", "eighth", "ninth",
	"tenth", "eleventh", "twelfth", "thirteenth", "fourteenth",
	"fifteenth", "sixteenth", "seventeenth", "eighteenth", "nineteenth",
	"zeroth", "tenth", "twentieth", "thirtieth", "fortieth",
	"fiftieth", "sixtieth", "seventieth", "eightieth", "ninetieth"
};

static void format(cl_object, int, int);

static void
fmt_error(char *s)
{
	FEerror("Format error: ~A.~%~V@@TV~%\"~A\"~%",
		3, make_simple_string(s),
		MAKE_FIXNUM(&ctl_string[ctl_index] - fmt_string->string.self),
		fmt_string);
}

static int
fmt_tempstr(int s)
{
	return(fmt_temporary_string->string.self[s]);
}

static int
ctl_advance(void)
{
	if (ctl_index >= ctl_end)
		fmt_error("unexpected end of control string");
	return(ctl_string[ctl_index++]);
}

static cl_object
fmt_advance(void)
{
	if (fmt_index >= fmt_end)
		fmt_error("arguments exhausted");
	return(fmt_base[fmt_index++]);
}

static int
fmt_skip(void)
{
	int c, level = 0;
	
LOOP:
	if (ctl_advance() != '~')
		goto LOOP;
	for (;;)
		switch (c = ctl_advance()) {
		case '\'':
			ctl_advance();

		case ',':
		case '0':  case '1':  case '2':  case '3':  case '4':
		case '5':  case '6':  case '7':  case '8':  case '9':
		case '+':
		case '-':
		case 'v':  case 'V':
		case '#':
		case ':':  case '@@':
			continue;

		default:
			goto DIRECTIVE;
		}

DIRECTIVE:
	switch (c) {
	case '(':  case '[':  case '<':  case '{':
		level++;
		break;

	case ')':  case ']':  case '>':  case '}':
		if (level == 0)
			return(ctl_index);
		else
			--level;
		break;

	case ';':
		if (level == 0)
			return(ctl_index);
		break;
	}
	goto LOOP;
}

static void
fmt_max_param(int n)
{
	if (fmt_nparam > n)
		fmt_error("too many parameters");
}

static void
fmt_not_colon(bool colon)
{
	if (colon)
		fmt_error("illegal :");
}

static void
fmt_not_atsign(bool atsign)
{
	if (atsign)
		fmt_error("illegal @@");
}

static void
fmt_not_colon_atsign(bool colon, bool atsign)
{
	if (colon && atsign)
		fmt_error("illegal :@@");
}

static void
fmt_set_param(int i, int *p, int t, int v)
{
	if (i >= fmt_nparam || fmt_param[i].fmt_param_type == 0)
		*p = v;
	else if (fmt_param[i].fmt_param_type != t)
		fmt_error("illegal parameter type");
	else
		*p = fmt_param[i].fmt_param_value;
}	

static void
fmt_set_param_positive(int i, int *p, const char *message)
{
	if (i >= fmt_nparam || fmt_param[i].fmt_param_type == 0)
		*p = -1;
	else if (fmt_param[i].fmt_param_type != INT)
		fmt_error("illegal parameter type");
	else {
		*p = fmt_param[i].fmt_param_value;
		if (*p < 0) fmt_error(message);
	}
}	

static void
fmt_ascii(bool colon, bool atsign)
{
	int mincol, colinc, minpad, padchar;
	cl_object x;
	int l, i;

	fmt_max_param(4);
	fmt_set_param(0, &mincol, INT, 0);
	fmt_set_param(1, &colinc, INT, 1);
	fmt_set_param(2, &minpad, INT, 0);
	fmt_set_param(3, &padchar, CHAR, ' ');

	fmt_temporary_string->string.fillp = 0;
	fmt_temporary_stream->stream.int0 = FILE_COLUMN(fmt_stream);
	fmt_temporary_stream->stream.int1 = FILE_COLUMN(fmt_stream);
	x = fmt_advance();
	if (colon && Null(x))
		writestr_stream("()", fmt_temporary_stream);
	else if (mincol == 0 && minpad == 0) {
		princ(x, fmt_stream);
		return;
	} else
		princ(x, fmt_temporary_stream);
	l = fmt_temporary_string->string.fillp;
	for (i = minpad;  l + i < mincol;  i += colinc)
		;
	if (!atsign) {
		write_string(fmt_temporary_string, fmt_stream);
		while (i-- > 0)
			WRITEC_STREAM(padchar, fmt_stream);
	} else {
		while (i-- > 0)
			WRITEC_STREAM(padchar, fmt_stream);
		write_string(fmt_temporary_string, fmt_stream);
	}
}

static void
fmt_S_expression(bool colon, bool atsign)
{
	int mincol, colinc, minpad, padchar;
	cl_object x;
	int l, i;

	fmt_max_param(4);
	fmt_set_param(0, &mincol, INT, 0);
	fmt_set_param(1, &colinc, INT, 1);
	fmt_set_param(2, &minpad, INT, 0);
	fmt_set_param(3, &padchar, CHAR, ' ');

	fmt_temporary_string->string.fillp = 0;
	fmt_temporary_stream->stream.int0 = FILE_COLUMN(fmt_stream);
	fmt_temporary_stream->stream.int1 = FILE_COLUMN(fmt_stream);
	x = fmt_advance();
	if (colon && Null(x))
		writestr_stream("()", fmt_temporary_stream);
	else if (mincol == 0 && minpad == 0) {
		prin1(x, fmt_stream);
		return;
	} else
		prin1(x, fmt_temporary_stream);
	l = fmt_temporary_string->string.fillp;
	for (i = minpad;  l + i < mincol;  i += colinc)
		;
	if (!atsign) {
		write_string(fmt_temporary_string, fmt_stream);
		while (i-- > 0)
			WRITEC_STREAM(padchar, fmt_stream);
	} else {
		while (i-- > 0)
			WRITEC_STREAM(padchar, fmt_stream);
		write_string(fmt_temporary_string, fmt_stream);
	}
}


static void
fmt_integer(cl_object x, bool colon, bool atsign,
	    int radix, int mincol, int padchar, int commachar)
{
	int l, l1;
	int s;

	if (!FIXNUMP(x) && type_of(x) != t_bignum) {
		fmt_temporary_string->string.fillp = 0;
		fmt_temporary_stream->stream.int0 = FILE_COLUMN(fmt_stream);
		fmt_temporary_stream->stream.int1 = FILE_COLUMN(fmt_stream);
		setupPRINT(x, fmt_temporary_stream);
		PRINTescape = FALSE;
		PRINTbase = radix;
		write_object(x, 0);
		cleanupPRINT();
		l = fmt_temporary_string->string.fillp;
		mincol -= l;
		while (mincol-- > 0)
			WRITEC_STREAM(padchar, fmt_stream);
		for (s = 0;  l > 0;  --l, s++)
			WRITEC_STREAM(fmt_tempstr(s), fmt_stream);
		return;
	}
	fmt_temporary_string->string.fillp = 0;
	fmt_temporary_stream->stream.int0 = FILE_COLUMN(fmt_stream);
	fmt_temporary_stream->stream.int1 = FILE_COLUMN(fmt_stream);
	PRINTstream = fmt_temporary_stream;
	PRINTradix = FALSE;
	PRINTbase = radix;
	write_ch_fun = writec_PRINTstream;
	write_object(x, 0);
	l = l1 = fmt_temporary_string->string.fillp;
	s = 0;
	if (fmt_tempstr(s) == '-')
		--l1;
	mincol -= l;
	if (colon)
		mincol -= (l1 - 1)/3;
	if (atsign && fmt_tempstr(s) != '-')
		--mincol;
	while (mincol-- > 0)
		WRITEC_STREAM(padchar, fmt_stream);
	if (fmt_tempstr(s) == '-') {
		s++;
		WRITEC_STREAM('-', fmt_stream);
	} else if (atsign)
		WRITEC_STREAM('+', fmt_stream);
	while (l1-- > 0) {
		WRITEC_STREAM(fmt_tempstr(s++), fmt_stream);
		if (colon && l1 > 0 && l1%3 == 0)
			WRITEC_STREAM(commachar, fmt_stream);
	}
}

static void
fmt_decimal(bool colon, bool atsign)
{
	int mincol, padchar, commachar;

	fmt_max_param(3);
	fmt_set_param(0, &mincol, INT, 0);
	fmt_set_param(1, &padchar, CHAR, ' ');
	fmt_set_param(2, &commachar, CHAR, ',');
	fmt_integer(fmt_advance(), colon, atsign,
		    10, mincol, padchar, commachar);
}

static void
fmt_binary(bool colon, bool atsign)
{
	int mincol, padchar, commachar;

	fmt_max_param(3);
	fmt_set_param(0, &mincol, INT, 0);
	fmt_set_param(1, &padchar, CHAR, ' ');
	fmt_set_param(2, &commachar, CHAR, ',');
	fmt_integer(fmt_advance(), colon, atsign,
		    2, mincol, padchar, commachar);
}

static void
fmt_octal(bool colon, bool atsign)
{
	int mincol, padchar, commachar;

	fmt_max_param(3);
	fmt_set_param(0, &mincol, INT, 0);
	fmt_set_param(1, &padchar, CHAR, ' ');
	fmt_set_param(2, &commachar, CHAR, ',');
	fmt_integer(fmt_advance(), colon, atsign,
		    8, mincol, padchar, commachar);
}

static void
fmt_hexadecimal(bool colon, bool atsign)
{
	int mincol, padchar, commachar;

	fmt_max_param(3);
	fmt_set_param(0, &mincol, INT, 0);
	fmt_set_param(1, &padchar, CHAR, ' ');
	fmt_set_param(2, &commachar, CHAR, ',');
	fmt_integer(fmt_advance(), colon, atsign,
		    16, mincol, padchar, commachar);
}

static void
fmt_write_numeral(int s, int i)
{
	WRITESTR_STREAM(fmt_numeral[fmt_tempstr(s) - '0' + i], fmt_stream)
}

static void
fmt_write_ordinal(int s, int i)
{
	WRITESTR_STREAM(fmt_ordinal[fmt_tempstr(s) - '0' + i], fmt_stream)
}

static bool
fmt_thousand(int s, int i, bool b, bool o, int t)
{
	if (i == 3 && fmt_tempstr(s) > '0') {
		if (b)
			WRITEC_STREAM(' ', fmt_stream);
		fmt_write_numeral(s, 0);
		WRITESTR_STREAM(" hundred", fmt_stream)
		--i;
		s++;
		b = TRUE;
		if (o && (s > t))
			WRITESTR_STREAM("th", fmt_stream)
	}
	if (i == 3) {
		--i;
		s++;
	}
	if (i == 2 && fmt_tempstr(s) > '0') {
		if (b)
			WRITEC_STREAM(' ', fmt_stream);
		if (fmt_tempstr(s) == '1') {
			if (o && (s + 2 > t))
				fmt_write_ordinal(++s, 10);
			else
				fmt_write_numeral(++s, 10);
			return(TRUE);
		} else {
			if (o && (s + 1 > t))
				fmt_write_ordinal(s, 20);
			else
				fmt_write_numeral(s, 20);
			s++;
			if (fmt_tempstr(s) > '0') {
				WRITEC_STREAM('-', fmt_stream);
				if (o && s + 1 > t)
					fmt_write_ordinal(s, 0);
				else
					fmt_write_numeral(s, 0);
			}
			return(TRUE);
		}
	}
	if (i == 2)
		s++;
	if (fmt_tempstr(s) > '0') {
		if (b)
			WRITEC_STREAM(' ', fmt_stream);
		if (o && s + 1 > t)
			fmt_write_ordinal(s, 0);
		else
			fmt_write_numeral(s, 0);
		return(TRUE);
	}
	return(b);
}

static bool
fmt_nonillion(int s, int i, bool b, bool o, int t)
{
	int j;

	for (;  i > 3;  i -= j) {
		b = fmt_thousand(s, j = (i+2)%3+1, b, FALSE, t);
		if (j != 3 || fmt_tempstr(s) != '0' ||
		    fmt_tempstr(s+1) != '0' || fmt_tempstr(s+2) != '0') {
			WRITEC_STREAM(' ', fmt_stream);
			WRITESTR_STREAM(fmt_big_numeral[(i - 1)/3 - 1],
					fmt_stream)
			s += j;
			if (o && s > t)
				WRITESTR_STREAM("th", fmt_stream)
		} else
			s += j;
	}
	return(fmt_thousand(s, i, b, o, t));
}		

static void
fmt_roman(int i, int one, int five, int ten, bool colon)
{
	int j;

	if (i == 0)
		return;
	if ((!colon && i < 4) || (colon && i < 5))
		for (j = 0;  j < i;  j++)
			WRITEC_STREAM(one, fmt_stream);
	else if (!colon && i == 4) {
		WRITEC_STREAM(one, fmt_stream);
		WRITEC_STREAM(five, fmt_stream);
	} else if ((!colon && i < 9) || colon) {
		WRITEC_STREAM(five, fmt_stream);
		for (j = 5;  j < i;  j++)
			WRITEC_STREAM(one, fmt_stream);
	} else if (!colon && i == 9) {
		WRITEC_STREAM(one, fmt_stream);
		WRITEC_STREAM(ten, fmt_stream);
	}
}

static void
fmt_radix(bool colon, bool atsign)
{
	int radix, mincol, padchar, commachar;
	cl_object x;
	int i, j, k;
	int s, t;
	bool b;

	if (fmt_nparam == 0) {
		x = fmt_advance();
		assert_type_integer(x);
		if (atsign) {
			if (FIXNUMP(x))
				i = fix(x);
			else
				i = -1;
			if ((!colon && (i <= 0 || i >= 4000)) ||
			    (colon && (i <= 0 || i >= 5000))) {
				fmt_integer(x, FALSE, FALSE, 10, 0, ' ', ',');
				return;
			}
			fmt_roman(i/1000, 'M', '*', '*', colon);
			fmt_roman(i%1000/100, 'C', 'D', 'M', colon);
			fmt_roman(i%100/10, 'X', 'L', 'C', colon);
			fmt_roman(i%10, 'I', 'V', 'X', colon);
			return;
		}
		fmt_temporary_string->string.fillp = 0;
		fmt_temporary_stream->stream.int0 = FILE_COLUMN(fmt_stream);
		fmt_temporary_stream->stream.int1 = FILE_COLUMN(fmt_stream);
		PRINTstream = fmt_temporary_stream;
		PRINTradix = FALSE;
		PRINTbase = 10;
		write_ch_fun = writec_PRINTstream;
		write_object(x, 0);
		s = 0;
		i = fmt_temporary_string->string.fillp;
		if (i == 1 && fmt_tempstr(s) == '0') {
			WRITESTR_STREAM("zero", fmt_stream)
			if (colon)
				WRITESTR_STREAM("th", fmt_stream)
			return;
		} else if (fmt_tempstr(s) == '-') {
			WRITESTR_STREAM("minus ", fmt_stream)
			--i;
			s++;
		}
		t = fmt_temporary_string->string.fillp;
		for (; fmt_tempstr(--t) == '0' ;) ;
		for (b = FALSE;  i > 0;  i -= j) {
			b = fmt_nonillion(s, j = (i+29)%30+1, b,
					  i<=30&&colon, t);
			s += j;
			if (b && i > 30) {
				for (k = (i - 1)/30;  k > 0;  --k)
					WRITESTR_STREAM(" nonillion",
							fmt_stream)
				if (colon && s > t)
					WRITESTR_STREAM("th", fmt_stream)
			}
		}
		return;
	}
	fmt_max_param(4);
	fmt_set_param(0, &radix, INT, 10);
	fmt_set_param(1, &mincol, INT, 0);
	fmt_set_param(2, &padchar, CHAR, ' ');
	fmt_set_param(3, &commachar, CHAR, ',');
	x = fmt_advance();
	assert_type_integer(x);
	if (radix < 0 || radix > 36)
		FEerror("~D is illegal as a radix.", 1, MAKE_FIXNUM(radix));
	fmt_integer(x, colon, atsign, radix, mincol, padchar, commachar);
}	

static void
fmt_plural(bool colon, bool atsign)
{
	fmt_max_param(0);
	if (colon) {
		if (fmt_index == 0)
			fmt_error("can't back up");
		--fmt_index;
	}
	if (eql(fmt_advance(), MAKE_FIXNUM(1))) {
		if (atsign)
			WRITEC_STREAM('y', fmt_stream);
	      }
	else
		if (atsign)
			WRITESTR_STREAM("ies", fmt_stream)
		else
			WRITEC_STREAM('s', fmt_stream);
}

static void
fmt_character(bool colon, bool atsign)
{
	cl_object x;
	cl_index i;

	fmt_max_param(0);
	fmt_temporary_string->string.fillp = 0;
	fmt_temporary_stream->stream.int0 = 0;
	fmt_temporary_stream->stream.int1 = 0;
	x = fmt_advance();
	assert_type_character(x);
	prin1(x, fmt_temporary_stream);
	if (!colon && atsign)
		i = 0;
	else
		i = 2;
	for (;  i < fmt_temporary_string->string.fillp;  i++)
		WRITEC_STREAM(fmt_tempstr(i), fmt_stream);
}

static void
fmt_fix_float(bool colon, bool atsign)
{
	int w, d, k, overflowchar, padchar;
	double f;
	int sign;
	char buff[256], *b, buff1[256];
	int exp;
	int i, j;
	cl_object x;
	int n, m;

	b = buff1 + 1;

	fmt_not_colon(colon);
	fmt_max_param(5);
	fmt_set_param_positive(0, &w, "illegal width");
	fmt_set_param_positive(1, &d, "illegal number of digits");
	fmt_set_param(2, &k, INT, 0);
	fmt_set_param(3, &overflowchar, CHAR, -1);
	fmt_set_param(4, &padchar, CHAR, ' ');

	x = fmt_advance();
	if (FIXNUMP(x) ||
	    type_of(x) == t_bignum ||
	    type_of(x) == t_ratio)
		x = make_shortfloat(object_to_float(x));
	if (!REAL_TYPE(type_of(x))) {
		if (fmt_nparam > 1) fmt_nparam = 1;
		--fmt_index;
		fmt_decimal(colon, atsign);
		return;
	}
	if (type_of(x) == t_longfloat)
		n = 16;
	else
		n = 7;
	f = number_to_double(x);
	edit_double(n, f, &sign, buff, &exp);
	if (exp + k > 100 || exp + k < -100 || d > 100) {
		prin1(x, fmt_stream);
		return;
	}
	if (d >= 0)
		m = d + exp + k + 1;
	else if (w >= 0) {
		if (exp + k >= 0)
			m = w - 1;
		else
			m = w + exp + k - 2;
		if (sign < 0 || atsign)
			--m;
		if (m == 0)
			m = 1;
	} else
		m = n;
	if (m <= 0) {
		if (m == 0 && buff[0] >= '5') {
			exp++;
			n = m = 1;
			buff[0] = '1';
		} else
			n = m = 0;
	} else if (m < n) {
		n = m;
		edit_double(n, f, &sign, buff, &exp);
	}
	while (n >= 0)
		if (buff[n - 1] == '0')
			--n;
		else
			break;
	exp += k;
	j = 0;
	if (exp >= 0) {
		for (i = 0;  i <= exp;  i++)
			b[j++] = i < n ? buff[i] : '0';
		b[j++] = '.';
		if (d >= 0)
			for (m = i + d;  i < m;  i++)
				b[j++] = i < n ? buff[i] : '0';
		else
			for (;  i < n;  i++)
				b[j++] = buff[i];
	} else {
		b[j++] = '.';
		if (d >= 0) {
			for (i = 0;  i < (-exp) - 1 && i < d;  i++)
				b[j++] = '0';
			for (m = d - i, i = 0;  i < m;  i++)
				b[j++] = i < n ? buff[i] : '0';
		} else if (n > 0) {
			for (i = 0;  i < (-exp) - 1;  i++)
				b[j++] = '0';
			for (i = 0;  i < n;  i++)
				b[j++] = buff[i];
		}
	}
	b[j] = '\0';
	if (w >= 0) {
		if (sign < 0 || atsign)
			--w;
		if (j > w && overflowchar >= 0) {
			fmt_set_param(0, &w, INT, 0);
			for (i = 0;  i < w;  i++)
				WRITEC_STREAM(overflowchar, fmt_stream);
			return;
		}
		if (j < w && b[j-1] == '.') {
			b[j++] = '0';
			b[j] = '\0';
		}
		if (j < w && b[0] == '.') {
			*--b = '0';
			j++;
		}
		for (i = j;  i < w;  i++)
			WRITEC_STREAM(padchar, fmt_stream);
	} else {
		if (b[0] == '.') {
			*--b = '0';
			j++;
		}
		if (d < 0 && b[j-1] == '.') {
			b[j++] = '0';
			b[j] = '\0';
		}
	}
	if (sign < 0)
		WRITEC_STREAM('-', fmt_stream);
	else if (atsign)
		WRITEC_STREAM('+', fmt_stream);
	WRITESTR_STREAM(b, fmt_stream)
}

static int
fmt_exponent_length(int e)
{
	int i;

	if (e == 0)
		return(1);
	if (e < 0)
		e = -e;
	for (i = 0;  e > 0;  i++, e /= 10)
		;
	return(i);
}

static void
fmt_exponent1(int e)
{
	if (e == 0)
		return;
	fmt_exponent1(e/10);
	WRITEC_STREAM('0' + e%10, fmt_stream);
}

static void
fmt_exponent(int e)
{
	if (e == 0) {
		WRITEC_STREAM('0', fmt_stream);
		return;
	}
	if (e < 0)
		e = -e;
	fmt_exponent1(e);
}

static void
fmt_exponential_float(bool colon, bool atsign)
{
	int w, d, e, k, overflowchar, padchar, exponentchar;
	double f;
	int sign;
	char buff[256], *b, buff1[256];
	int exp;
	int i, j;
	cl_object x, y;
	int n, m;
	enum type t;

	b = buff1 + 1;

	fmt_not_colon(colon);
	fmt_max_param(7);
	fmt_set_param_positive(0, &w, "illegal width");
	fmt_set_param_positive(1, &d, "illegal number of digits");
	fmt_set_param_positive(2, &e, "illegal number of digits in exponent");
	fmt_set_param(3, &k, INT, 1);
	fmt_set_param(4, &overflowchar, CHAR, -1);
	fmt_set_param(5, &padchar, CHAR, ' ');
	fmt_set_param(6, &exponentchar, CHAR, -1);

	x = fmt_advance();
	if (FIXNUMP(x) ||
	    type_of(x) == t_bignum ||
	    type_of(x) == t_ratio)
		x = make_shortfloat(object_to_float(x));
	if (!REAL_TYPE(type_of(x))) {
		if (fmt_nparam > 1) fmt_nparam = 1;
		--fmt_index;
		fmt_decimal(colon, atsign);
		return;
	}
	if (type_of(x) == t_longfloat)
		n = 16;
	else
		n = 7;
	f = number_to_double(x);
	edit_double(n, f, &sign, buff, &exp);
	if (d >= 0) {
		if (k > 0) {
			if (!(k < d + 2))
				fmt_error("illegal scale factor");
			m = d + 1;
		} else {
			if (!(k > -d))
				fmt_error("illegal scale factor");
			m = d + k;
		}
	} else if (w >= 0) {
		if (k > 0)
			m = w - 1;
		else
			m = w + k - 1;
		if (sign < 0 || atsign)
			--m;
		if (e >= 0)
			m -= e + 2;
		else
			m -= fmt_exponent_length(e - k + 1) + 2;
	} else
		m = n;
	if (m <= 0) {
		if (m == 0 && buff[0] >= '5') {
			exp++;
			n = m = 1;
			buff[0] = '1';
		} else
			n = m = 0;
	} else if (m < n) {
		n = m;
		edit_double(n, f, &sign, buff, &exp);
	}
	while (n >= 0)
		if (buff[n - 1] == '0')
			--n;
		else
			break;
	exp = exp - k + 1;
	j = 0;
	if (k > 0) {
		for (i = 0;  i < k;  i++)
			b[j++] = i < n ? buff[i] : '0';
		b[j++] = '.';
		if (d >= 0)
			for (m = i + (d - k + 1);  i < m;  i++)
				b[j++] = i < n ? buff[i] : '0';
		else
			for (;  i < n;  i++)
				b[j++] = buff[i];
	} else {
		b[j++] = '.';
		if (d >= 0) {
			for (i = 0;  i < -k && i < d;  i++)
				b[j++] = '0';
			for (m = d - i, i = 0;  i < m;  i++)
				b[j++] = i < n ? buff[i] : '0';
		} else if (n > 0) {
			for (i = 0;  i < -k;  i++)
				b[j++] = '0';
			for (i = 0;  i < n;  i++)
				b[j++] = buff[i];
		}
	}
	b[j] = '\0';
	if (w >= 0) {
		if (sign < 0 || atsign)
			--w;
		i = fmt_exponent_length(exp);
		if (e >= 0) {
			if (i > e) {
				if (overflowchar >= 0)
					goto OVER;
				else
					e = i;
			}
			w -= e + 2;
		} else
			w -= i + 2;
		if (j > w && overflowchar >= 0)
			goto OVER;
		if (j < w && b[j-1] == '.') {
			b[j++] = '0';
			b[j] = '\0';
		}
		if (j < w && b[0] == '.') {
			*--b = '0';
			j++;
		}
		for (i = j;  i < w;  i++)
			WRITEC_STREAM(padchar, fmt_stream);
	} else {
		if (b[j-1] == '.') {
			b[j++] = '0';
			b[j] = '\0';
		}
		if (d < 0 && b[0] == '.') {
			*--b = '0';
			j++;
		}
	}
	if (sign < 0)
		WRITEC_STREAM('-', fmt_stream);
	else if (atsign)
		WRITEC_STREAM('+', fmt_stream);
	WRITESTR_STREAM(b, fmt_stream)
	y = symbol_value(@'*read-default-float-format*');
	if (exponentchar < 0) {
		if (y == @'long-float' || y == @'double-float')
			t = t_longfloat;
		else
			t = t_shortfloat;
		if (type_of(x) == t)
			exponentchar = 'E';
		else if (type_of(x) == t_shortfloat)
			exponentchar = 'S';
		else
			exponentchar = 'L';
	}
	WRITEC_STREAM(exponentchar, fmt_stream);
	if (exp < 0)
		WRITEC_STREAM('-', fmt_stream);
	else
		WRITEC_STREAM('+', fmt_stream);
	if (e >= 0)
		for (i = e - fmt_exponent_length(exp);  i > 0;  --i)
			WRITEC_STREAM('0', fmt_stream);
	fmt_exponent(exp);
	return;

OVER:
	fmt_set_param(0, &w, INT, -1);
	for (i = 0;  i < w;  i++)
		WRITEC_STREAM(overflowchar, fmt_stream);
	return;
}

static void
fmt_general_float(bool colon, bool atsign)
{
	int w, d, e, k, overflowchar, padchar, exponentchar;
	int sign, exp;
	char buff[256];
	cl_object x;
	int n, ee, ww, q, dd;

	fmt_not_colon(colon);
	fmt_max_param(7);
	fmt_set_param_positive(0, &w, "illegal width");
	fmt_set_param_positive(1, &d, "illegal number of digits");
	fmt_set_param_positive(2, &e, "illegal number of digits in exponent");
	fmt_set_param(3, &k, INT, 1);
	fmt_set_param(4, &overflowchar, CHAR, -1);
	fmt_set_param(5, &padchar, CHAR, ' ');
	fmt_set_param(6, &exponentchar, CHAR, -1);

	x = fmt_advance();
	if (!REAL_TYPE(type_of(x))) {
		if (fmt_nparam > 1) fmt_nparam = 1;
		--fmt_index;
		fmt_decimal(colon, atsign);
		return;
	}
	if (type_of(x) == t_longfloat)
		q = 16;
	else
		q = 7;
	edit_double(q, number_to_double(x), &sign, buff, &exp);
	n = exp + 1;
	while (q >= 0)
		if (buff[q - 1] == '0')
			--q;
		else
			break;
	if (e >= 0)
		ee = e + 2;
	else
		ee = 4;
	ww = w - ee;
	if (d < 0) {
		d = n < 7 ? n : 7;
		d = q > d ? q : d;
	}
	dd = d - n;
	if (0 <= dd && dd <= d) {
		fmt_nparam = 5;
		fmt_param[0].fmt_param_value = ww;
		fmt_param[1].fmt_param_value = dd;
		fmt_param[1].fmt_param_type = INT;
		fmt_param[2].fmt_param_type = NONE;
		fmt_param[3] = fmt_param[4];
		fmt_param[4] = fmt_param[5];
		--fmt_index;
		fmt_fix_float(colon, atsign);
		if (w >= 0)
			while (ww++ < w)
				WRITEC_STREAM(padchar, fmt_stream);
		return;
	}
	fmt_param[1].fmt_param_value = d;
	fmt_param[1].fmt_param_type = INT;
	--fmt_index;
	fmt_exponential_float(colon, atsign);
}

static void
fmt_dollars_float(bool colon, bool atsign)
{
	int d, n, w, padchar;
	double f;
	int sign;
	char buff[256];
	int exp;
	int q, i;
	cl_object x;

	fmt_max_param(4);
	fmt_set_param(0, &d, INT, 2);
	if (d < 0)
		fmt_error("illegal number of digits");
	fmt_set_param(1, &n, INT, 1);
	if (n < 0)
		fmt_error("illegal number of digits");
	fmt_set_param(2, &w, INT, 0);
	if (w < 0)
		fmt_error("illegal width");
	fmt_set_param(3, &padchar, CHAR, ' ');
	x = fmt_advance();
	if (!REAL_TYPE(type_of(x))) {
		if (fmt_nparam < 3)
			fmt_nparam = 0;
		else {
			fmt_nparam = 1;
			fmt_param[0] = fmt_param[2];
		}
		--fmt_index;
		fmt_decimal(colon, atsign);
		return;
	}
	q = 7;
	if (type_of(x) == t_longfloat)
		q = 16;
	f = number_to_double(x);
	edit_double(q, f, &sign, buff, &exp);
	if ((q = exp + d + 1) > 0)
		edit_double(q, f, &sign, buff, &exp);
	exp++;
	if (w > 100 || exp > 100 || exp < -100) {
		fmt_nparam = 6;
		fmt_param[0] = fmt_param[2];
		fmt_param[1].fmt_param_value = d + n - 1;
		fmt_param[1].fmt_param_type = INT;
		fmt_param[2].fmt_param_type =
		fmt_param[3].fmt_param_type =
		fmt_param[4].fmt_param_type = NONE;
		fmt_param[5] = fmt_param[3];
		--fmt_index;
		fmt_exponential_float(colon, atsign);
	}
	if (exp > n)
		n = exp;
	if (sign < 0 || atsign)
		--w;
	if (colon) {
		if (sign < 0)
			WRITEC_STREAM('-', fmt_stream);
		else if (atsign)
			WRITEC_STREAM('+', fmt_stream);
		while (--w > n + d)
			WRITEC_STREAM(padchar, fmt_stream);
	} else {
		while (--w > n + d)
			WRITEC_STREAM(padchar, fmt_stream);
		if (sign < 0)
			WRITEC_STREAM('-', fmt_stream);
		else if (atsign)
			WRITEC_STREAM('+', fmt_stream);
	}
	for (i = n - exp;  i > 0;  --i)
		WRITEC_STREAM('0', fmt_stream);
	for (i = 0;  i < exp;  i++)
		WRITEC_STREAM((i < q ? buff[i] : '0'), fmt_stream);
	WRITEC_STREAM('.', fmt_stream);
	for (d += i;  i < d;  i++)
		WRITEC_STREAM((i < q ? buff[i] : '0'), fmt_stream);
}

static void
fmt_percent(bool colon, bool atsign)
{
	int n, i;

	fmt_max_param(1);
	fmt_set_param(0, &n, INT, 1);
	fmt_not_colon(colon);
	fmt_not_atsign(atsign);
	while (n-- > 0) {
		WRITEC_STREAM('\n', fmt_stream);
		if (n == 0)
			for (i = fmt_indents;  i > 0;  --i)
				WRITEC_STREAM(' ', fmt_stream);
	}
}

static void
fmt_ampersand(bool colon, bool atsign)
{
	int n;

	fmt_max_param(1);
	fmt_set_param(0, &n, INT, 1);
	fmt_not_colon(colon);
	fmt_not_atsign(atsign);
	if (n == 0)
		return;
	if (FILE_COLUMN(fmt_stream) != 0)
		WRITEC_STREAM('\n', fmt_stream);
	while (--n > 0)
		WRITEC_STREAM('\n', fmt_stream);
	fmt_indents = 0;
}

static void
fmt_bar(bool colon, bool atsign)
{
	int n;

	fmt_max_param(1);
	fmt_set_param(0, &n, INT, 1);
	fmt_not_colon(colon);
	fmt_not_atsign(atsign);
	while (n-- > 0)
		WRITEC_STREAM('\f', fmt_stream);
}

static void
fmt_tilde(bool colon, bool atsign)
{
	int n;

	fmt_max_param(1);
	fmt_set_param(0, &n, INT, 1);
	fmt_not_colon(colon);
	fmt_not_atsign(atsign);
	while (n-- > 0)
		WRITEC_STREAM('~', fmt_stream);
}

static void
fmt_newline(bool colon, bool atsign)
{
	fmt_max_param(0);
	fmt_not_colon_atsign(colon, atsign);
	if (atsign)
		WRITEC_STREAM('\n', fmt_stream);
	while (ctl_index < ctl_end && isspace(ctl_string[ctl_index])) {
		if (colon)
			WRITEC_STREAM(ctl_string[ctl_index], fmt_stream);
		ctl_index++;
	}
}

static void
fmt_tabulate(bool colon, bool atsign)
{
	int colnum, colinc;
	int c, i;
	
	fmt_max_param(2);
	fmt_not_colon(colon);
	fmt_set_param(0, &colnum, INT, 1);
	fmt_set_param(1, &colinc, INT, 1);
	if (!atsign) {
		c = FILE_COLUMN(fmt_stream);
		if (c < 0) {
			WRITESTR_STREAM("  ", fmt_stream)
			return;
		}
		if (c > colnum && colinc <= 0)
			return;
		while (c > colnum)
			colnum += colinc;
		for (i = colnum - c;  i > 0;  --i)
			WRITEC_STREAM(' ', fmt_stream);
	} else {
		for (i = colnum;  i > 0;  --i)
			WRITEC_STREAM(' ', fmt_stream);
		c = FILE_COLUMN(fmt_stream);
		if (c < 0 || colinc <= 0)
			return;
		colnum = 0;
		while (c > colnum)
			colnum += colinc;
		for (i = colnum - c;  i > 0;  --i)
			WRITEC_STREAM(' ', fmt_stream);
	}
}

static void
fmt_asterisk(bool colon, bool atsign)
{
	int n;

	fmt_max_param(1);
	fmt_not_colon_atsign(colon, atsign);
	if (atsign) {
		fmt_set_param(0, &n, INT, 0);
		if (n < 0 || n >= fmt_end)
			fmt_error("can't goto");
		fmt_index = n;
	} else if (colon) {
		fmt_set_param(0, &n, INT, 1);
		if (n > fmt_index)
			fmt_error("can't back up");
		fmt_index -= n;
	} else {
		fmt_set_param(0, &n, INT, 1);
		while (n-- > 0)
			fmt_advance();
	}
}	

static void
fmt_indirection(bool colon, bool atsign)
{
	cl_object s, l;
	fmt_old;
	jmp_buf fmt_jmp_buf0;
	int up_colon;

	fmt_max_param(0);
	fmt_not_colon(colon);
	s = fmt_advance();
	if (type_of(s) != t_string)
		fmt_error("control string expected");
	if (atsign) {
		fmt_save;
		fmt_jmp_buf = (int *)fmt_jmp_buf0;
		fmt_string = s;
		if ((up_colon = ecls_setjmp(fmt_jmp_buf))) {
			if (--up_colon)
				fmt_error("illegal ~:^");
		} else
			format(fmt_stream, 0, s->string.fillp);
		fmt_restore1;
	} else {
		l = fmt_advance();
		fmt_save;
		fmt_base = alloca(length(l) * sizeof(cl_object));
		fmt_index = 0;
		for (fmt_end = 0;  !endp(l);  fmt_end++, l = CDR(l))
			fmt_base[fmt_end] = CAR(l);
		fmt_jmp_buf = (int *)fmt_jmp_buf0;
		fmt_string = s;
		if ((up_colon = ecls_setjmp(fmt_jmp_buf))) {
			if (--up_colon)
				fmt_error("illegal ~:^");
		} else
			format(fmt_stream, 0, s->string.fillp);
		fmt_restore;
	}
}

static void
fmt_case(bool colon, bool atsign)
{
	cl_object x;
	cl_index i;
	int j;
	fmt_old;
	jmp_buf fmt_jmp_buf0;
	int up_colon;
	bool b;

	x = make_string_output_stream(64);
	i = ctl_index;
	j = fmt_skip();
	if (ctl_string[--j] != ')' || ctl_string[--j] != '~')
		fmt_error("~) expected");
	fmt_save;
	fmt_jmp_buf = (int *)fmt_jmp_buf0;
	if ((up_colon = ecls_setjmp(fmt_jmp_buf)))
		;
	else
		format(x, ctl_origin + i, j - i);
	fmt_restore1;
	x = x->stream.object0;
	if (!colon && !atsign)
		for (i = 0;  i < x->string.fillp;  i++) {
			if (isupper(j = x->string.self[i]))
				j = tolower(j);
			WRITEC_STREAM(j, fmt_stream);
		}
	else if (colon && !atsign)
		for (b = TRUE, i = 0;  i < x->string.fillp;  i++) {
			if (islower(j = x->string.self[i])) {
				if (b)
					j = toupper(j);
				b = FALSE;
			} else if (isupper(j)) {
				if (!b)
					j = tolower(j);
				b = FALSE;
			} else if (!isdigit(j))
				b = TRUE;
			WRITEC_STREAM(j, fmt_stream);
		}
	else if (!colon && atsign)
		for (b = TRUE, i = 0;  i < x->string.fillp;  i++) {
			if (islower(j = x->string.self[i])) {
				if (b)
					j = toupper(j);
				b = FALSE;
			} else if (isupper(j)) {
				if (!b)
					j = tolower(j);
				b = FALSE;
			}
			WRITEC_STREAM(j, fmt_stream);
		}
	else
		for (i = 0;  i < x->string.fillp;  i++) {
			if (islower(j = x->string.self[i]))
				j = toupper(j);
			WRITEC_STREAM(j, fmt_stream);
		}
	if (up_colon)
		ecls_longjmp(fmt_jmp_buf, up_colon);
}

static void
fmt_conditional(bool colon, bool atsign)
{
	int i, j, k;
	cl_object x;
	int n;
	bool done;
	fmt_old;

	fmt_not_colon_atsign(colon, atsign);
	if (colon) {
		fmt_max_param(0);
		i = ctl_index;
		j = fmt_skip();
		if (ctl_string[--j] != ';' || ctl_string[--j] != '~')
			fmt_error("~; expected");
		k = fmt_skip();
		if (ctl_string[--k] != ']' || ctl_string[--k] != '~')
			fmt_error("~] expected");
		if (Null(fmt_advance())) {
			fmt_save;
			format(fmt_stream, ctl_origin + i, j - i);
			fmt_restore1;
		} else {
			fmt_save;
			format(fmt_stream, ctl_origin + j + 2, k - (j + 2));
			fmt_restore1;
		}
	} else if (atsign) {
		i = ctl_index;
		j = fmt_skip();
		if (ctl_string[--j] != ']' || ctl_string[--j] != '~')
			fmt_error("~] expected");
		if (Null(fmt_advance()))
			;
		else {
			--fmt_index;
			fmt_save;
			format(fmt_stream, ctl_origin + i, j - i);
			fmt_restore1;
		}
	} else {
		fmt_max_param(1);
		if (fmt_nparam == 0) {
			x = fmt_advance();
			if (!FIXNUMP(x))
				fmt_error("illegal argument for conditional");
			n = fix(x);
		} else
			fmt_set_param(0, &n, INT, 0);
		i = ctl_index;
		for (done = FALSE;;  --n) {
			j = fmt_skip();
			for (k = j;  ctl_string[--k] != '~';)
				;
			if (n == 0) {
				fmt_save;
				format(fmt_stream, ctl_origin + i, k - i);
				fmt_restore1;
				done = TRUE;
			}
			i = j;
			if (ctl_string[--j] == ']') {
				if (ctl_string[--j] != '~')
					fmt_error("~] expected");
				return;
			}
			if (ctl_string[j] == ';') {
				if (ctl_string[--j] == '~')
					continue;
				if (ctl_string[j] == ':')
					goto ELSE;
			}
			fmt_error("~; or ~] expected");
		}
	ELSE:
		if (ctl_string[--j] != '~')
			fmt_error("~:; expected");
		j = fmt_skip();
		if (ctl_string[--j] != ']' || ctl_string[--j] != '~')
			fmt_error("~] expected");
		if (!done) {
			fmt_save;
			format(fmt_stream, ctl_origin + i, j - i);
			fmt_restore1;
		}
	}
}

static void
fmt_iteration(bool colon, bool atsign)
{
	int n, i, o;
	volatile int j;
	bool colon_close = FALSE;
	cl_object l;
	fmt_old;
	jmp_buf fmt_jmp_buf0;
	int up_colon;

	fmt_max_param(1);
	fmt_set_param(0, &n, INT, 1000000);
	i = ctl_index;
	j = fmt_skip();
	if (ctl_string[--j] != '}')
		fmt_error("~} expected");
	if (ctl_string[--j] == ':') {
		colon_close = TRUE;
		--j;
	}
	if (ctl_string[j] != '~')
		fmt_error("syntax error");
	o = ctl_origin;
	if (!colon && !atsign) {
		l = fmt_advance();
		fmt_save;
		fmt_base = (cl_object *)alloca(length(l) * sizeof(cl_object));
		fmt_index = 0;
		for (fmt_end = 0;  !endp(l);  fmt_end++, l = CDR(l))
			fmt_base[fmt_end] = CAR(l);
		fmt_jmp_buf = (int *)fmt_jmp_buf0;
		if (colon_close)
			goto L1;
		while (fmt_index < fmt_end) {
		L1:
			if (n-- <= 0)
				break;
			if ((up_colon = ecls_setjmp(fmt_jmp_buf))) {
				if (--up_colon)
					fmt_error("illegal ~:^");
				break;
			}
			format(fmt_stream, o + i, j - i);
		}
		fmt_restore;
	} else if (colon && !atsign) {
	  	int fl = 0;
		volatile cl_object l0;
		l0 = fmt_advance();
		fmt_save;
		for (l = l0; !endp(l); l = CDR(l))
		  fl += length(CAR(l));
		fmt_base = (cl_object *)alloca(fl * sizeof(cl_object));
		fmt_jmp_buf = (int *)fmt_jmp_buf0;
		if (colon_close)
			goto L2;
		while (!endp(l0)) {
		L2:
			if (n-- <= 0)
				break;
			l = CAR(l0);
			l0 = CDR(l0);
			fmt_index = 0;
			for (fmt_end = 0; !endp(l); fmt_end++, l = CDR(l))
				fmt_base[fmt_end] = CAR(l);
			if ((up_colon = ecls_setjmp(fmt_jmp_buf))) {
				if (--up_colon)
					break;
				else
					continue;
			}
			format(fmt_stream, o + i, j - i);
		}
		fmt_restore;
	} else if (!colon && atsign) {
		fmt_save;
		fmt_jmp_buf = (int *)fmt_jmp_buf0;
		if (colon_close)
			goto L3;
		while (fmt_index < fmt_end) {
		L3:
			if (n-- <= 0)
				break;
			if ((up_colon = ecls_setjmp(fmt_jmp_buf))) {
				if (--up_colon)
					fmt_error("illegal ~:^");
				break;
			}
			format(fmt_stream, o + i, j - i);
		}
		fmt_restore1;
	} else if (colon && atsign) {
		if (colon_close)
			goto L4;
		while (fmt_index < fmt_end) {
		L4:
			if (n-- <= 0)
				break;
			l = fmt_advance();
			fmt_save;
			fmt_base = (cl_object *)alloca(length(l) * sizeof(cl_object));
			fmt_index = 0;
			for (fmt_end = 0; !endp(l); fmt_end++, l = CDR(l))
				fmt_base[fmt_end] = CAR(l);
			fmt_jmp_buf = (int *)fmt_jmp_buf0;
			if ((up_colon = ecls_setjmp(fmt_jmp_buf))) {
				fmt_restore;
				if (--up_colon)
					break;
				else
					continue;
			}
			format(fmt_stream, o + i, j - i);
			fmt_restore;
		}
	}
}

static void
fmt_justification(volatile bool colon, bool atsign)
{
	int mincol, colinc, minpad, padchar;
	cl_object fields[16];
	fmt_old;
	jmp_buf fmt_jmp_buf0;
	volatile int i, j, k, l, m, n, j0, l0;
	int up_colon;
	volatile int special = 0;
	volatile int spare_spaces, line_length;

	fmt_max_param(4);
	fmt_set_param(0, &mincol, INT, 0);
	fmt_set_param(1, &colinc, INT, 1);
	fmt_set_param(2, &minpad, INT, 0);
	fmt_set_param(3, &padchar, CHAR, ' ');

	n = 0;
	for (;;) {
		if (n >= 16)
			fmt_error("too many fields");
		i = ctl_index;
		j0 = j = fmt_skip();
		while (ctl_string[--j] != '~')
			;
		fields[n] = make_string_output_stream(64);
		fmt_save;
		fmt_jmp_buf = (int *)fmt_jmp_buf0;
		if ((up_colon = ecls_setjmp(fmt_jmp_buf))) {
			--n;
			if (--up_colon)
				fmt_error("illegal ~:^");
			fmt_restore1;
			while (ctl_string[--j0] != '>')
				j0 = fmt_skip();
			if (ctl_string[--j0] != '~')
				fmt_error("~> expected");
			break;
		}
		format(fields[n++], ctl_origin + i, j - i);
		fmt_restore1;
		if (ctl_string[--j0] == '>') {
			if (ctl_string[--j0] != '~')
				fmt_error("~> expected");
			break;
		} else if (ctl_string[j0] != ';')
			fmt_error("~; expected");
		else if (ctl_string[--j0] == ':') {
			if (n != 1)
				fmt_error("illegal ~:;");
			special = 1;
			for (j = j0;  ctl_string[j] != '~';  --j)
				;
			fmt_save;
			format(fmt_stream, ctl_origin + j, j0 - j + 2);
			fmt_restore1;
			spare_spaces = fmt_spare_spaces;
			line_length = fmt_line_length;
		} else if (ctl_string[j0] != '~')
			fmt_error("~; expected");
	}
	for (i = special, l = 0;  i < n;  i++)
		l += fields[i]->stream.object0->string.fillp;
	m = n - 1 - special;
	if (m <= 0 && !colon && !atsign) {
		m = 0;
		colon = TRUE;
	}
	if (colon)
		m++;
	if (atsign)
		m++;
	l0 = l;
	l += minpad * m;
	for (k = 0;  mincol + k * colinc < l;  k++)
		;
	l = mincol + k * colinc;
	if (special != 0 &&
	    FILE_COLUMN(fmt_stream) + l + spare_spaces >= line_length)
		princ(fields[0]->stream.object0, fmt_stream);
	l -= l0;
	for (i = special;  i < n;  i++) {
		if (i > 0 || colon)
			for (j = l / m, l -= j, --m;  j > 0;  --j)
				WRITEC_STREAM(padchar, fmt_stream);
		princ(fields[i]->stream.object0, fmt_stream);
	}
	if (atsign)
		for (j = l;  j > 0;  --j)
			WRITEC_STREAM(padchar, fmt_stream);
}

static void
fmt_up_and_out(bool colon, bool atsign)
{
	int i, j, k;

	fmt_max_param(3);
	fmt_not_atsign(atsign);
	if (fmt_nparam == 0) {
		if (fmt_index >= fmt_end)
			ecls_longjmp(fmt_jmp_buf, ++colon);
	} else if (fmt_nparam == 1) {
		fmt_set_param(0, &i, INT, 0);
		if (i == 0)
			ecls_longjmp(fmt_jmp_buf, ++colon);
	} else if (fmt_nparam == 2) {
		fmt_set_param(0, &i, INT, 0);
		fmt_set_param(1, &j, INT, 0);
		if (i == j)
			ecls_longjmp(fmt_jmp_buf, ++colon);
	} else {
		fmt_set_param(0, &i, INT, 0);
		fmt_set_param(1, &j, INT, 0);
		fmt_set_param(2, &k, INT, 0);
		if (i <= j && j <= k)
			ecls_longjmp(fmt_jmp_buf, ++colon);
	}
}

static void
fmt_semicolon(bool colon, bool atsign)
{
	fmt_not_atsign(atsign);
	if (!colon)
		fmt_error("~:; expected");
	fmt_max_param(2);
	fmt_set_param(0, &fmt_spare_spaces, INT, 0);
	fmt_set_param(1, &fmt_line_length, INT, 72);
}

@(defun format (strm string &rest args)
	cl_object x = OBJNULL;
	jmp_buf fmt_jmp_buf0;
	bool colon;
	fmt_old;
@
	if (Null(strm)) {
		strm = make_string_output_stream(64);
		x = strm->stream.object0;
	} else if (strm == Ct)
		strm = symbol_value(@'*standard-output*');
	else if (type_of(strm) == t_string) {
		x = strm;
		if (!x->string.hasfillp)
		  FEerror("The string ~S doesn't have a fill-pointer.", 1, x);
		strm = make_string_output_stream(0);
		strm->stream.object0 = x;
		x = OBJNULL;
	}
	fmt_save;
RETRY:	if (type_of(strm) == t_stream) {
	  if (strm->stream.mode == (short)smm_synonym) {
 		strm = symbol_value(strm->stream.object0);
		goto RETRY;
	      }
	  else
	    fmt_writec = writec_stream;
	} else
#ifdef CLOS
	  if (type_of(strm) == t_instance)
	    fmt_writec = interactive_writec_stream;
	else
#endif CLOS
	  FEerror("~S is not a stream.", 1, strm);
	assert_type_string(string);
	if (frs_push(FRS_PROTECT, Cnil)) {
		frs_pop();
		fmt_restore;
		unwind(nlj_fr, nlj_tag);
	}
	fmt_base = (cl_object *)args;
	fmt_index = 0;
	fmt_end = narg - 2;
	fmt_jmp_buf = (int *)fmt_jmp_buf0;
	if (symbol_value(@'si::*indent-formatted-output*') != Cnil)
		fmt_indents = FILE_COLUMN(strm);
	else
		fmt_indents = 0;
	fmt_string = string;
	if ((colon = ecls_setjmp(fmt_jmp_buf))) {
		if (--colon)
			fmt_error("illegal ~:^");
	} else {
		format(strm, 0, string->string.fillp);
		FLUSH_STREAM(strm);
	}
	frs_pop();
	fmt_restore;
	@(return (x == OBJNULL? Cnil : x))
@)

static void
format(cl_object fmt_stream0, int ctl_origin0, int ctl_end0)
{
	int c, i, n;
	bool colon, atsign;
	cl_object x;

	fmt_stream = fmt_stream0;
	ctl_origin = ctl_origin0;
	ctl_index = 0;
	ctl_end = ctl_end0;

LOOP:
	if (ctl_index >= ctl_end)
		return;
	if ((c = ctl_advance()) != '~') {
		WRITEC_STREAM(c, fmt_stream);
		goto LOOP;
	}
	n = 0;
	for (;;) {
		switch (c = ctl_advance()) {
		case ',':
			fmt_param[n].fmt_param_type = NONE;
			break;

		case '0':  case '1':  case '2':  case '3':  case '4':
		case '5':  case '6':  case '7':  case '8':  case '9':
		DIGIT:
			i = 0;
			do {
				i = i*10 + (c - '0');
				c = ctl_advance();
			} while (isdigit(c));
			fmt_param[n].fmt_param_type = INT;
			fmt_param[n].fmt_param_value = i;
			break;

		case '+':
			c = ctl_advance();
			if (!isdigit(c))
				fmt_error("digit expected");
			goto DIGIT;

		case '-':
			c = ctl_advance();
			if (!isdigit(c))
				fmt_error("digit expected");
			i = 0;
			do {
				i = i*10 + (c - '0');
				c = ctl_advance();
			} while (isdigit(c));
			fmt_param[n].fmt_param_type = INT;
			fmt_param[n].fmt_param_value = -i;
			break;

		case '\'':
			fmt_param[n].fmt_param_type = CHAR;
			fmt_param[n].fmt_param_value = ctl_advance();
			c = ctl_advance();
			break;

		case 'v':  case 'V':
			x = fmt_advance();
			if (FIXNUMP(x)) {
				fmt_param[n].fmt_param_type = INT;
				fmt_param[n].fmt_param_value = fix(x);
			} else if (type_of(x) == t_character) {
				fmt_param[n].fmt_param_type = CHAR;
				fmt_param[n].fmt_param_value = CHAR_CODE(x);
			} else
				fmt_error("illegal V parameter");
			c = ctl_advance();
			break;

		case '#':
			fmt_param[n].fmt_param_type = INT;
			fmt_param[n].fmt_param_value = fmt_end - fmt_index;
			c = ctl_advance();
			break;

		default:
			if (n > 0)
				fmt_error("illegal ,");
			else
				goto DIRECTIVE;
		}
		n++;
		if (c != ',')
			break;
	}

DIRECTIVE:
	colon = atsign = FALSE;
	if (c == ':') {
		colon = TRUE;
		c = ctl_advance();
	}
	if (c == '@@') {
		atsign = TRUE;
		c = ctl_advance();
	}
	fmt_nparam = n;
	switch (c) {
	case 'a':  case 'A':
		fmt_ascii(colon, atsign);
		break;

	case 's':  case 'S':
		fmt_S_expression(colon, atsign);
		break;

	case 'd':  case 'D':
		fmt_decimal(colon, atsign);
		break;

	case 'b':  case 'B':
		fmt_binary(colon, atsign);
		break;

	case 'o':  case 'O':
		fmt_octal(colon, atsign);
		break;

	case 'x':  case 'X':
		fmt_hexadecimal(colon, atsign);
		break;

	case 'r':  case 'R':
		fmt_radix(colon, atsign);
		break;

	case 'p':  case 'P':
		fmt_plural(colon, atsign);
		break;

	case 'c':  case 'C':
		fmt_character(colon, atsign);
		break;

	case 'f':  case 'F':
		fmt_fix_float(colon, atsign);
		break;

	case 'e':  case 'E':
		fmt_exponential_float(colon, atsign);
		break;

	case 'g':  case 'G':
		fmt_general_float(colon, atsign);
		break;

	case '$':
		fmt_dollars_float(colon, atsign);
		break;

	case '%':
		fmt_percent(colon, atsign);
		break;

	case '&':
		fmt_ampersand(colon, atsign);
		break;

	case '|':
		fmt_bar(colon, atsign);
		break;

	case '~':
		fmt_tilde(colon, atsign);
		break;

	case '\n':
	case '\r':
		fmt_newline(colon, atsign);
		break;

	case 't':  case 'T':
		fmt_tabulate(colon, atsign);
		break;

	case '*':
		fmt_asterisk(colon, atsign);
		break;

	case '?':
		fmt_indirection(colon, atsign);
		break;

	case '(':
		fmt_case(colon, atsign);
		break;

	case '[':
		fmt_conditional(colon, atsign);
		break;

	case '{':
		fmt_iteration(colon, atsign);
		break;

	case '<':
		fmt_justification(colon, atsign);
		break;

	case '^':
		fmt_up_and_out(colon, atsign);
		break;

	case ';':
		fmt_semicolon(colon, atsign);
		break;

	default:
		fmt_error("illegal directive");
	}
	goto LOOP;
}

void
init_format(void)
{
	fmt_temporary_stream = make_string_output_stream(64);
	register_root(&fmt_temporary_stream);
	fmt_temporary_string = fmt_temporary_stream->stream.object0;

	SYM_VAL(@'si::*indent-formatted-output*') = Cnil;
}
