/*
    format.c -- Format.
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

#include "ecl.h"
#include <ctype.h>
#include "internal.h"

#define FMT_MAX_PARAM	8
typedef struct format_stack_struct {
  cl_object	stream;
  cl_object	string;
  cl_object	aux_stream;
  cl_object	aux_string;
  cl_index	ctl_index, ctl_end;
  const char	*ctl_str;
  cl_index	base, index, end;
  jmp_buf	*jmp_buf;
  cl_index	indents;
  cl_index	spare_spaces;
  cl_index	line_length;
  struct { int type, value; } param[FMT_MAX_PARAM];
  int		nparam;
} *format_stack;

static cl_object fmt_aux_stream;

/******************* COMMON ***************************/

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

static void format(format_stack, const char *s, cl_index);

static cl_object
get_aux_stream(void)
{
	cl_object stream;

	start_critical_section();
	if (fmt_aux_stream == Cnil)
		stream = make_string_output_stream(64);
	else {
		stream = fmt_aux_stream;
		fmt_aux_stream = Cnil;
	}
	end_critical_section();
	return stream;
}	

static void
fmt_error(format_stack fmt, const char *s)
{
	FEerror("Format error: ~A.~%~V@@TV~%\"~A\"~%",
		3, make_constant_string(s),
		MAKE_FIXNUM(&fmt->ctl_str[fmt->ctl_index] - (char *)fmt->string->string.self),
		fmt->string);
}

static int
tempstr(format_stack fmt, int s)
{
	return(fmt->aux_string->string.self[s]);
}

static int
ctl_advance(format_stack fmt)
{
	if (fmt->ctl_index >= fmt->ctl_end)
		fmt_error(fmt, "unexpected end of control string");
	return(fmt->ctl_str[fmt->ctl_index++]);
}

static cl_object
fmt_advance(format_stack fmt)
{
	if (fmt->index >= fmt->end)
		fmt_error(fmt, "arguments exhausted");
	return(cl_stack[fmt->index++]);
}

static void
fmt_push_list(format_stack fmt, cl_object l)
{
	for (;  !endp(l);  l = CDR(l))
		cl_stack_push(CAR(l));
}

static int
fmt_skip(format_stack fmt)
{
	int c, level = 0;
	
LOOP:
	if (ctl_advance(fmt) != '~')
		goto LOOP;
	for (;;)
		switch (c = ctl_advance(fmt)) {
		case '\'':
			ctl_advance(fmt);

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
			return(fmt->ctl_index);
		else
			--level;
		break;

	case ';':
		if (level == 0)
			return(fmt->ctl_index);
		break;
	}
	goto LOOP;
}

static void
ensure_param(format_stack fmt, int n)
{
	if (fmt->nparam > n)
		fmt_error(fmt, "too many parameters");
	while (n-- > fmt->nparam)
		fmt->param[n].type = NONE;
}

static void
fmt_not_colon(format_stack fmt, bool colon)
{
	if (colon)
		fmt_error(fmt, "illegal :");
}

static void
fmt_not_atsign(format_stack fmt, bool atsign)
{
	if (atsign)
		fmt_error(fmt, "illegal @@");
}

static void
fmt_not_colon_atsign(format_stack fmt, bool colon, bool atsign)
{
	if (colon && atsign)
		fmt_error(fmt, "illegal :@@");
}

static int
set_param(format_stack fmt, int i, int t, int v)
{
	if (i >= fmt->nparam || fmt->param[i].type == NONE)
		return v;
	else if (fmt->param[i].type != t)
		fmt_error(fmt, "illegal parameter type");
	return fmt->param[i].value;
}	

static int
set_param_positive(format_stack fmt, int i, const char *message)
{
	if (i >= fmt->nparam || fmt->param[i].type == NONE)
		return -1;
	else if (fmt->param[i].type != INT)
		fmt_error(fmt, "illegal parameter type");
	else {
		int p = fmt->param[i].value;
		if (p < 0) fmt_error(fmt, message);
		return p;
	}
}	

static void
fmt_copy(format_stack fmt_copy, format_stack fmt)
{
	*fmt_copy = *fmt;
}

static void
fmt_copy1(format_stack fmt_copy, format_stack fmt)
{
	fmt_copy->stream = fmt->stream;
	fmt_copy->ctl_str = fmt->ctl_str;
	fmt_copy->ctl_index = fmt->ctl_index;
	fmt_copy->ctl_end = fmt->ctl_end;
	fmt_copy->jmp_buf = fmt->jmp_buf;
	fmt_copy->indents = fmt->indents;
	fmt_copy->string = fmt->string;
}

static void
fmt_ascii(format_stack fmt, bool colon, bool atsign)
{
	int mincol, colinc, minpad, padchar;
	cl_object x;
	int l, i;

	ensure_param(fmt, 4);
	mincol = set_param(fmt, 0, INT, 0);
	colinc = set_param(fmt, 1, INT, 1);
	minpad = set_param(fmt, 2, INT, 0);
	padchar = set_param(fmt, 3, CHAR, ' ');

	fmt->aux_string->string.fillp = 0;
	fmt->aux_stream->stream.int0 = file_column(fmt->stream);
	fmt->aux_stream->stream.int1 = file_column(fmt->stream);
	x = fmt_advance(fmt);
	if (colon && Null(x))
		writestr_stream("()", fmt->aux_stream);
	else if (mincol == 0 && minpad == 0) {
		princ(x, fmt->stream);
		return;
	} else
		princ(x, fmt->aux_stream);
	l = fmt->aux_string->string.fillp;
	for (i = minpad;  l + i < mincol;  i += colinc)
		;
	if (!atsign) {
		write_string(fmt->aux_string, fmt->stream);
		while (i-- > 0)
			writec_stream(padchar, fmt->stream);
	} else {
		while (i-- > 0)
			writec_stream(padchar, fmt->stream);
		write_string(fmt->aux_string, fmt->stream);
	}
}

static void
fmt_S_expression(format_stack fmt, bool colon, bool atsign)
{
	int mincol, colinc, minpad, padchar;
	cl_object x;
	int l, i;

	ensure_param(fmt, 4);
	mincol = set_param(fmt, 0, INT, 0);
	colinc = set_param(fmt, 1, INT, 1);
	minpad = set_param(fmt, 2, INT, 0);
	padchar = set_param(fmt, 3, CHAR, ' ');

	fmt->aux_string->string.fillp = 0;
	fmt->aux_stream->stream.int0 = file_column(fmt->stream);
	fmt->aux_stream->stream.int1 = file_column(fmt->stream);
	x = fmt_advance(fmt);
	if (colon && Null(x))
		writestr_stream("()", fmt->aux_stream);
	else if (mincol == 0 && minpad == 0) {
		prin1(x, fmt->stream);
		return;
	} else
		prin1(x, fmt->aux_stream);
	l = fmt->aux_string->string.fillp;
	for (i = minpad;  l + i < mincol;  i += colinc)
		;
	if (!atsign) {
		write_string(fmt->aux_string, fmt->stream);
		while (i-- > 0)
			writec_stream(padchar, fmt->stream);
	} else {
		while (i-- > 0)
			writec_stream(padchar, fmt->stream);
		write_string(fmt->aux_string, fmt->stream);
	}
}


static void
fmt_integer(format_stack fmt, cl_object x, bool colon, bool atsign,
	    int radix, int mincol, int padchar, int commachar)
{
	int l, l1;
	int s;

	if (!FIXNUMP(x) && type_of(x) != t_bignum) {
		fmt->aux_string->string.fillp = 0;
		fmt->aux_stream->stream.int0 = file_column(fmt->stream);
		fmt->aux_stream->stream.int1 = file_column(fmt->stream);
		cl_setup_printer(fmt->aux_stream);
		PRINTescape = FALSE;
		PRINTbase = radix;
		cl_write_object(x);
		l = fmt->aux_string->string.fillp;
		mincol -= l;
		while (mincol-- > 0)
			writec_stream(padchar, fmt->stream);
		for (s = 0;  l > 0;  --l, s++)
			writec_stream(tempstr(fmt, s), fmt->stream);
		return;
	}
	fmt->aux_string->string.fillp = 0;
	fmt->aux_stream->stream.int0 = file_column(fmt->stream);
	fmt->aux_stream->stream.int1 = file_column(fmt->stream);
	PRINTstream = fmt->aux_stream;
	PRINTradix = FALSE;
	PRINTbase = radix;
	cl_write_object(x);
	l = l1 = fmt->aux_string->string.fillp;
	s = 0;
	if (tempstr(fmt, s) == '-')
		--l1;
	mincol -= l;
	if (colon)
		mincol -= (l1 - 1)/3;
	if (atsign && tempstr(fmt, s) != '-')
		--mincol;
	while (mincol-- > 0)
		writec_stream(padchar, fmt->stream);
	if (tempstr(fmt, s) == '-') {
		s++;
		writec_stream('-', fmt->stream);
	} else if (atsign)
		writec_stream('+', fmt->stream);
	while (l1-- > 0) {
		writec_stream(tempstr(fmt, s++), fmt->stream);
		if (colon && l1 > 0 && l1%3 == 0)
			writec_stream(commachar, fmt->stream);
	}
}

static void
fmt_decimal(format_stack fmt, bool colon, bool atsign)
{
	int mincol, padchar, commachar;

	ensure_param(fmt, 3);
	mincol = set_param(fmt, 0, INT, 0);
	padchar = set_param(fmt, 1, CHAR, ' ');
	commachar = set_param(fmt, 2, CHAR, ',');
	fmt_integer(fmt, fmt_advance(fmt), colon, atsign,
		    10, mincol, padchar, commachar);
}

static void
fmt_binary(format_stack fmt, bool colon, bool atsign)
{
	int mincol, padchar, commachar;

	ensure_param(fmt, 3);
	mincol = set_param(fmt, 0, INT, 0);
	padchar = set_param(fmt, 1, CHAR, ' ');
	commachar = set_param(fmt, 2, CHAR, ',');
	fmt_integer(fmt, fmt_advance(fmt), colon, atsign,
		    2, mincol, padchar, commachar);
}

static void
fmt_octal(format_stack fmt, bool colon, bool atsign)
{
	int mincol, padchar, commachar;

	ensure_param(fmt, 3);
	mincol = set_param(fmt, 0, INT, 0);
	padchar = set_param(fmt, 1, CHAR, ' ');
	commachar = set_param(fmt, 2, CHAR, ',');
	fmt_integer(fmt, fmt_advance(fmt), colon, atsign,
		    8, mincol, padchar, commachar);
}

static void
fmt_hexadecimal(format_stack fmt, bool colon, bool atsign)
{
	int mincol, padchar, commachar;

	ensure_param(fmt, 3);
	mincol = set_param(fmt, 0, INT, 0);
	padchar = set_param(fmt, 1, CHAR, ' ');
	commachar = set_param(fmt, 2, CHAR, ',');
	fmt_integer(fmt, fmt_advance(fmt), colon, atsign,
		    16, mincol, padchar, commachar);
}

static void
fmt_write_numeral(format_stack fmt, int s, int i)
{
	writestr_stream(fmt_numeral[tempstr(fmt, s) - '0' + i], fmt->stream);
}

static void
fmt_write_ordinal(format_stack fmt, int s, int i)
{
	writestr_stream(fmt_ordinal[tempstr(fmt, s) - '0' + i], fmt->stream);
}

static bool
fmt_thousand(format_stack fmt, int s, int i, bool b, bool o, int t)
{
	if (i == 3 && tempstr(fmt, s) > '0') {
		if (b)
			writec_stream(' ', fmt->stream);
		fmt_write_numeral(fmt, s, 0);
		writestr_stream(" hundred", fmt->stream);
		--i;
		s++;
		b = TRUE;
		if (o && (s > t))
			writestr_stream("th", fmt->stream);
	}
	if (i == 3) {
		--i;
		s++;
	}
	if (i == 2 && tempstr(fmt, s) > '0') {
		if (b)
			writec_stream(' ', fmt->stream);
		if (tempstr(fmt, s) == '1') {
			if (o && (s + 2 > t))
				fmt_write_ordinal(fmt, ++s, 10);
			else
				fmt_write_numeral(fmt, ++s, 10);
			return(TRUE);
		} else {
			if (o && (s + 1 > t))
				fmt_write_ordinal(fmt, s, 20);
			else
				fmt_write_numeral(fmt, s, 20);
			s++;
			if (tempstr(fmt, s) > '0') {
				writec_stream('-', fmt->stream);
				if (o && s + 1 > t)
					fmt_write_ordinal(fmt, s, 0);
				else
					fmt_write_numeral(fmt, s, 0);
			}
			return(TRUE);
		}
	}
	if (i == 2)
		s++;
	if (tempstr(fmt, s) > '0') {
		if (b)
			writec_stream(' ', fmt->stream);
		if (o && s + 1 > t)
			fmt_write_ordinal(fmt, s, 0);
		else
			fmt_write_numeral(fmt, s, 0);
		return(TRUE);
	}
	return(b);
}

static bool
fmt_nonillion(format_stack fmt, int s, int i, bool b, bool o, int t)
{
	int j;

	for (;  i > 3;  i -= j) {
		b = fmt_thousand(fmt, s, j = (i+2)%3+1, b, FALSE, t);
		if (j != 3 || tempstr(fmt, s) != '0' ||
		    tempstr(fmt, s+1) != '0' || tempstr(fmt, s+2) != '0') {
			writec_stream(' ', fmt->stream);
			writestr_stream(fmt_big_numeral[(i - 1)/3 - 1],
					fmt->stream);
			s += j;
			if (o && s > t)
				writestr_stream("th", fmt->stream);
		} else
			s += j;
	}
	return(fmt_thousand(fmt, s, i, b, o, t));
}		

static void
fmt_roman(format_stack fmt, int i, int one, int five, int ten, bool colon)
{
	int j;

	if (i == 0)
		return;
	if ((!colon && i < 4) || (colon && i < 5))
		for (j = 0;  j < i;  j++)
			writec_stream(one, fmt->stream);
	else if (!colon && i == 4) {
		writec_stream(one, fmt->stream);
		writec_stream(five, fmt->stream);
	} else if ((!colon && i < 9) || colon) {
		writec_stream(five, fmt->stream);
		for (j = 5;  j < i;  j++)
			writec_stream(one, fmt->stream);
	} else if (!colon && i == 9) {
		writec_stream(one, fmt->stream);
		writec_stream(ten, fmt->stream);
	}
}

static void
fmt_radix(format_stack fmt, bool colon, bool atsign)
{
	int radix, mincol, padchar, commachar;
	cl_object x;
	int i, j, k;
	int s, t;
	bool b;

	if (fmt->nparam == 0) {
		x = fmt_advance(fmt);
		assert_type_integer(x);
		if (atsign) {
			if (FIXNUMP(x))
				i = fix(x);
			else
				i = -1;
			if ((!colon && (i <= 0 || i >= 4000)) ||
			    (colon && (i <= 0 || i >= 5000))) {
				fmt_integer(fmt, x, FALSE, FALSE, 10, 0, ' ', ',');
				return;
			}
			fmt_roman(fmt, i/1000, 'M', '*', '*', colon);
			fmt_roman(fmt, i%1000/100, 'C', 'D', 'M', colon);
			fmt_roman(fmt, i%100/10, 'X', 'L', 'C', colon);
			fmt_roman(fmt, i%10, 'I', 'V', 'X', colon);
			return;
		}
		fmt->aux_string->string.fillp = 0;
		fmt->aux_stream->stream.int0 = file_column(fmt->stream);
		fmt->aux_stream->stream.int1 = file_column(fmt->stream);
		PRINTstream = fmt->aux_stream;
		PRINTradix = FALSE;
		PRINTbase = 10;
		cl_write_object(x);
		s = 0;
		i = fmt->aux_string->string.fillp;
		if (i == 1 && tempstr(fmt, s) == '0') {
			writestr_stream("zero", fmt->stream);
			if (colon)
				writestr_stream("th", fmt->stream);
			return;
		} else if (tempstr(fmt, s) == '-') {
			writestr_stream("minus ", fmt->stream);
			--i;
			s++;
		}
		t = fmt->aux_string->string.fillp;
		for (; tempstr(fmt, --t) == '0' ;) ;
		for (b = FALSE;  i > 0;  i -= j) {
			b = fmt_nonillion(fmt, s, j = (i+29)%30+1, b,
					  i<=30&&colon, t);
			s += j;
			if (b && i > 30) {
				for (k = (i - 1)/30;  k > 0;  --k)
					writestr_stream(" nonillion",
							fmt->stream);
				if (colon && s > t)
					writestr_stream("th", fmt->stream);
			}
		}
		return;
	}
	ensure_param(fmt, 4);
	radix = set_param(fmt, 0, INT, 10);
	mincol = set_param(fmt, 1, INT, 0);
	padchar = set_param(fmt, 2, CHAR, ' ');
	commachar = set_param(fmt, 3, CHAR, ',');
	x = fmt_advance(fmt);
	assert_type_integer(x);
	if (radix < 0 || radix > 36)
		FEerror("~D is illegal as a radix.", 1, MAKE_FIXNUM(radix));
	fmt_integer(fmt, x, colon, atsign, radix, mincol, padchar, commachar);
}	

static void
fmt_plural(format_stack fmt, bool colon, bool atsign)
{
	ensure_param(fmt, 0);
	if (colon) {
		if (fmt->index == fmt->base)
			fmt_error(fmt, "can't back up");
		--fmt->index;
	}
	if (eql(fmt_advance(fmt), MAKE_FIXNUM(1))) {
		if (atsign)
			writec_stream('y', fmt->stream);
	      }
	else
		if (atsign)
			writestr_stream("ies", fmt->stream);
		else
			writec_stream('s', fmt->stream);
}

static void
fmt_character(format_stack fmt, bool colon, bool atsign)
{
	cl_object x;
	cl_index i;

	ensure_param(fmt, 0);
	fmt->aux_string->string.fillp = 0;
	fmt->aux_stream->stream.int0 = 0;
	fmt->aux_stream->stream.int1 = 0;
	x = fmt_advance(fmt);
	assert_type_character(x);
	prin1(x, fmt->aux_stream);
	if (!colon && atsign)
		i = 0;
	else
		i = 2;
	for (;  i < fmt->aux_string->string.fillp;  i++)
		writec_stream(tempstr(fmt, i), fmt->stream);
}

static void
fmt_fix_float(format_stack fmt, bool colon, bool atsign)
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

	fmt_not_colon(fmt, colon);
	ensure_param(fmt, 5);
	w = set_param_positive(fmt, 0, "illegal width");
	d = set_param_positive(fmt, 1, "illegal number of digits");
	k = set_param(fmt, 2, INT, 0);
	overflowchar = set_param(fmt, 3, CHAR, -1);
	padchar = set_param(fmt, 4, CHAR, ' ');

	x = fmt_advance(fmt);
	if (FIXNUMP(x) ||
	    type_of(x) == t_bignum ||
	    type_of(x) == t_ratio)
		x = make_shortfloat(object_to_float(x));
	if (!REAL_TYPE(type_of(x))) {
		if (fmt->nparam > 1) fmt->nparam = 1;
		--fmt->index;
		fmt_decimal(fmt, colon, atsign);
		return;
	}
	if (type_of(x) == t_longfloat)
		n = 16;
	else
		n = 7;
	f = number_to_double(x);
	edit_double(n, f, &sign, buff, &exp);
	if (exp + k > 100 || exp + k < -100 || d > 100) {
		prin1(x, fmt->stream);
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
			w = set_param(fmt, 0, INT, 0);
			for (i = 0;  i < w;  i++)
				writec_stream(overflowchar, fmt->stream);
			return;
		}
		if (j < w && d < 0 && b[j-1] == '.') {
			b[j++] = '0';
			b[j] = '\0';
		}
		if (j < w && b[0] == '.') {
			*--b = '0';
			j++;
		}
		for (i = j;  i < w;  i++)
			writec_stream(padchar, fmt->stream);
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
		writec_stream('-', fmt->stream);
	else if (atsign)
		writec_stream('+', fmt->stream);
	writestr_stream(b, fmt->stream);
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
fmt_exponent1(cl_object stream, int e)
{
	if (e == 0)
		return;
	fmt_exponent1(stream, e/10);
	writec_stream('0' + e%10, stream);
}

static void
fmt_exponent(format_stack fmt, int e)
{
	if (e == 0) {
		writec_stream('0', fmt->stream);
		return;
	}
	if (e < 0)
		e = -e;
	fmt_exponent1(fmt->stream, e);
}

static void
fmt_exponential_float(format_stack fmt, bool colon, bool atsign)
{
	int w, d, e, k, overflowchar, padchar, exponentchar;
	double f;
	int sign;
	char buff[256], *b, buff1[256];
	int exp;
	int i, j;
	cl_object x, y;
	int n, m;
	cl_type t;

	b = buff1 + 1;

	fmt_not_colon(fmt, colon);
	ensure_param(fmt, 7);
	w = set_param_positive(fmt, 0, "illegal width");
	d = set_param_positive(fmt, 1, "illegal number of digits");
	e = set_param_positive(fmt, 2, "illegal number of digits in exponent");
	k = set_param(fmt, 3, INT, 1);
	overflowchar = set_param(fmt, 4, CHAR, -1);
	padchar = set_param(fmt, 5, CHAR, ' ');
	exponentchar = set_param(fmt, 6, CHAR, -1);

	x = fmt_advance(fmt);
	if (FIXNUMP(x) ||
	    type_of(x) == t_bignum ||
	    type_of(x) == t_ratio)
		x = make_shortfloat(object_to_float(x));
	if (!REAL_TYPE(type_of(x))) {
		if (fmt->nparam > 1) fmt->nparam = 1;
		--fmt->index;
		fmt_decimal(fmt, colon, atsign);
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
				fmt_error(fmt, "illegal scale factor");
			m = d + 1;
		} else {
			if (!(k > -d))
				fmt_error(fmt, "illegal scale factor");
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
		if (j < w && b[0] == '.') {
			*--b = '0';
			j++;
		}
		for (i = j;  i < w;  i++)
			writec_stream(padchar, fmt->stream);
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
		writec_stream('-', fmt->stream);
	else if (atsign)
		writec_stream('+', fmt->stream);
	writestr_stream(b, fmt->stream);
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
	writec_stream(exponentchar, fmt->stream);
	if (exp < 0)
		writec_stream('-', fmt->stream);
	else
		writec_stream('+', fmt->stream);
	if (e >= 0)
		for (i = e - fmt_exponent_length(exp);  i > 0;  --i)
			writec_stream('0', fmt->stream);
	fmt_exponent(fmt, exp);
	return;

OVER:
	w = set_param(fmt, 0, INT, -1);
	for (i = 0;  i < w;  i++)
		writec_stream(overflowchar, fmt->stream);
	return;
}

static void
fmt_general_float(format_stack fmt, bool colon, bool atsign)
{
	int w, d, e, k, overflowchar, padchar, exponentchar;
	int sign, exp;
	char buff[256];
	cl_object x;
	int n, ee, ww, q, dd;

	fmt_not_colon(fmt, colon);
	ensure_param(fmt, 7);
	w = set_param_positive(fmt, 0, "illegal width");
	d = set_param_positive(fmt, 1, "illegal number of digits");
	e = set_param_positive(fmt, 2, "illegal number of digits in exponent");
	k = set_param(fmt, 3, INT, 1);
	overflowchar = set_param(fmt, 4, CHAR, -1);
	padchar = set_param(fmt, 5, CHAR, ' ');
	exponentchar = set_param(fmt, 6, CHAR, -1);

	x = fmt_advance(fmt);
	if (!REAL_TYPE(type_of(x))) {
		if (fmt->nparam > 1) fmt->nparam = 1;
		--fmt->index;
		fmt_decimal(fmt, colon, atsign);
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
		fmt->nparam = 5;
		fmt->param[0].value = ww;
		fmt->param[1].value = dd;
		fmt->param[1].type = INT;
		fmt->param[2].type = NONE;
		fmt->param[3] = fmt->param[4];
		fmt->param[4] = fmt->param[5];
		--fmt->index;
		fmt_fix_float(fmt, colon, atsign);
		if (w >= 0)
			while (ww++ < w)
				writec_stream(padchar, fmt->stream);
		return;
	}
	fmt->param[1].value = d;
	fmt->param[1].type = INT;
	--fmt->index;
	fmt_exponential_float(fmt, colon, atsign);
}

static void
fmt_dollars_float(format_stack fmt, bool colon, bool atsign)
{
	int d, n, w, padchar;
	double f;
	int sign;
	char buff[256];
	int exp;
	int q, i;
	cl_object x;

	ensure_param(fmt, 4);
	d = set_param_positive(fmt, 0, "illegal number of digits");
	if (d < 0) d = 2;
	n = set_param_positive(fmt, 1, "illegal number of digits");
	if (n < 0) n = 1;
	w = set_param_positive(fmt, 2, "illegal width");
	if (w < 0) w = 0;
	padchar = set_param(fmt, 3, CHAR, ' ');
	x = fmt_advance(fmt);
	if (!REAL_TYPE(type_of(x))) {
		if (fmt->nparam < 3)
			fmt->nparam = 0;
		else {
			fmt->nparam = 1;
			fmt->param[0] = fmt->param[2];
		}
		--fmt->index;
		fmt_decimal(fmt, colon, atsign);
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
		fmt->nparam = 6;
		fmt->param[0] = fmt->param[2];
		fmt->param[1].value = d + n - 1;
		fmt->param[1].type = INT;
		fmt->param[2].type =
		fmt->param[3].type =
		fmt->param[4].type = NONE;
		fmt->param[5] = fmt->param[3];
		--fmt->index;
		fmt_exponential_float(fmt, colon, atsign);
	}
	if (exp > n)
		n = exp;
	if (sign < 0 || atsign)
		--w;
	if (colon) {
		if (sign < 0)
			writec_stream('-', fmt->stream);
		else if (atsign)
			writec_stream('+', fmt->stream);
		while (--w > n + d)
			writec_stream(padchar, fmt->stream);
	} else {
		while (--w > n + d)
			writec_stream(padchar, fmt->stream);
		if (sign < 0)
			writec_stream('-', fmt->stream);
		else if (atsign)
			writec_stream('+', fmt->stream);
	}
	for (i = n - exp;  i > 0;  --i)
		writec_stream('0', fmt->stream);
	for (i = 0;  i < exp;  i++)
		writec_stream((i < q ? buff[i] : '0'), fmt->stream);
	writec_stream('.', fmt->stream);
	for (d += i;  i < d;  i++)
		writec_stream((i < q ? buff[i] : '0'), fmt->stream);
}

static void
fmt_percent(format_stack fmt, bool colon, bool atsign)
{
	int n, i;

	ensure_param(fmt, 1);
	n = set_param(fmt, 0, INT, 1);
	fmt_not_colon(fmt, colon);
	fmt_not_atsign(fmt, atsign);
	while (n-- > 0) {
		writec_stream('\n', fmt->stream);
		if (n == 0)
			for (i = fmt->indents;  i > 0;  --i)
				writec_stream(' ', fmt->stream);
	}
}

static void
fmt_ampersand(format_stack fmt, bool colon, bool atsign)
{
	int n;

	ensure_param(fmt, 1);
	n = set_param(fmt, 0, INT, 1);
	fmt_not_colon(fmt, colon);
	fmt_not_atsign(fmt, atsign);
	if (n == 0)
		return;
	if (file_column(fmt->stream) != 0)
		writec_stream('\n', fmt->stream);
	while (--n > 0)
		writec_stream('\n', fmt->stream);
	fmt->indents = 0;
}

static void
fmt_bar(format_stack fmt, bool colon, bool atsign)
{
	int n;

	ensure_param(fmt, 1);
	n = set_param(fmt, 0, INT, 1);
	fmt_not_colon(fmt, colon);
	fmt_not_atsign(fmt, atsign);
	while (n-- > 0)
		writec_stream('\f', fmt->stream);
}

static void
fmt_tilde(format_stack fmt, bool colon, bool atsign)
{
	int n;

	ensure_param(fmt, 1);
	n = set_param(fmt, 0, INT, 1);
	fmt_not_colon(fmt, colon);
	fmt_not_atsign(fmt, atsign);
	while (n-- > 0)
		writec_stream('~', fmt->stream);
}

static void
fmt_newline(format_stack fmt, bool colon, bool atsign)
{
	ensure_param(fmt, 0);
	fmt_not_colon_atsign(fmt, colon, atsign);
	if (atsign)
		writec_stream('\n', fmt->stream);
	while (fmt->ctl_index < fmt->ctl_end && isspace(fmt->ctl_str[fmt->ctl_index])) {
		if (colon)
			writec_stream(fmt->ctl_str[fmt->ctl_index], fmt->stream);
		fmt->ctl_index++;
	}
}

static void
fmt_tabulate(format_stack fmt, bool colon, bool atsign)
{
	int colnum, colinc;
	int c, i;
	
	ensure_param(fmt, 2);
	fmt_not_colon(fmt, colon);
	colnum = set_param(fmt, 0, INT, 1);
	colinc = set_param(fmt, 1, INT, 1);
	if (!atsign) {
		c = file_column(fmt->stream);
		if (c < 0) {
			writestr_stream("  ", fmt->stream);
			return;
		}
		if (c > colnum && colinc <= 0)
			return;
		while (c > colnum)
			colnum += colinc;
		for (i = colnum - c;  i > 0;  --i)
			writec_stream(' ', fmt->stream);
	} else {
		for (i = colnum;  i > 0;  --i)
			writec_stream(' ', fmt->stream);
		c = file_column(fmt->stream);
		if (c < 0 || colinc <= 0)
			return;
		colnum = 0;
		while (c > colnum)
			colnum += colinc;
		for (i = colnum - c;  i > 0;  --i)
			writec_stream(' ', fmt->stream);
	}
}

static void
fmt_asterisk(format_stack fmt, bool colon, bool atsign)
{
	int n;

	ensure_param(fmt, 1);
	fmt_not_colon_atsign(fmt, colon, atsign);
	if (atsign) {
		n = set_param(fmt, 0, INT, 0);
		n += fmt->base;
		if (n < fmt->base || n >= fmt->end)
			fmt_error(fmt, "can't goto");
		fmt->index = n;
	} else if (colon) {
		n = set_param(fmt, 0, INT, 1);
		if (n > fmt->index)
			fmt_error(fmt, "can't back up");
		fmt->index -= n;
	} else {
		n = set_param(fmt, 0, INT, 1);
		while (n-- > 0)
			fmt_advance(fmt);
	}
}	

static void
fmt_indirection(format_stack fmt, bool colon, bool atsign)
{
	cl_object s, l;
	struct format_stack_struct fmt_old;
	jmp_buf fmt_jmp_buf0;
	int up_colon;

	ensure_param(fmt, 0);
	fmt_not_colon(fmt, colon);
	s = fmt_advance(fmt);
	if (type_of(s) != t_string)
		fmt_error(fmt, "control string expected");
	if (atsign) {
		fmt_copy(&fmt_old, fmt);
		fmt->jmp_buf = &fmt_jmp_buf0;
		fmt->string = s;
		if ((up_colon = ecl_setjmp(*fmt->jmp_buf))) {
			if (--up_colon)
				fmt_error(fmt, "illegal ~:^");
		} else
			format(fmt, s->string.self, s->string.fillp);
		fmt_copy1(fmt, &fmt_old);
	} else {
		l = fmt_advance(fmt);
		fmt_copy(&fmt_old, fmt);
		fmt->base = cl_stack_index();
		fmt_push_list(fmt, l);
		fmt->index = fmt->base;
		fmt->end = cl_stack_index();
		fmt->jmp_buf = &fmt_jmp_buf0;
		fmt->string = s;
		if ((up_colon = ecl_setjmp(*fmt->jmp_buf))) {
			if (--up_colon)
				fmt_error(fmt, "illegal ~:^");
		} else
			format(fmt, s->string.self, s->string.fillp);
		cl_stack_set_index(fmt->base);
		fmt_copy(fmt, &fmt_old);
	}
}

static void
fmt_case(format_stack fmt, bool colon, bool atsign)
{
	cl_object x;
	cl_index i;
	int j;
	struct format_stack_struct fmt_old;
	jmp_buf fmt_jmp_buf0;
	int up_colon;
	bool b;

	x = make_string_output_stream(64);
	i = fmt->ctl_index;
	j = fmt_skip(fmt);
	if (fmt->ctl_str[--j] != ')' || fmt->ctl_str[--j] != '~')
		fmt_error(fmt, "~) expected");
	fmt_copy(&fmt_old, fmt);
	fmt->stream = x;
	fmt->jmp_buf = &fmt_jmp_buf0;
	if ((up_colon = ecl_setjmp(*fmt->jmp_buf)))
		;
	else
		format(fmt, fmt->ctl_str + i, j - i);
	fmt_copy1(fmt, &fmt_old);
	x = x->stream.object0;
	if (!colon && !atsign)
		for (i = 0;  i < x->string.fillp;  i++) {
			if (isupper(j = x->string.self[i]))
				j = tolower(j);
			writec_stream(j, fmt->stream);
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
			writec_stream(j, fmt->stream);
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
			writec_stream(j, fmt->stream);
		}
	else
		for (i = 0;  i < x->string.fillp;  i++) {
			if (islower(j = x->string.self[i]))
				j = toupper(j);
			writec_stream(j, fmt->stream);
		}
	if (up_colon)
		ecl_longjmp(*fmt->jmp_buf, up_colon);
}

static void
fmt_conditional(format_stack fmt, bool colon, bool atsign)
{
	int i, j, k;
	cl_object x;
	int n;
	bool done;
	struct format_stack_struct fmt_old;

	fmt_not_colon_atsign(fmt, colon, atsign);
	if (colon) {
		ensure_param(fmt, 0);
		i = fmt->ctl_index;
		j = fmt_skip(fmt);
		if (fmt->ctl_str[--j] != ';' || fmt->ctl_str[--j] != '~')
			fmt_error(fmt, "~; expected");
		k = fmt_skip(fmt);
		if (fmt->ctl_str[--k] != ']' || fmt->ctl_str[--k] != '~')
			fmt_error(fmt, "~] expected");
		if (Null(fmt_advance(fmt))) {
			fmt_copy(&fmt_old, fmt);
			format(fmt, fmt->ctl_str + i, j - i);
			fmt_copy1(fmt, &fmt_old);
		} else {
			fmt_copy(&fmt_old, fmt);
			format(fmt, fmt->ctl_str + j + 2, k - (j + 2));
			fmt_copy1(fmt, &fmt_old);
		}
	} else if (atsign) {
		i = fmt->ctl_index;
		j = fmt_skip(fmt);
		if (fmt->ctl_str[--j] != ']' || fmt->ctl_str[--j] != '~')
			fmt_error(fmt, "~] expected");
		if (Null(fmt_advance(fmt)))
			;
		else {
			--fmt->index;
			fmt_copy(&fmt_old, fmt);
			format(fmt, fmt->ctl_str + i, j - i);
			fmt_copy1(fmt, &fmt_old);
		}
	} else {
		ensure_param(fmt, 1);
		if (fmt->nparam == 0) {
			x = fmt_advance(fmt);
			if (!FIXNUMP(x))
				fmt_error(fmt, "illegal argument for conditional");
			n = fix(x);
		} else
			n = set_param(fmt, 0, INT, 0);
		i = fmt->ctl_index;
		for (done = FALSE;;  --n) {
			j = fmt_skip(fmt);
			for (k = j;  fmt->ctl_str[--k] != '~';)
				;
			if (n == 0) {
				fmt_copy(&fmt_old, fmt);
				format(fmt, fmt->ctl_str + i, k - i);
				fmt_copy1(fmt, &fmt_old);
				done = TRUE;
			}
			i = j;
			if (fmt->ctl_str[--j] == ']') {
				if (fmt->ctl_str[--j] != '~')
					fmt_error(fmt, "~] expected");
				return;
			}
			if (fmt->ctl_str[j] == ';') {
				if (fmt->ctl_str[--j] == '~')
					continue;
				if (fmt->ctl_str[j] == ':')
					goto ELSE;
			}
			fmt_error(fmt, "~; or ~] expected");
		}
	ELSE:
		if (fmt->ctl_str[--j] != '~')
			fmt_error(fmt, "~:; expected");
		j = fmt_skip(fmt);
		if (fmt->ctl_str[--j] != ']' || fmt->ctl_str[--j] != '~')
			fmt_error(fmt, "~] expected");
		if (!done) {
			fmt_copy(&fmt_old, fmt);
			format(fmt, fmt->ctl_str + i, j - i);
			fmt_copy1(fmt, &fmt_old);
		}
	}
}

static void
fmt_iteration(format_stack fmt, bool colon, bool atsign)
{
	int n, i;
	const char *o;
	volatile int j;
	bool colon_close = FALSE;
	cl_object l;
	struct format_stack_struct fmt_old;
	jmp_buf fmt_jmp_buf0;
	int up_colon;

	ensure_param(fmt, 1);
	n = set_param(fmt, 0, INT, 1000000);
	i = fmt->ctl_index;
	j = fmt_skip(fmt);
	if (fmt->ctl_str[--j] != '}')
		fmt_error(fmt, "~} expected");
	if (fmt->ctl_str[--j] == ':') {
		colon_close = TRUE;
		--j;
	}
	if (fmt->ctl_str[j] != '~')
		fmt_error(fmt, "syntax error");
	o = fmt->ctl_str;
	if (!colon && !atsign) {
		l = fmt_advance(fmt);
		fmt_copy(&fmt_old, fmt);
		fmt->base = cl_stack_index();
		fmt_push_list(fmt, l);
		fmt->index = fmt->base;
		fmt->end = cl_stack_index();
		fmt->jmp_buf = &fmt_jmp_buf0;
		if (colon_close)
			goto L1;
		while (fmt->index < fmt->end) {
		L1:
			if (n-- <= 0)
				break;
			if ((up_colon = ecl_setjmp(*fmt->jmp_buf))) {
				if (--up_colon)
					fmt_error(fmt, "illegal ~:^");
				break;
			}
			format(fmt, o + i, j - i);
		}
		cl_stack_set_index(fmt->base);
		fmt_copy(fmt, &fmt_old);
	} else if (colon && !atsign) {
	  	int fl = 0;
		volatile cl_object l0;
		l0 = fmt_advance(fmt);
		fmt_copy(&fmt_old, fmt);
		for (l = l0; !endp(l); l = CDR(l))
		  fl += length(CAR(l));
		fmt->base = cl_stack_index();
		fmt->jmp_buf = &fmt_jmp_buf0;
		if (colon_close)
			goto L2;
		while (!endp(l0)) {
		L2:
			if (n-- <= 0)
				break;
			l = CAR(l0);
			l0 = CDR(l0);
			fmt_push_list(fmt, l);
			fmt->index = fmt->base;
			fmt->end = cl_stack_index();
			if ((up_colon = ecl_setjmp(*fmt->jmp_buf))) {
				if (--up_colon)
					break;
				else
					continue;
			}
			format(fmt, o + i, j - i);
			cl_stack_set_index(fmt->base);
		}
		fmt_copy(fmt, &fmt_old);
	} else if (!colon && atsign) {
		fmt_copy(&fmt_old, fmt);
		fmt->jmp_buf = &fmt_jmp_buf0;
		if (colon_close)
			goto L3;
		while (fmt->index < fmt->end) {
		L3:
			if (n-- <= 0)
				break;
			if ((up_colon = ecl_setjmp(*fmt->jmp_buf))) {
				if (--up_colon)
					fmt_error(fmt, "illegal ~:^");
				break;
			}
			format(fmt, o + i, j - i);
		}
		fmt_copy1(fmt, &fmt_old);
	} else if (colon && atsign) {
		if (colon_close)
			goto L4;
		while (fmt->index < fmt->end) {
		L4:
			if (n-- <= 0)
				break;
			l = fmt_advance(fmt);
			fmt_copy(&fmt_old, fmt);
			fmt->base = cl_stack_index();
			fmt_push_list(fmt, l);
			fmt->index = fmt->base;
			fmt->end = cl_stack_index();
			fmt->jmp_buf = &fmt_jmp_buf0;
			if ((up_colon = ecl_setjmp(*fmt->jmp_buf))) {
				fmt_copy(fmt, &fmt_old);
				if (--up_colon)
					break;
				else
					continue;
			}
			format(fmt, o + i, j - i);
			cl_stack_set_index(fmt->base);
			fmt_copy(fmt, &fmt_old);
		}
	}
}

static void
fmt_justification(format_stack fmt, volatile bool colon, bool atsign)
{
	int mincol, colinc, minpad, padchar;
	volatile cl_index fields_start;
	cl_index fields_end;
	struct format_stack_struct fmt_old;
	jmp_buf fmt_jmp_buf0;
	volatile int i, j, k, l, m, j0, l0;
	int up_colon;
	volatile cl_object special = Cnil;
	volatile int spare_spaces, line_length;

	ensure_param(fmt, 4);
	mincol = set_param(fmt, 0, INT, 0);
	colinc = set_param(fmt, 1, INT, 1);
	minpad = set_param(fmt, 2, INT, 0);
	padchar = set_param(fmt, 3, CHAR, ' ');

	fields_start = cl_stack_index();
	for (;;) {
		cl_object this_field = make_string_output_stream(64);
		i = fmt->ctl_index;
		j0 = j = fmt_skip(fmt);
		while (fmt->ctl_str[--j] != '~')
			;

		fmt_copy(&fmt_old, fmt);
		fmt->jmp_buf = &fmt_jmp_buf0;
		if ((up_colon = ecl_setjmp(*fmt->jmp_buf))) {
			if (--up_colon)
				fmt_error(fmt, "illegal ~:^");
			fmt_copy1(fmt, &fmt_old);
			while (fmt->ctl_str[--j0] != '>')
				j0 = fmt_skip(fmt);
			if (fmt->ctl_str[--j0] != '~')
				fmt_error(fmt, "~> expected");
			break;
		}
		fmt->stream = this_field;
		format(fmt, fmt->ctl_str + i, j - i);
		cl_stack_push(this_field->stream.object0);
		fmt_copy1(fmt, &fmt_old);

		if (fmt->ctl_str[--j0] == '>') {
			if (fmt->ctl_str[--j0] != '~')
				fmt_error(fmt, "~> expected");
			break;
		} else if (fmt->ctl_str[j0] != ';')
			fmt_error(fmt, "~; expected");
		else if (fmt->ctl_str[--j0] == ':') {
			if (cl_stack_index() - fields_start != 1 || !Null(special))
				fmt_error(fmt, "illegal ~:;");
			special = cl_stack_pop();
			for (j = j0;  fmt->ctl_str[j] != '~';  --j)
				;
			fmt_copy(&fmt_old, fmt);
			format(fmt, fmt->ctl_str + j, j0 - j + 2);
			fmt_copy1(fmt, &fmt_old);
			spare_spaces = fmt->spare_spaces;
			line_length = fmt->line_length;
		} else if (fmt->ctl_str[j0] != '~')
			fmt_error(fmt, "~; expected");
	}
	/*
	 * Compute the length of items to be output. If the clause ~:; was
	 * found, the first item is not included.
	 */
	fields_end = cl_stack_index();
	for (i = fields_start, l = 0;  i < fields_end;  i++)
		l += cl_stack[i]->string.fillp;
	/*
	 * Count the number of segments that need padding, "M". If the colon
	 * modifier, the first item needs padding. If the @@ modifier is
	 * present, the last modifier also needs padding.
	 */
	m = fields_end - fields_start - 1;
	if (m <= 0 && !colon && !atsign) {
		m = 0;
		colon = TRUE;
	}
	if (colon)
		m++;
	if (atsign)
		m++;
	/*
	 * Count the minimal length in which the text fits. This length must
	 * the smallest integer of the form l = mincol + k * colinc. If the
	 * length exceeds the line length, the text before the ~:; is output
	 * first.
	 */
	l0 = l;
	l += minpad * m;
	for (k = 0;  mincol + k * colinc < l;  k++)
		;
	l = mincol + k * colinc;
	if (special != Cnil &&
	    file_column(fmt->stream) + l + spare_spaces > line_length)
		princ(special, fmt->stream);
	/*
	 * Output the text with the padding segments. The total number of
	 * padchars is kept in "l", and it is shared equally among all segments.
	 */
	l -= l0;
	for (i = fields_start;  i < fields_end;  i++) {
		if (i > fields_start || colon)
			for (j = l / m, l -= j, --m;  j > 0;  --j)
				writec_stream(padchar, fmt->stream);
		princ(cl_stack[i], fmt->stream);
	}
	if (atsign)
		for (j = l;  j > 0;  --j)
			writec_stream(padchar, fmt->stream);
	cl_stack_set_index(fields_start);
}

static void
fmt_up_and_out(format_stack fmt, bool colon, bool atsign)
{
	int i, j, k;

	ensure_param(fmt, 3);
	fmt_not_atsign(fmt, atsign);
	if (fmt->nparam == 0) {
		if (fmt->index >= fmt->end)
			ecl_longjmp(*fmt->jmp_buf, ++colon);
	} else if (fmt->nparam == 1) {
		i = set_param(fmt, 0, INT, 0);
		if (i == 0)
			ecl_longjmp(*fmt->jmp_buf, ++colon);
	} else if (fmt->nparam == 2) {
		i = set_param(fmt, 0, INT, 0);
		j = set_param(fmt, 1, INT, 0);
		if (i == j)
			ecl_longjmp(*fmt->jmp_buf, ++colon);
	} else {
		i = set_param(fmt, 0, INT, 0);
		j = set_param(fmt, 1, INT, 0);
		k = set_param(fmt, 2, INT, 0);
		if (i <= j && j <= k)
			ecl_longjmp(*fmt->jmp_buf, ++colon);
	}
}

static void
fmt_semicolon(format_stack fmt, bool colon, bool atsign)
{
	fmt_not_atsign(fmt, atsign);
	if (!colon)
		fmt_error(fmt, "~:; expected");
	ensure_param(fmt, 2);
	fmt->spare_spaces = set_param(fmt, 0, INT, 0);
	fmt->line_length = set_param(fmt, 1, INT, 72);
}

@(defun format (strm string &rest args)
	cl_object x = OBJNULL;
	struct format_stack_struct fmt;
	jmp_buf fmt_jmp_buf0;
	int colon;
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
	assert_type_string(string);
	fmt.stream = strm;
	fmt.base = cl_stack_index();
	for (narg -= 2; narg; narg--)
		cl_stack_push(cl_va_arg(args));
	fmt.index = fmt.base;
	fmt.end = cl_stack_index();
	fmt.jmp_buf = &fmt_jmp_buf0;
	if (symbol_value(@'si::*indent-formatted-output*') != Cnil)
		fmt.indents = file_column(strm);
	else
		fmt.indents = 0;
	fmt.string = string;
	fmt.aux_stream = get_aux_stream();
	fmt.aux_string = fmt.aux_stream->stream.object0;
	if ((colon = ecl_setjmp(*fmt.jmp_buf))) {
		if (--colon)
			fmt_error(&fmt, "illegal ~:^");
	} else {
		format(&fmt, string->string.self, string->string.fillp);
		flush_stream(strm);
	}
	cl_stack_set_index(fmt.base);
	fmt_aux_stream = fmt.aux_stream;
	@(return (x == OBJNULL? Cnil : x))
@)

static void
format(format_stack fmt, const char *str, cl_index end)
{
	int c;
	cl_index i, n;
	bool colon, atsign;
	cl_object x;

	fmt->ctl_str = str;
	fmt->ctl_index = 0;
	fmt->ctl_end = end;

LOOP:
	if (fmt->ctl_index >= fmt->ctl_end)
		return;
	if ((c = ctl_advance(fmt)) != '~') {
		writec_stream(c, fmt->stream);
		goto LOOP;
	}
	n = 0;
	for (;;) {
		switch (c = ctl_advance(fmt)) {
		case ',':
			fmt->param[n].type = NONE;
			break;

		case '0':  case '1':  case '2':  case '3':  case '4':
		case '5':  case '6':  case '7':  case '8':  case '9':
		DIGIT:
			i = 0;
			do {
				i = i*10 + (c - '0');
				c = ctl_advance(fmt);
			} while (isdigit(c));
			fmt->param[n].type = INT;
			fmt->param[n].value = i;
			break;

		case '+':
			c = ctl_advance(fmt);
			if (!isdigit(c))
				fmt_error(fmt, "digit expected");
			goto DIGIT;

		case '-':
			c = ctl_advance(fmt);
			if (!isdigit(c))
				fmt_error(fmt, "digit expected");
			i = 0;
			do {
				i = i*10 + (c - '0');
				c = ctl_advance(fmt);
			} while (isdigit(c));
			fmt->param[n].type = INT;
			fmt->param[n].value = -i;
			break;

		case '\'':
			fmt->param[n].type = CHAR;
			fmt->param[n].value = ctl_advance(fmt);
			c = ctl_advance(fmt);
			break;

		case 'v':  case 'V':
			x = fmt_advance(fmt);
			if (FIXNUMP(x)) {
				fmt->param[n].type = INT;
				fmt->param[n].value = fix(x);
			} else if (type_of(x) == t_character) {
				fmt->param[n].type = CHAR;
				fmt->param[n].value = CHAR_CODE(x);
			} else
				fmt_error(fmt, "illegal V parameter");
			c = ctl_advance(fmt);
			break;

		case '#':
			fmt->param[n].type = INT;
			fmt->param[n].value = fmt->end - fmt->index;
			c = ctl_advance(fmt);
			break;

		default:
			if (n > 0)
				fmt_error(fmt, "illegal ,");
			else
				goto DIRECTIVE;
		}
		n++;
		if (n == FMT_MAX_PARAM)
			fmt_error(fmt, "too many parameters");
		if (c != ',')
			break;
	}

DIRECTIVE:
	colon = atsign = FALSE;
	if (c == ':') {
		colon = TRUE;
		c = ctl_advance(fmt);
	}
	if (c == '@@') {
		atsign = TRUE;
		c = ctl_advance(fmt);
	}
	fmt->nparam = n;
	switch (c) {
	case 'a':  case 'A':
		fmt_ascii(fmt, colon, atsign);
		break;

	case 's':  case 'S':
		fmt_S_expression(fmt, colon, atsign);
		break;

	case 'd':  case 'D':
		fmt_decimal(fmt, colon, atsign);
		break;

	case 'b':  case 'B':
		fmt_binary(fmt, colon, atsign);
		break;

	case 'o':  case 'O':
		fmt_octal(fmt, colon, atsign);
		break;

	case 'x':  case 'X':
		fmt_hexadecimal(fmt, colon, atsign);
		break;

	case 'r':  case 'R':
		fmt_radix(fmt, colon, atsign);
		break;

	case 'p':  case 'P':
		fmt_plural(fmt, colon, atsign);
		break;

	case 'c':  case 'C':
		fmt_character(fmt, colon, atsign);
		break;

	case 'f':  case 'F':
		fmt_fix_float(fmt, colon, atsign);
		break;

	case 'e':  case 'E':
		fmt_exponential_float(fmt, colon, atsign);
		break;

	case 'g':  case 'G':
		fmt_general_float(fmt, colon, atsign);
		break;

	case '$':
		fmt_dollars_float(fmt, colon, atsign);
		break;

	case '%':
		fmt_percent(fmt, colon, atsign);
		break;

	case '&':
		fmt_ampersand(fmt, colon, atsign);
		break;

	case '|':
		fmt_bar(fmt, colon, atsign);
		break;

	case '~':
		fmt_tilde(fmt, colon, atsign);
		break;

	case '\n':
	case '\r':
		fmt_newline(fmt, colon, atsign);
		break;

	case 't':  case 'T':
		fmt_tabulate(fmt, colon, atsign);
		break;

	case '*':
		fmt_asterisk(fmt, colon, atsign);
		break;

	case '?':
		fmt_indirection(fmt, colon, atsign);
		break;

	case '(':
		fmt_case(fmt, colon, atsign);
		break;

	case '[':
		fmt_conditional(fmt, colon, atsign);
		break;

	case '{':
		fmt_iteration(fmt, colon, atsign);
		break;

	case '<':
		fmt_justification(fmt, colon, atsign);
		break;

	case '^':
		fmt_up_and_out(fmt, colon, atsign);
		break;

	case ';':
		fmt_semicolon(fmt, colon, atsign);
		break;

	default:
		fmt_error(fmt, "illegal directive");
	}
	goto LOOP;
}

void
init_format(void)
{
	fmt_aux_stream = make_string_output_stream(64);
	ecl_register_static_root(&fmt_aux_stream);

	SYM_VAL(@'si::*indent-formatted-output*') = Cnil;
}
