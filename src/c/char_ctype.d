/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    char_ctype.d -- Character properties.
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

#ifndef ECL_UNICODE
#include <ctype.h>

bool
ecl_graphic_char_p(ecl_character code)
{
	return code == ' ' || isgraph(code);
}

bool
ecl_alpha_char_p(ecl_character code)
{
	return isalpha(code);
}

bool
ecl_upper_case_p(ecl_character code)
{
	return isupper(code);
}

bool
ecl_lower_case_p(ecl_character code)
{
	return islower(code);
}

bool
ecl_both_case_p(ecl_character code)
{
	return islower(code) || isupper(code);
}

bool
ecl_alphanumericp(ecl_character i)
{
	return isalnum(i);
}

ecl_character
ecl_char_upcase(ecl_character code)
{
	return toupper(code);
}

ecl_character
ecl_char_downcase(ecl_character code)
{
	return tolower(code);
}

#else /* ECL_UNICODE */

static uint8_t *
ucd_char_data(ecl_character code)
{
	unsigned char page = cl_core.ucd_pages[code >> 8];
	return cl_core.ucd_data + ((cl_index)page << 10) + 4 * (code & 0xFF);
}

static cl_index
ucd_value_0(ecl_character code)
{
	return ucd_char_data(code)[0];
}

#define read_3bytes(c) c[0] + (c[1] << 8) + (c[2] << 16)

static ecl_character
ucd_value_1(ecl_character code)
{
	uint8_t *c = ucd_char_data(code);
	return read_3bytes(c);
}

static int
ucd_general_category(ecl_character code)
{
	return cl_core.ucd_misc[8 * ucd_value_0(code)];
}

static int
ucd_decimal_digit(ecl_character code)
{
	return cl_core.ucd_misc[3 + 8 * ucd_value_0(code)];
}

bool
ecl_graphic_char_p(ecl_character code)
{
	/* compatible to SBCL */
	return code > 159 || ((31 < code) && (code < 127));
}

bool
ecl_alpha_char_p(ecl_character code)
{
	return ucd_general_category(code) < 5;
}

bool
ecl_upper_case_p(ecl_character code)
{
	return ucd_value_0(code) == 0;
}

bool
ecl_lower_case_p(ecl_character code)
{
	return ucd_value_0(code) == 1;
}

bool
ecl_both_case_p(ecl_character code)
{
	return ucd_value_0(code) < 2;
}

bool
ecl_alphanumericp(ecl_character i)
{
	int gc = ucd_general_category(i);
	return (gc < 5) || (gc == 12);
}

ecl_character
ecl_char_upcase(ecl_character code)
{
	uint8_t *c = ucd_char_data(code);
	if (c[0] == 1) {
		c++;
		return read_3bytes(c);
	} else {
		return code;
	}
}

ecl_character
ecl_char_downcase(ecl_character code)
{
	uint8_t *c = ucd_char_data(code);
	if (c[0] == 0) {
		c++;
		return read_3bytes(c);
	} else {
		return code;
	}
}
#endif
