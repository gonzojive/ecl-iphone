/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    string.d -- String routines.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under thep terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#include <ecl/ecl.h>
#include <ctype.h>
#ifdef ECL_UNICODE
#include <wctype.h>
#endif
#include <string.h>
#include <ecl/ecl-inl.h>

static cl_object
do_make_base_string(cl_index s, int code)
{
	cl_object x = cl_alloc_simple_base_string(s);
	cl_index i;
	for (i = 0;  i < s;  i++)
		x->base_string.self[i] = code;
	return x;
}

#ifdef ECL_UNICODE
static cl_object
do_make_string(cl_index s, cl_index code)
{
	cl_object x = cl_alloc_simple_extended_string(s);
	cl_object c = CODE_CHAR(code);
	cl_index i;
	for (i = 0;  i < s;  i++)
		x->string.self[i] = c;
	return x;
}
#else
#define do_make_string do_make_base_string
#endif

@(defun make_string (size &key (initial_element CODE_CHAR(' '))
		     (element_type @'character'))
	cl_index s;
	cl_object x;
@
	s = ecl_to_index(size);
	/* INV: ecl_[base_]char_code() checks the type of initial_element() */
	if (element_type == @'base-char' || element_type == @'standard-char') {
		int code = ecl_base_char_code(initial_element);
		x = do_make_base_string(s, code);
	} else if (element_type == @'character') {
		cl_index code = ecl_char_code(initial_element);
		x = do_make_string(s, code);
	} else if (funcall(3, @'subtypep', element_type, @'base-char') == Ct) {
		int code = ecl_base_char_code(initial_element);
		x = do_make_base_string(s, code);
	} else if (funcall(3, @'subtypep', element_type, @'character') == Ct) {
		cl_index code = ecl_char_code(initial_element);
		x = do_make_string(s, code);
	} else {
		FEerror("The type ~S is not a valid string char type.",
			1, element_type);
	}
	@(return x)
@)

cl_object
cl_alloc_simple_base_string(cl_index length)
{
	cl_object x;

	x = cl_alloc_object(t_base_string);
	x->base_string.hasfillp     = FALSE;
	x->base_string.adjustable   = FALSE;
	x->base_string.displaced    = Cnil;
	x->base_string.dim          = (x->base_string.fillp = length);
	x->base_string.self         = (char *)cl_alloc_atomic(length+1);
	x->base_string.self[length] = x->base_string.self[0] = 0;
	return(x);
}

#ifdef ECL_UNICODE
cl_object
cl_alloc_simple_extended_string(cl_index length)
{
	cl_object x;

        /* should this call si_make_vector? */
	x = cl_alloc_object(t_string);
	x->string.hasfillp   = FALSE;
	x->string.adjustable = FALSE;
	x->string.displaced  = Cnil;
	x->string.dim        = x->string.fillp = length;
	x->string.self       = (cl_object *)cl_alloc_align(sizeof (cl_object)*length, sizeof (cl_object));
	return(x);
}
#endif

/*
	Make a string of a certain size, with some eading zeros to
	keep C happy. The string must be adjustable, to allow further
	growth. (See unixfsys.c for its use).
*/
cl_object
cl_alloc_adjustable_base_string(cl_index l)
{
	cl_object output = cl_alloc_simple_base_string(l);
	output->base_string.fillp = 0;
	output->base_string.hasfillp = TRUE;
 	output->base_string.adjustable = TRUE;
	return output;
}

/*
	Make_simple_base_string(s) makes a simple-base string from C string s.
*/
cl_object
make_simple_base_string(char *s)
{
	cl_object x;
	cl_index l = strlen(s);

	x = cl_alloc_object(t_base_string);
	x->base_string.hasfillp = FALSE;
	x->base_string.adjustable = FALSE;
	x->base_string.displaced = Cnil;
	x->base_string.dim = (x->base_string.fillp = l);
	x->base_string.self = s;
	
	return(x);
}

cl_object
make_base_string_copy(const char *s)
{
	cl_object x;
	cl_index l = strlen(s);

	x = cl_alloc_simple_base_string(l);
	memcpy(x->base_string.self, s, l);
	return(x);
}

cl_object
ecl_cstring_to_base_string_or_nil(const char *s)
{
	if (s == NULL)
		return Cnil;
	else
		return make_base_string_copy(s);
}

bool
ecl_fits_in_base_string(cl_object s)
{
 AGAIN:
	switch (type_of(s)) {
#ifdef ECL_UNICODE
	case t_string: {
		cl_index i;
		for (i = 0; i < s->string.fillp; i++) {
			if (!BASE_CHAR_P(s->string.self[i]))
				return 0;
		}
		return 1;
	}
#endif
	case t_base_string:
		return 1;
	default:
		s = ecl_type_error(@'si::copy-to-simple-base-string',"",s,@'string');
		goto AGAIN;
	}
}

cl_object
si_copy_to_simple_base_string(cl_object x)
{
	cl_object y;
 AGAIN:
	switch(type_of(x)) {
	case t_symbol:
		x = x->symbol.name;
		goto AGAIN;
	case t_character:
		x = cl_string(x);
		goto AGAIN;
#ifdef ECL_UNICODE
	case t_string: {
		cl_index index, length = x->string.fillp;
		y = cl_alloc_simple_base_string(length);
		for (index=0; index < length; index++) {
			cl_object c = x->string.self[index];
			if (!BASE_CHAR_P(c))
				FEerror("Cannot coerce string ~A to a base-string", 1, x);
			y->base_string.self[index] = CHAR_CODE(c);
		}
		break;
	}
#endif
	case t_base_string: {
		cl_index length = x->base_string.fillp;
		y = cl_alloc_simple_base_string(length);
		memcpy(y->base_string.self, x->base_string.self, length);
		break;
	}
	case t_list:
		if (Null(x)) {
			x = Cnil_symbol->symbol.name;
			goto AGAIN;
		}
	default:
		x = ecl_type_error(@'si::copy-to-simple-base-string',"",x,@'string');
		goto AGAIN;
	}
	@(return y)
}

cl_object
cl_string(cl_object x)
{
 AGAIN:
	switch (type_of(x)) {
	case t_symbol:
		x = x->symbol.name;
		break;
	case t_character: {
		cl_object y;
#ifdef ECL_UNICODE
		if (BASE_CHAR_P(x)) {
			y = cl_alloc_simple_base_string(1);
			y->base_string.self[0] = CHAR_CODE(x);
			x = y;
		} else {
			y = cl_alloc_simple_extended_string(1);
			y->string.self[0] = x;
			x = y;
		}
#else
		y = cl_alloc_simple_base_string(1);
		y->base_string.self[0] = CHAR_CODE(x);
		x = y;
		break;
#endif
	}
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string:
		break;
	case t_list:
		if (Null(x)) {
			x = Cnil_symbol->symbol.name;
			break;
		}
	default:
		x = ecl_type_error(@'string',"",x,@'string');
		goto AGAIN;
	}
	@(return x)
}

#ifdef ECL_UNICODE
cl_object
si_coerce_to_base_string(cl_object x)
{
	if (type_of(x) != t_base_string) {
		x = si_copy_to_simple_base_string(x);
	}
	@(return x)
}

cl_object
si_coerce_to_extended_string(cl_object x)
{
	cl_object y;
AGAIN:
	switch (type_of(x)) {
	case t_symbol:
		x = x->symbol.name;
		goto AGAIN;
	case t_character:
		y = cl_alloc_simple_extended_string(1);
		y->string.self[0] = x;
		break;
	case t_base_string: {
		cl_index index, len = x->base_string.dim;
		y = cl_alloc_simple_extended_string(x->base_string.fillp);
		for(index=0; index < len; index++) {
			y->string.self[index] = CODE_CHAR(x->base_string.self[index]);
		}
		y->string.fillp = x->base_string.fillp;
	}
	case t_string:
		y = x;
		break;
	case t_list:
		if (Null(x)) {
			x = Cnil_symbol->symbol.name;
			goto AGAIN;
		}
	default:
		x = ecl_type_error(@'si::coerce-to-extended-string',"",x,@'string');
		goto AGAIN;
	}
	@(return y)
}
#endif

cl_object
cl_char(cl_object object, cl_object index)
{
	cl_index position = ecl_to_index(index);
	@(return CODE_CHAR(ecl_char(object, position)))
}

cl_index
ecl_char(cl_object object, cl_index index)
{
	/* CHAR bypasses fill pointers when accessing strings */
 AGAIN:
	switch(type_of(object)) {
#ifdef ECL_UNICODE
	case t_string:
		if (index >= object->string.dim)
			FEillegal_index(object, MAKE_FIXNUM(index));
		return CHAR_CODE(object->string.self[index]);
#endif
	case t_base_string:
		if (index >= object->base_string.dim)
			FEillegal_index(object, MAKE_FIXNUM(index));
		return object->base_string.self[index];
	default:
		object = ecl_type_error(@'char',"",object,@'string');
		goto AGAIN;
	}
}

cl_object
si_char_set(cl_object object, cl_object index, cl_object value)
{
	cl_index position = ecl_to_index(index);
	cl_index c = ecl_char_code(value);
	ecl_char_set(object, position, c);
	@(return value)
}

void
ecl_char_set(cl_object object, cl_index index, cl_index value)
{
 AGAIN:
	/* CHAR bypasses fill pointers when accessing strings */
	switch(type_of(object)) {
#ifdef ECL_UNICODE
	case t_string:
		if (index >= object->string.dim)
			FEillegal_index(object, MAKE_FIXNUM(index));
		object->string.self[index] = CODE_CHAR(value);
		break;
#endif
	case t_base_string:
		if (index >= object->base_string.dim)
			FEillegal_index(object, MAKE_FIXNUM(index));
		/* INV: ecl_char_code() checks type of value */
		object->base_string.self[index] = value;
		break;
	default:
		object = ecl_type_error(@'si::char-set', "", object, @'string');
		goto AGAIN;
	}
}

void
get_string_start_end(cl_object string, cl_object start, cl_object end,
		     cl_index *ps, cl_index *pe)
{
	/* INV: works on both t_base_string and t_string */
	/* INV: Works with either string or symbol */
	if (!FIXNUMP(start) || FIXNUM_MINUSP(start))
		goto E;
	else
		*ps = fix(start);
	if (Null(end)) {
		*pe = string->vector.fillp;
		if (*pe < *ps)
			goto E;
	} else if (!FIXNUMP(end) || FIXNUM_MINUSP(end))
		goto E;
	else {
		*pe = fix(end);
		if (*pe < *ps || *pe > string->vector.fillp)
			goto E;
	}
	return;

E:
	FEerror("~S and ~S are illegal as :START and :END~%\
for the string designator ~S.", 3, start, end, string);
}

#ifdef ECL_UNICODE
static int
compare_strings(cl_object string1, cl_index s1, cl_index e1,
		cl_object string2, cl_index s2, cl_index e2,
		int case_sensitive, cl_index *m)
{
	cl_index c1, c2;
	for (; s1 < e1; s1++, s2++) {
		if (s2 >= e2) { /* s1 is longer than s2, therefore s2 < s1 */
			*m = s1;
			return +1;
		}
		c1 = ecl_char(string1, s1);
		c2 = ecl_char(string2, s2);
		if (!case_sensitive) {
			c1 = towupper(c1);
			c2 = towupper(c2);
		}
		if (c1 < c2) {
			*m = s1;
			return -1;
		} else if (c1 > c2) {
			*m = s1;
			return +1;
		}
	}
	*m = s1;
	if (s2 >= e2) {
		return 0;
	} else { /* s1 is shorter than s2, hence s1 < s2 */
		return -1;
	}
}
#endif

static int
compare_base(char *s1, cl_index l1, char *s2, cl_index l2, int case_sensitive, cl_index *m)
{
	cl_index l, c1, c2;
	for (l = 0; l < l1; l++, s1++, s2++) {
		if (l == l2) { /* s1 is longer than s2, therefore s2 < s1 */
			*m = l;
			return +1;
		}
		c1 = *s1;
		c2 = *s2;
		if (!case_sensitive) {
			c1 = toupper(c1);
			c2 = toupper(c2);
		}
		if (c1 < c2) {
			*m = l;
			return -1;
		} else if (c1 > c2) {
			*m = l;
			return +1;
		}
	}
	*m = l;
	if (l1 == l2) 
		return 0;
	else { /* s1 is shorter than s2, hence s1 < s2 */
		return -1;
	}
}

@(defun string= (string1 string2 &key (start1 MAKE_FIXNUM(0)) end1
		                      (start2 MAKE_FIXNUM(0)) end2)
	cl_index s1, e1, s2, e2;
@
  AGAIN:
	string1 = cl_string(string1);
	string2 = cl_string(string2);
	get_string_start_end(string1, start1, end1, &s1, &e1);
	get_string_start_end(string2, start2, end2, &s2, &e2);
	if (e1 - s1 != e2 - s2)
		@(return Cnil)
#ifdef ECL_UNICODE
	switch(type_of(string1)) {
	case t_string:
		switch(type_of(string2)) {
		case t_string:
			while (s1 < e1)
				if (string1->string.self[s1++] != string2->string.self[s2++])
					@(return Cnil)
			@(return Ct)
		case t_base_string:
			while (s1 < e1)
				if (CHAR_CODE(string1->string.self[s1++]) != string2->base_string.self[s2++])
					@(return Cnil)
			@(return Ct)
		}
		break;
	case t_base_string:
		switch(type_of(string2)) {
		case t_string:
			while (s1 < e1)
				if (string1->base_string.self[s1++] != CHAR_CODE(string2->string.self[s2++]))
					@(return Cnil)
			@(return Ct)
		case t_base_string:
			while (s1 < e1)
				if (string1->base_string.self[s1++] != string2->base_string.self[s2++])
					@(return Cnil)
			@(return Ct)
		}
		break;
 	}
#else
	while (s1 < e1)
		if (string1->base_string.self[s1++] !=
		    string2->base_string.self[s2++])
			@(return Cnil)
#endif
	@(return Ct)
@)

/*
	This correponds to string= (just the string equality).
*/
bool
ecl_string_eq(cl_object x, cl_object y)
{
	cl_index i, j;
 AGAIN:
	i = x->base_string.fillp;
	j = y->base_string.fillp;
	if (i != j) return 0;
#ifdef ECL_UNICODE
	switch(type_of(x)) {
	case t_string:
		switch(type_of(y)) {
		case t_string:
			return memcmp(x->string.self, y->string.self, i * sizeof *x->string.self) == 0;
		case t_base_string: {
			cl_index index;
			for(index=0; index<i; index++)
				if (x->string.self[index] != CODE_CHAR(y->base_string.self[index]))
					return 0;
			return 1;
			}
		default:
			y = ecl_type_error(@'string=',"",y,@'string');
			goto AGAIN;
		}
		break;
	case t_base_string:
		switch(type_of(y)) {
		case t_string:
			return ecl_string_eq(y, x);
		case t_base_string:
			return memcmp(x->base_string.self, y->base_string.self, i) == 0;
		default:
			y = ecl_type_error(@'string=',"",y,@'string');
			goto AGAIN;
		}
		break;
	default:
		x = ecl_type_error(@'string=',"",x,@'string');
		goto AGAIN;
	}
#else
	return memcmp(x->base_string.self, y->base_string.self, i) == 0;
#endif
}


@(defun string_equal (string1 string2 &key (start1 MAKE_FIXNUM(0)) end1
		                           (start2 MAKE_FIXNUM(0)) end2)
	cl_index s1, e1, s2, e2;
	int output;
@
AGAIN:
	string1 = cl_string(string1);
	string2 = cl_string(string2);
	get_string_start_end(string1, start1, end1, &s1, &e1);
	get_string_start_end(string2, start2, end2, &s2, &e2);
	if (e1 - s1 != e2 - s2)
		@(return Cnil);
#ifdef ECL_UNICODE
	if (type_of(string1) != t_base_string || type_of(string2) != t_base_string) {
		output = compare_strings(string1, s1, e1, string2, s2, e2, 0, &e1);
	} else
#endif
	output = compare_base(string1->base_string.self + s1, e1 - s1,
			      string2->base_string.self + s2, e2 - s2,
			      0, &e1);
	@(return ((output == 0)? Ct : Cnil))
@)

static cl_object
string_compare(cl_narg narg, int sign1, int sign2, int case_sensitive, cl_va_list ARGS)
{
	cl_object string1 = cl_va_arg(ARGS);
	cl_object string2 = cl_va_arg(ARGS);
	cl_index s1, e1, s2, e2;
	int output;
	cl_object result;
	cl_object KEYS[4];
#define start1 KEY_VARS[0]
#define end1 KEY_VARS[1]
#define start2 KEY_VARS[2]
#define end2 KEY_VARS[3]
#define start1p KEY_VARS[4]
#define start2p KEY_VARS[6]
	cl_object KEY_VARS[8];

	if (narg < 2) FEwrong_num_arguments_anonym();
	KEYS[0]=@':start1';
	KEYS[1]=@':end1';
	KEYS[2]=@':start2';
	KEYS[3]=@':end2';
	cl_parse_key(ARGS, 4, KEYS, KEY_VARS, NULL, FALSE);

	string1 = cl_string(string1);
	string2 = cl_string(string2);
	if (start1p == Cnil) start1 = MAKE_FIXNUM(0);
	if (start2p == Cnil) start2 = MAKE_FIXNUM(0);
	get_string_start_end(string1, start1, end1, &s1, &e1);
	get_string_start_end(string2, start2, end2, &s2, &e2);
#ifdef ECL_UNICODE
	if (type_of(string1) != t_base_string || type_of(string2) != t_base_string) {
		output = compare_strings(string1, s1, e1, string2, s2, e2,
					 case_sensitive, &e1);
	} else
#endif
	{
		output = compare_base(string1->base_string.self + s1, e1 - s1,
				      string2->base_string.self + s2, e2 - s2,
				      case_sensitive, &e1);
		e1 += s1;
	}
	if (output == sign1 || output == sign2) {
		result = MAKE_FIXNUM(e1);
	} else {
		result = Cnil;
	}
	@(return result)
#undef start1p
#undef start2p
#undef start1
#undef end1
#undef start2
#undef end2
}

@(defun string< (&rest args)
@
	return string_compare(narg, -1, -1, 1, args);
@)

@(defun string> (&rest args)
@
	return string_compare(narg, +1, +1, 1, args);
@)

@(defun string<= (&rest args)
@
	return string_compare(narg, -1, 0, 1, args);
@)

@(defun string>= (&rest args)
@
	return string_compare(narg, 0, +1, 1, args);
@)

@(defun string/= (&rest args)
@
	return string_compare(narg, -1, +1, 1, args);
@)

@(defun string-lessp (&rest args)
@
	return string_compare(narg, -1, -1, 0, args);
@)

@(defun string-greaterp (&rest args)
@
	return string_compare(narg, +1, +1, 0, args);
@)

@(defun string-not-greaterp (&rest args)
@
	return string_compare(narg, -1, 0, 0, args);
@)

@(defun string-not-lessp (&rest args)
@
	return string_compare(narg, 0, +1, 0, args);
@)

@(defun string-not-equal (&rest args)
@
	return string_compare(narg, -1, +1, 0, args);
@)

bool
ecl_member_char(int c, cl_object char_bag)
{
	cl_index i, f;
 AGAIN:
	switch (type_of(char_bag)) {
	case t_list:
		loop_for_in(char_bag) {
			cl_object other = CAR(char_bag);
			if (CHARACTERP(other) && c == CHAR_CODE(other))
				return(TRUE);
		} end_loop_for_in;
		return(FALSE);
	case t_vector:
		for (i = 0, f = char_bag->vector.fillp;  i < f;  i++) {
			cl_object other = char_bag->vector.self.t[i];
			if (CHARACTERP(other) && c == CHAR_CODE(other))
				return(TRUE);
		}
		return(FALSE);
#ifdef ECL_UNICODE
	case t_string:
		for (i = 0, f = char_bag->string.fillp;  i < f;  i++) {
			if (c == CHAR_CODE(char_bag->string.self[i]))
				return(TRUE);
		}
		return(FALSE);
#endif
	case t_base_string:
		for (i = 0, f = char_bag->base_string.fillp;  i < f;  i++) {
			if (c == char_bag->base_string.self[i])
				return(TRUE);
		}
		return(FALSE);
	case t_bitvector:
		return(FALSE);
	default:
		char_bag = ecl_type_error(@'member',"",char_bag,@'sequence');
		goto AGAIN;
	}
}

static cl_object
string_trim0(bool left_trim, bool right_trim, cl_object char_bag, cl_object strng)
{
	cl_index i, j;

	strng = cl_string(strng);
	i = 0;
	j = ecl_length(strng);
	if (left_trim) {
		for (;  i < j;  i++) {
			cl_index c = ecl_char(strng, i);
			if (!ecl_member_char(c, char_bag))
				break;
		}
	}
	if (right_trim) {
		for (; j > i; j--) {
			cl_index c = ecl_char(strng, j-1);
			if (!ecl_member_char(c, char_bag)) {
				break;
			}
		}
	}
	return cl_subseq(3, strng, MAKE_FIXNUM(i), MAKE_FIXNUM(j));
}

cl_object
cl_string_trim(cl_object char_bag, cl_object strng)
{
	return string_trim0(TRUE, TRUE, char_bag, strng);
}

cl_object
cl_string_left_trim(cl_object char_bag, cl_object strng)
{
	return string_trim0(TRUE, FALSE, char_bag, strng);
}

cl_object
cl_string_right_trim(cl_object char_bag, cl_object strng)
{
	return string_trim0(FALSE, TRUE, char_bag, strng);
}

static cl_object
string_case(cl_narg narg, int (*casefun)(int c, bool *bp), cl_va_list ARGS)
{
	cl_object strng = cl_va_arg(ARGS);
	cl_index s, e, i;
	bool b;
	cl_object KEYS[2];
#define start KEY_VARS[0]
#define end KEY_VARS[1]
#define startp KEY_VARS[2]
	cl_object conv;
	cl_object KEY_VARS[4];

	if (narg < 1) FEwrong_num_arguments_anonym();
	KEYS[0]=@':start';
	KEYS[1]=@':end';
	cl_parse_key(ARGS, 2, KEYS, KEY_VARS, NULL, FALSE);

	strng = cl_string(strng);
	conv  = cl_copy_seq(strng);
	if (startp == Cnil)
		start = MAKE_FIXNUM(0);
	get_string_start_end(conv, start, end, &s, &e);
	b = TRUE;
#ifdef ECL_UNICODE
	switch(type_of(conv)) {
	case t_string:
		for (i = s;  i < e;  i++)
			conv->string.self[i] = CODE_CHAR((*casefun)(CHAR_CODE(conv->string.self[i]), &b));
		break;
	case t_base_string:
		for (i = s;  i < e;  i++)
			conv->base_string.self[i] = (*casefun)(conv->base_string.self[i], &b);
		break;
	}
#else
	for (i = s;  i < e;  i++)
		conv->base_string.self[i] = (*casefun)(conv->base_string.self[i], &b);
#endif
	@(return conv)
#undef startp
#undef start
#undef end
}

static int
char_upcase(int c, bool *bp)
{
	return(toupper(c));
}

@(defun string-upcase (&rest args)
@
	return string_case(narg, char_upcase, args);
@)

static int
char_downcase(int c, bool *bp)
{
	return tolower(c);
}

@(defun string-downcase (&rest args)
@
	return string_case(narg, char_downcase, args);
@)

static int
char_capitalize(int c, bool *bp)
{
	if (islower(c)) {
		if (*bp)
			c = toupper(c);
		*bp = FALSE;
	} else if (isupper(c)) {
		if (!*bp)
			c = tolower(c);
		*bp = FALSE;
	} else {
		*bp = !isdigit(c);
	}
	return(c);
}

@(defun string-capitalize (&rest args)
@
	return string_case(narg, char_capitalize, args);
@)


static cl_object
nstring_case(cl_narg narg, cl_object fun, int (*casefun)(int, bool *), cl_va_list ARGS)
{
	cl_object strng = cl_va_arg(ARGS);
	cl_index s, e, i;
	bool b;
	cl_object KEYS[2];
#define start KEY_VARS[0]
#define end KEY_VARS[1]
#define startp KEY_VARS[2]
	cl_object KEY_VARS[4];

	if (narg < 1) FEwrong_num_arguments_anonym();
	KEYS[0]=@':start';
	KEYS[1]=@':end';
	cl_parse_key(ARGS, 2, KEYS, KEY_VARS, NULL, FALSE);

	strng = ecl_check_type_string(fun,strng);
	if (startp == Cnil) start = MAKE_FIXNUM(0);
	get_string_start_end(strng, start, end, &s, &e);
	b = TRUE;
#ifdef ECL_UNICODE
	if (type_of(strng) == t_string) {
		for (i = s;  i < e;  i++)
			strng->string.self[i] = CODE_CHAR((*casefun)(CHAR_CODE(strng->string.self[i]), &b));
	} else {
		for (i = s;  i < e;  i++)
			strng->base_string.self[i] = (*casefun)(strng->base_string.self[i], &b);
	}
#else
	for (i = s;  i < e;  i++)
		strng->base_string.self[i] = (*casefun)(strng->base_string.self[i], &b);
#endif
	@(return strng)
#undef startp
#undef start
#undef end
}

@(defun nstring-upcase (&rest args)
@
	return nstring_case(narg, @'nstring-upcase', char_upcase, args);
@)

@(defun nstring-downcase (&rest args)
@
	return nstring_case(narg, @'nstring-downcase', char_downcase, args);
@)

@(defun nstring-capitalize (&rest args)
@
	return nstring_case(narg, @'nstring-capitalize', char_capitalize, args);
@)

@(defun si::base_string_concatenate (&rest args)
	cl_index l;
	int i;
	cl_object output;
@
	/* Compute final size and store NONEMPTY coerced strings. */
	for (i = 0, l = 0; i < narg; i++) {
		cl_object s = si_coerce_to_base_string(cl_va_arg(args));
		if (s->base_string.fillp) {
			cl_stack_push(s);
			l += s->base_string.fillp;
		}
	}
	/* Do actual copying by recovering those strings */
	output = cl_alloc_simple_base_string(l);
	while (l) {
		cl_object s = cl_stack_pop();
		size_t bytes = s->base_string.fillp;
		l -= bytes;
		memcpy(output->base_string.self + l, s->base_string.self, bytes);
	}
	@(return output);
@)

int
ecl_string_push_extend(cl_object s, int c)
{
 AGAIN:
	switch(type_of(s)) {
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string:
		/* We use the fact that both string types are
		   byte-compatible except for the data. */
		if (s->base_string.fillp >= s->base_string.dim) {
			cl_object other;
			cl_index new_length;
			if (!s->base_string.adjustable)
				FEerror("string-push-extend: the string ~S is not adjustable.",
					1, s);
			if (s->base_string.dim >= ADIMLIM)
				FEerror("Can't extend the string.", 0);
			new_length = 1 + s->base_string.dim + (s->base_string.dim / 2);
			if (new_length > ADIMLIM)
				new_length = ADIMLIM;
			other = si_make_vector(cl_array_element_type(s),
					       MAKE_FIXNUM(new_length), Ct,
					       MAKE_FIXNUM(s->base_string.fillp),
					       Cnil, MAKE_FIXNUM(0));
			ecl_copy_subarray(other, 0, s, 0, s->base_string.fillp);
			s = si_replace_array(s, other);
		}
		ecl_char_set(s, s->base_string.fillp++, c);
		return c;
	default:
		s = ecl_type_error(@'vector-push-extend',"",s,@'string');
		goto AGAIN;
	}
}
