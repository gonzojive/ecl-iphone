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


#include "ecl.h"
#include <ctype.h>
#include <string.h>
#include "ecl-inl.h"

@(defun make_string (size &key (initial_element CODE_CHAR(' '))
		     (element_type @'character')
		     &aux x)
	cl_index i, s, code;
@
	if (element_type != @'character'
	    && element_type != @'base-char'
	    && element_type != @'standard-char') {
	  if (funcall(2, @'subtypep', element_type, @'character') == Cnil)
	    FEerror("The type ~S is not a valid string char type.",
		    1, element_type);
	}
	/* INV: char_code() checks the type of initial_element() */
	code = char_code(initial_element);
	s = object_to_index(size);
	x = cl_alloc_simple_string(s);
	for (i = 0;  i < s;  i++)
		x->string.self[i] = code;
	@(return x)
@)

cl_object
cl_alloc_simple_string(cl_index l)
{
	cl_object x;

	x = cl_alloc_object(t_string);
	x->string.hasfillp = FALSE;
	x->string.adjustable = FALSE;
	x->string.displaced = Cnil;
	x->string.dim = (x->string.fillp = l);
	x->string.self = (char *)cl_alloc_atomic(l+1);
	x->string.self[l] = x->string.self[0] = 0;
	return(x);
}

/*
	Make a string of a certain size, with some eading zeros to
	keep C happy. The string must be adjustable, to allow further
	growth. (See unixfsys.c for its use).
*/
cl_object
cl_alloc_adjustable_string(cl_index l)
{
	cl_object output = cl_alloc_simple_string(l);
	output->string.fillp = 0;
	output->string.hasfillp = TRUE;
 	output->string.adjustable = TRUE;
	return output;
}

/*
	Make_simple_string(s) makes a simple string from C string s.
*/
cl_object
make_simple_string(char *s)
{
	cl_object x;
	cl_index l = strlen(s);

	x = cl_alloc_object(t_string);
	x->string.hasfillp = FALSE;
	x->string.adjustable = FALSE;
	x->string.displaced = Cnil;
	x->string.dim = (x->string.fillp = l);
	x->string.self = s;
	
	return(x);
}

cl_object
make_string_copy(const char *s)
{
	cl_object x;
	cl_index l = strlen(s);

	x = cl_alloc_simple_string(l);
	memcpy(x->string.self, s, l);
	return(x);
}


/*
	Copy_simple_string(x) copies string x to a simple string.
*/
cl_object
copy_simple_string(cl_object x)
{
	cl_object y;
	cl_index l = x->string.fillp;

	y = cl_alloc_simple_string(l);
	memcpy(y->string.self, x->string.self, l);
	return(y);
}

cl_object
coerce_to_simple_string(cl_object x)
{
	assert_type_string(x);
	return x->string.adjustable? copy_simple_string(x) : x;
}

cl_object
cl_string(cl_object x)
{
	cl_object y;

	switch (type_of(x)) {
	case t_symbol:
		return1(x->symbol.name);

	case t_character:
		y = cl_alloc_simple_string(1);
		y->string.self[0] = CHAR_CODE(x);
		return1(y);

	case t_string:
		return1(x);

	default:
		FEtype_error_string(x);
	}
}

cl_object
cl_char(cl_object s, cl_object i)
{
	cl_index j;

	assert_type_string(s);
	j = object_to_index(i);
	/* CHAR bypasses fill pointers when accessing strings */
	if (j >= s->string.dim)
		illegal_index(s, i);
	@(return CODE_CHAR(s->string.self[j]))
}

cl_object
si_char_set(cl_object str, cl_object index, cl_object c)
{
	cl_index j;

	assert_type_string(str);
	j = object_to_index(index);
	/* CHAR bypasses fill pointers when accessing strings */
	if (j >= str->string.dim)
		illegal_index(str, index);
	/* INV: char_code() checks type of `c' */
	str->string.self[j] = char_code(c);
	@(return c)
}

void
get_string_start_end(cl_object string, cl_object start, cl_object end,
		     cl_index *ps, cl_index *pe)
{
	/* INV: Works with either string or symbol */
	if (!FIXNUMP(start) || FIXNUM_MINUSP(start))
		goto E;
	else
		*ps = fix(start);
	if (Null(end)) {
		*pe = string->string.fillp;
		if (*pe < *ps)
			goto E;
	} else if (!FIXNUMP(end) || FIXNUM_MINUSP(end))
		goto E;
	else {
		*pe = fix(end);
		if (*pe < *ps || *pe > string->string.fillp)
			goto E;
	}
	return;

E:
	FEerror("~S and ~S are illegal as :START and :END~%\
for the string designator ~S.", 3, start, end, string);
}

@(defun string= (string1 string2 &key (start1 MAKE_FIXNUM(0)) end1
		   (start2 MAKE_FIXNUM(0)) end2)
	cl_index s1, e1, s2, e2;
@
	string1 = cl_string(string1);
	string2 = cl_string(string2);
	get_string_start_end(string1, start1, end1, &s1, &e1);
	get_string_start_end(string2, start2, end2, &s2, &e2);
	if (e1 - s1 != e2 - s2)
		@(return Cnil)
	while (s1 < e1)
		if (string1->string.self[s1++] !=
		    string2->string.self[s2++])
			@(return Cnil)
	@(return Ct)
@)

/*
	This correponds to string= (just the string equality).
*/
bool
string_eq(cl_object x, cl_object y)
{
	/* INV: Works with either a symbol or a string */
	cl_index i, j;
	i = x->string.fillp;
	j = y->string.fillp;
	return (i == j && memcmp(x->string.self, y->string.self, i) == 0);
}


@(defun string_equal (string1 string2 &key (start1 MAKE_FIXNUM(0)) end1
		      (start2 MAKE_FIXNUM(0)) end2)
	cl_index s1, e1, s2, e2;
	cl_index i1, i2;
@
	string1 = cl_string(string1);
	string2 = cl_string(string2);
	get_string_start_end(string1, start1, end1, &s1, &e1);
	get_string_start_end(string2, start2, end2, &s2, &e2);
	if (e1 - s1 != e2 - s2)
		@(return Cnil)
	while (s1 < e1) {
		i1 = string1->string.self[s1++];
		i2 = string2->string.self[s2++];
		if (toupper(i1) != toupper(i2))
			@(return Cnil)
	}
	@(return Ct)
@)

/*
	This corresponds to string-equal
	(string equality ignoring the case).
*/
bool
string_equal(cl_object x, cl_object y)
{
	cl_index i, j;
	register char *p, *q;

	/* INV: Works with symbols ands strings */
	i = x->string.fillp;
	j = y->string.fillp;
	if (i != j)
		return(FALSE);
	p = x->string.self;
	q = y->string.self;
	for (i = 0;  i < j;  i++)
		if (toupper(p[i]) != toupper(q[i]))
			return(FALSE);
	return(TRUE);
}

static cl_return
string_cmp(int narg, int sign, int boundary, cl_va_list ARGS)
{
	cl_object string1 = cl_va_arg(ARGS);
	cl_object string2 = cl_va_arg(ARGS);
	cl_index s1, e1, s2, e2;
	int s, i1, i2;
	cl_object KEYS[4];
#define start1 KEY_VARS[0]
#define end1 KEY_VARS[1]
#define start2 KEY_VARS[2]
#define end2 KEY_VARS[3]
#define start1p KEY_VARS[4]
#define start2p KEY_VARS[6]
	cl_object KEY_VARS[8];

	if (narg < 2) FEtoo_few_arguments(narg);
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
	while (s1 < e1) {
		if (s2 == e2)
		  return1(sign>0 ? Cnil : MAKE_FIXNUM(s1));
		i1 = string1->string.self[s1];
		i2 = string2->string.self[s2];
		if (sign == 0) {
			if (i1 != i2)
			  return1(MAKE_FIXNUM(s1));
		} else {
			s = sign*(i2-i1);
			if (s > 0)
			  return1(MAKE_FIXNUM(s1));
			if (s < 0)
			  return1(Cnil);
		}
		s1++;
		s2++;
	}
	if (s2 == e2)
	  return1(boundary==0 ? MAKE_FIXNUM(s1) : Cnil);
	else
	  return1(sign>=0 ? MAKE_FIXNUM(s1) : Cnil);
#undef start1p
#undef start2p
#undef start1
#undef end1
#undef start2
#undef end2
}

@(defun string< (&rest args)
@
	@(return string_cmp(narg, 1, 1, args))
@)

@(defun string> (&rest args)
@
	@(return string_cmp(narg,-1, 1, args))
@)

@(defun string<= (&rest args)
@
	@(return string_cmp(narg, 1, 0, args))
@)

@(defun string>= (&rest args)
@
	@(return string_cmp(narg,-1, 0, args))
@)

@(defun string/= (&rest args)
@
	@(return string_cmp(narg, 0, 1, args))
@)

static cl_return
string_compare(int narg, int sign, int boundary, cl_va_list ARGS)
{
	cl_object string1 = cl_va_arg(ARGS);
	cl_object string2 = cl_va_arg(ARGS);
	cl_index s1, e1, s2, e2;
	int i1, i2, s;

	cl_object KEYS[4];
#define start1 KEY_VARS[0]
#define end1 KEY_VARS[1]
#define start2 KEY_VARS[2]
#define end2 KEY_VARS[3]
#define start1p KEY_VARS[4]
#define start2p KEY_VARS[6]
	cl_object KEY_VARS[8];

	if (narg < 2) FEtoo_few_arguments(narg);
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
	while (s1 < e1) {
		if (s2 == e2)
		  return1(sign>0 ? Cnil : MAKE_FIXNUM(s1));
		i1 = string1->string.self[s1];
		i1 = toupper(i1);
		i2 = string2->string.self[s2];
		i2 = toupper(i2);
		if (sign == 0) {
			if (i1 != i2)
			  return1(MAKE_FIXNUM(s1));
		} else {
			s = sign*(i2-i1);
			if (s > 0)
			  return1(MAKE_FIXNUM(s1));
			if (s < 0)
			  return1(Cnil);
		}
		s1++;
		s2++;
	}
	if (s2 == e2)
		return1(boundary==0 ? MAKE_FIXNUM(s1) : Cnil);
	else
		return1(sign>=0 ? MAKE_FIXNUM(s1) : Cnil);
#undef start1p
#undef start2p
#undef start1
#undef end1
#undef start2
#undef end2
}

@(defun string-lessp (&rest args)
@
	@(return string_compare(narg, 1, 1, args))
@)

@(defun string-greaterp (&rest args)
@
	@(return string_compare(narg,-1, 1, args))
@)

@(defun string-not-greaterp (&rest args)
@
	@(return string_compare(narg, 1, 0, args))
@)

@(defun string-not-lessp (&rest args)
@
	@(return string_compare(narg,-1, 0, args))
@)

@(defun string-not-equal (&rest args)
@
	@(return string_compare(narg, 0, 1, args))
@)

bool
member_char(int c, cl_object char_bag)
{
	cl_index i, f;

	switch (type_of(char_bag)) {
	case t_cons:
		loop_for_in(char_bag) {
			cl_object other = CAR(char_bag);
			if (CHARACTERP(other) && c == CHAR_CODE(other))
				return(TRUE);
			char_bag = CDR(char_bag);
		} end_loop_for_in;
		return(FALSE);

	case t_vector:
		for (i = 0, f = char_bag->vector.fillp;  i < f;  i++) {
			cl_object other = char_bag->vector.self.t[i];
			if (CHARACTERP(other) && c == CHAR_CODE(other))
				return(TRUE);
		}
		return(FALSE);

	case t_string:
		for (i = 0, f = char_bag->string.fillp;  i < f;  i++) {
			if (c == char_bag->string.self[i])
				return(TRUE);
		}
		return(FALSE);

	case t_bitvector:
		return(FALSE);

	default:
		FEerror("~S is not a sequence.", 1, char_bag);
	}
}

static cl_return
string_trim0(bool left_trim, bool right_trim, cl_object char_bag, cl_object strng)
{
	cl_object res;
	cl_index i, j, k;

	strng = cl_string(strng);
	i = 0;
	j = strng->string.fillp - 1;
	if (left_trim)
		for (;  i <= j;  i++)
			if (!member_char(strng->string.self[i], char_bag))
				break;
	if (right_trim)
		for (;  j >= i;  --j)
			if (!member_char(strng->string.self[j], char_bag))
				break;
	k = j - i + 1;
	res = cl_alloc_simple_string(k);
	memcpy(res->string.self, strng->string.self+i, k);
	return1(res);
}

cl_return
cl_string_trim(cl_object char_bag, cl_object strng)
	{ return string_trim0(TRUE, TRUE, char_bag, strng); }
cl_return
cl_string_left_trim(cl_object char_bag, cl_object strng)
	{ return string_trim0(TRUE, FALSE, char_bag, strng); }
cl_return
cl_string_right_trim(cl_object char_bag, cl_object strng)
	{ return string_trim0(FALSE, TRUE, char_bag, strng);}


static cl_return
string_case(int narg, int (*casefun)(int c, bool *bp), cl_va_list ARGS)
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

	if (narg < 1) FEtoo_few_arguments(narg);
	KEYS[0]=@':start';
	KEYS[1]=@':end';
	cl_parse_key(ARGS, 2, KEYS, KEY_VARS, NULL, FALSE);

	strng = cl_string(strng);
	conv = copy_simple_string(strng);
	if (startp == Cnil) start = MAKE_FIXNUM(0);
	get_string_start_end(conv, start, end, &s, &e);
	b = TRUE;
	for (i = s;  i < e;  i++)
		conv->string.self[i] = (*casefun)(conv->string.self[i], &b);
	return1(conv);
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
	@(return string_case(narg, char_upcase, args))
@)

static int
char_downcase(int c, bool *bp)
{
	return(tolower(c));
}

@(defun string-downcase (&rest args)
@
	@(return string_case(narg, char_downcase, args))
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
	} else if (!isdigit(c))
		*bp = TRUE;
	return(c);
}

@(defun string-capitalize (&rest args)
@
	@(return string_case(narg, char_capitalize, args))
@)


static cl_return
nstring_case(int narg, int (*casefun)(int, bool *), cl_va_list ARGS)
{
	cl_object strng = cl_va_arg(ARGS);
	cl_index s, e, i;
	bool b;
	cl_object KEYS[2];
#define start KEY_VARS[0]
#define end KEY_VARS[1]
#define startp KEY_VARS[2]
	cl_object KEY_VARS[4];

	if (narg < 1) FEtoo_few_arguments(narg);
	KEYS[0]=@':start';
	KEYS[1]=@':end';
	cl_parse_key(ARGS, 2, KEYS, KEY_VARS, NULL, FALSE);

	assert_type_string(strng);
	if (startp == Cnil) start = MAKE_FIXNUM(0);
	get_string_start_end(strng, start, end, &s, &e);
	b = TRUE;
	for (i = s;  i < e;  i++)
		strng->string.self[i] = (*casefun)(strng->string.self[i], &b);
	return1(strng);
#undef startp
#undef start
#undef end
}

@(defun nstring-upcase (&rest args)
@
	@(return nstring_case(narg, char_upcase, args))
@)

@(defun nstring-downcase (&rest args)
@
	@(return nstring_case(narg, char_downcase, args))
@)

@(defun nstring-capitalize (&rest args)
@
	@(return nstring_case(narg, char_capitalize, args))
@)

@(defun si::string_concatenate (&rest args)
	cl_index l;
	int i;
	cl_object v, strings[narg];
	char *vself;
@
	for (i = 0, l = 0;  i < narg;  i++) {
		strings[i] = cl_string(cl_va_arg(args));
		l += strings[i]->string.fillp;
	}
	v = cl_alloc_simple_string(l);
	for (i = 0, vself = v->string.self;  i < narg;  i++, vself += l) {
		l = strings[i]->string.fillp;
		memcpy(vself, strings[i]->string.self, l);
	}
	@(return v)
@)

int
cl_string_push_extend(cl_object s, int c)
{
	char *p;
	cl_index new_length;

	if (type_of(s) != t_string) {
		FEtype_error_string(s);
	} else if (s->string.fillp >= s->string.dim) {
		if (!s->string.adjustable)
			FEerror("string-push-extend: the string ~S is not adjustable.",
				1, s);
#ifdef THREADS
		start_critical_section(); /* avoid losing p */
#endif
		if (s->string.dim >= ADIMLIM/2)
			FEerror("Can't extend the string.", 0);
		new_length = s->string.dim * 2;
		p = (char *)cl_alloc(new_length+1); p[new_length] = 0;
		memcpy(p, s->string.self, s->string.dim * sizeof(char));
		s->string.dim = new_length;
		adjust_displaced(s, p - s->string.self);
#ifdef THREADS
		end_critical_section();
#endif
	}
	s->string.self[s->string.fillp++] = c;
	return c;
}
