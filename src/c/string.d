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
#include <string.h>
#include <ecl/ecl-inl.h>


#ifdef ECL_UNICODE
/* TODO: add a special variable to allow make-string to default to base-char rather than character. */
/* should be @'character' -- FIXME */

@(defun make_string (size &key (initial_element CODE_CHAR(' '))
		     (element_type @'character')
		     &aux x)
	cl_index i, s, code;
@
	/* INV: char_code() checks the type of initial_element() */
	code = char_code(initial_element);
	s = object_to_index(size);

	/* this code should use subtypep */
	/* handle base-char strings */
	if (element_type == @'base-char' || element_type == @'standard-char') {
		x = cl_alloc_simple_base_string(s);
		for (i = 0;  i < s;  i++)
			x->base_string.self[i] = code;
		@(return x)
	}

	if (element_type != @'character'
            && (funcall(3, @'subtypep', element_type, @'character') == Cnil))
	    FEerror("The type ~S is not a valid string char type.", 1, element_type);

	x = cl_alloc_simple_extended_string(s);
	for (i = 0;  i < s;  i++)
		x->string.self[i] = CODE_CHAR(code);
	@(return x)
@)
#else
@(defun make_string (size &key (initial_element CODE_CHAR(' '))
		     (element_type @'character')
		     &aux x)
	cl_index i, s, code;
@
	if (element_type != @'character'
	    && element_type != @'base-char'
	    && element_type != @'standard-char') {
	  if (funcall(3, @'subtypep', element_type, @'character') == Cnil)
	    FEerror("The type ~S is not a valid string char type.",
		    1, element_type);
	}
	/* INV: char_code() checks the type of initial_element() */
	code = char_code(initial_element);
	s = object_to_index(size);
	x = cl_alloc_simple_base_string(s);
	for (i = 0;  i < s;  i++)
		x->base_string.self[i] = code;
	@(return x)
@)
#endif

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


/*
	Copy_simple_base_string(x) copies string x to a simple base-string.
*/
cl_object
copy_simple_base_string(cl_object x)
{
	cl_object y;
	cl_index l = x->base_string.fillp;

	y = cl_alloc_simple_base_string(l);
	memcpy(y->base_string.self, x->base_string.self, l);
	return(y);
}

#ifdef ECL_UNICODE
cl_object
copy_simple_string(cl_object x)
{
	cl_object y;
	cl_index length = x->vector.fillp;

	switch(type_of(x)) {
	case t_string:
		y = cl_alloc_simple_extended_string(length);
		memcpy(y->string.self, x->string.self, length * sizeof (cl_object));
		return(y);
	case t_base_string:
		y = cl_alloc_simple_base_string(length);
		memcpy(y->base_string.self, x->base_string.self, length);
		return(y);
	}
}
#endif

#ifdef ECL_UNICODE
cl_object
coerce_to_simple_base_string(cl_object source)
{
AGAIN:
	switch(type_of(source)) {
	case t_string: {
		cl_index  index;
		cl_index  length      = source->string.fillp;
		cl_object destination = cl_alloc_simple_base_string(length);
		for(index=0; index<length; index++) {
			/* this will smash extended-chars arbitrarily ... checkme */
			destination->base_string.self[index] = CHAR_CODE(source->string.self[index]);
			}
		return destination;
		}
	case t_base_string:
		return source->base_string.adjustable? copy_simple_base_string(source) : source;
	case t_symbol:
		source = source->symbol.name;
		goto AGAIN;
	default:
		FEtype_error_string(source);
	}
}

cl_object
coerce_to_simple_extended_string(cl_object source)
{
AGAIN:
	switch(type_of(source)) {
	case t_string:
		return source->string.adjustable? copy_simple_string(source) : source;
	case t_base_string: {
		cl_index  index;
		cl_index  length      = source->string.fillp;
		cl_object destination = cl_alloc_simple_extended_string(length);
		for(index=0; index<length; index++) {
			/* this will smash extended-chars arbitrarily ... checkme */
			destination->string.self[index] = CODE_CHAR(source->base_string.self[index]);
			}
		return destination;
		}
	case t_symbol:
		source = source->symbol.name;
		goto AGAIN;
	default:
		FEtype_error_string(source);
	}
}

cl_object
coerce_to_simple_string(cl_object source)
{
AGAIN:
	switch(type_of(source)) {
	case t_string:
		return source->base_string.adjustable? copy_simple_string(source) : source;
	case t_base_string:
		return source->base_string.adjustable? copy_simple_base_string(source) : source;
	case t_symbol:
		source = source->symbol.name;
		goto AGAIN;
	default:
		FEtype_error_string(source);
	}
}
#else
cl_object
coerce_to_simple_base_string(cl_object source)
{
AGAIN:
	switch(type_of(source)) {
	case t_base_string:
		return source->base_string.adjustable? copy_simple_base_string(source) : source;
	case t_symbol:
		source = source->symbol.name;
		goto AGAIN;
	default:
		FEtype_error_string(source);
	}
}

cl_object
coerce_to_simple_string(cl_object source)
{
AGAIN:
	switch(type_of(source)) {
	case t_string:
		return source->base_string.adjustable? copy_simple_string(source) : source;
	case t_base_string:
		return source->base_string.adjustable? copy_simple_base_string(source) : source;
	case t_symbol:
		source = source->symbol.name;
		goto AGAIN;
	default:
		FEtype_error_string(source);
	}
}
#endif

cl_object
cl_string(cl_object x)
{
	cl_object y;

	switch (type_of(x)) {
	case t_symbol:
		x = x->symbol.name;
		break;
	case t_character:
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
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string:
		break;
	default:
		FEtype_error_string(x);
	}
	@(return x)
}

#ifdef ECL_UNICODE
cl_object
cl_base_string(cl_object x)
{
	cl_object y;

	switch (type_of(x)) {
	case t_symbol:
		x = x->symbol.name;
		break;
	case t_character:
		/* truncates extended chars ... */
		y = cl_alloc_simple_base_string(1);
		y->base_string.self[0] = CHAR_CODE(x);
		x = y;
		break;
	case t_string: {
		cl_index index;
		y = cl_alloc_simple_base_string(x->string.fillp);
		for(index=0; index<x->string.fillp; index++)
			y->base_string.self[index] = CHAR_CODE(x->string.self[index]);
		x = y;
		}
	case t_base_string:
		break;
	default:
		FEtype_error_string(x);
	}
	@(return x)
}
#endif

#ifdef ECL_UNICODE
cl_object
cl_extended_string(cl_object x)
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
		x = y;
		break;
	case t_base_string: {
		cl_index index;
		y = cl_alloc_simple_extended_string(x->base_string.fillp);
		for(index=0; index<x->base_string.fillp; index++)
			y->string.self[index] = CODE_CHAR(x->base_string.self[index]);
		x = y;
		}
	case t_string:
		break;
	default:
		FEtype_error_string(x);
	}
	@(return x)
}
#endif

cl_object
cl_char(cl_object object, cl_object index)
{
	cl_index position = object_to_index(index);
	/* CHAR bypasses fill pointers when accessing strings */

	switch(type_of(object)) {
#ifdef ECL_UNICODE
	case t_string:
		if (position >= object->string.dim)
			illegal_index(object, index);
		@(return object->string.self[position])
#endif
	case t_base_string:
		if (position >= object->base_string.dim)
			illegal_index(object, index);
		@(return CODE_CHAR(object->base_string.self[position]))
	default:
		FEtype_error_string(object);
	}
}

cl_object
si_char_set(cl_object object, cl_object index, cl_object value)
{
	cl_index position = object_to_index(index);

	/* CHAR bypasses fill pointers when accessing strings */
	switch(type_of(object)) {
#ifdef ECL_UNICODE
	case t_string:
		if (position >= object->string.dim)
			illegal_index(object, index);
		if (!CHARACTERP(value)) FEtype_error_character(value);
		object->string.self[position] = value;
		@(return object->string.self[position])
#endif
	case t_base_string:
		if (position >= object->base_string.dim)
			illegal_index(object, index);
		/* INV: char_code() checks type of value */
		object->base_string.self[position] = char_code(value);
		@(return value)
	default:
		FEtype_error_string(object);
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
		default:
			FEtype_error_string(string2);
		}
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
		default:
			FEtype_error_string(string2);
		}
	default:
		FEtype_error_string(string1);
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
string_eq(cl_object x, cl_object y)
{
	cl_index i, j;
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
		}
	case t_base_string:
		switch(type_of(y)) {
		case t_string: {
			cl_index index;
			for(index=0; index<i; index++)
				if (CODE_CHAR(x->base_string.self[index]) != y->string.self[index])
					return 0;
			return 1;
			}
		case t_base_string:
			return memcmp(x->base_string.self, y->base_string.self, i) == 0;
		}
	}
#else
	return memcmp(x->base_string.self, y->base_string.self, i) == 0;
#endif
}


#ifdef ECL_UNICODE
@(defun string_equal (string1 string2 &key (start1 MAKE_FIXNUM(0)) end1
		                           (start2 MAKE_FIXNUM(0)) end2)
	cl_index s1, e1, s2, e2;
@
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
				if (toupper(CHAR_CODE(string1->string.self[s1++])) != toupper(CHAR_CODE(string2->string.self[s2++])))
					@(return Cnil)
			@(return Ct)
		case t_base_string:
			while (s1 < e1)
				if (toupper(CHAR_CODE(string1->string.self[s1++])) != toupper(string2->base_string.self[s2++]))
					@(return Cnil)
			@(return Ct)
		default:
			FEtype_error_string(string2);
		}
	case t_base_string:
		switch(type_of(string2)) {
		case t_string:
			while (s1 < e1)
				if (toupper(string1->base_string.self[s1++]) != toupper(CHAR_CODE(string2->string.self[s2++])))
					@(return Cnil)
			@(return Ct)
		case t_base_string:
			while (s1 < e1)
				if (toupper(string1->base_string.self[s1++]) != toupper(string2->base_string.self[s2++]))
					@(return Cnil)
			@(return Ct)
		default:
			FEtype_error_string(string2);
		}
	default:
		FEtype_error_string(string1);
	}
#else
	while (s1 < e1)
		if (string1->base_string.self[s1++] !=
		    string2->base_string.self[s2++])
			@(return Cnil)
#endif
	@(return Ct)
@)
#else
@(defun string_equal (string1 string2 &key (start1 MAKE_FIXNUM(0)) end1
		      (start2 MAKE_FIXNUM(0)) end2)
	cl_index s1, e1, s2, e2;
	char i1, i2;
@
	string1 = cl_string(string1);
	string2 = cl_string(string2);
	get_string_start_end(string1, start1, end1, &s1, &e1);
	get_string_start_end(string2, start2, end2, &s2, &e2);
	if (e1 - s1 != e2 - s2)
		@(return Cnil)
	while (s1 < e1) {
		i1 = string1->base_string.self[s1++];
		i2 = string2->base_string.self[s2++];
		if (toupper(i1) != toupper(i2))
			@(return Cnil)
	}
	@(return Ct)
@)
#endif

/*
	This corresponds to string-equal
	(string equality ignoring the case).
*/
#ifdef ECL_UNICODE
bool
string_equal(cl_object x, cl_object y)
{
	cl_index i, j;

	/* INV: Works with symbols ands strings */
	i = x->base_string.fillp;
	j = y->base_string.fillp;

	if (i != j) return(FALSE);

	switch(type_of(x)) {
	case t_string:
		switch(type_of(x)) {
		case t_string:
			for (i = 0;  i < j;  i++)
				if (toupper(CHAR_CODE(x->string.self[i])) != toupper(CHAR_CODE(y->string.self[i])))
					return(FALSE);
			break;
		case t_base_string:
			for (i = 0;  i < j;  i++)
				if (toupper(CHAR_CODE(x->string.self[i])) != toupper(y->base_string.self[i]))
					return(FALSE);
			break;
		}
	case t_base_string:
		switch(type_of(x)) {
		case t_string:
			for (i = 0;  i < j;  i++)
				if (toupper(x->base_string.self[i]) != toupper(CHAR_CODE(y->string.self[i])))
					return(FALSE);
			break;
		case t_base_string: {
			register char *p, *q;
			p = x->base_string.self;
			q = y->base_string.self;

			for (i = 0;  i < j;  i++)
				if (toupper(p[i]) != toupper(q[i]))
					return(FALSE);
			break;
			}
		}
	}
	return(TRUE);
}
#else
bool
string_equal(cl_object x, cl_object y)
{
	cl_index i, j;
	register char *p, *q;

	/* INV: Works with symbols ands strings */
	i = x->base_string.fillp;
	j = y->base_string.fillp;
	if (i != j)
		return(FALSE);
	p = x->base_string.self;
	q = y->base_string.self;
	for (i = 0;  i < j;  i++)
		if (toupper(p[i]) != toupper(q[i]))
			return(FALSE);
	return(TRUE);
}
#endif

#ifdef ECL_UNICODE
static cl_object
string_cmp(cl_narg narg, int sign, int boundary, cl_va_list ARGS)
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
	switch(type_of(string1)) {
	case t_string:
		switch(type_of(string2)) {
		case t_string:
			while (s1 < e1) {
				if (s2 == e2)
		  			return1(sign>0 ? Cnil : MAKE_FIXNUM(s1));
				i1 = CHAR_CODE(string1->string.self[s1]);
				i2 = CHAR_CODE(string2->string.self[s2]);
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
		case t_base_string:
			while (s1 < e1) {
				if (s2 == e2)
		  			return1(sign>0 ? Cnil : MAKE_FIXNUM(s1));
				i1 = CHAR_CODE(string1->string.self[s1]);
				i2 = string2->base_string.self[s2];
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
		}
	case t_base_string:
		switch(type_of(string2)) {
		case t_string:
			while (s1 < e1) {
				if (s2 == e2)
		  			return1(sign>0 ? Cnil : MAKE_FIXNUM(s1));
				i1 = string1->base_string.self[s1];
				i2 = CHAR_CODE(string2->string.self[s2]);
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
		case t_base_string:
			while (s1 < e1) {
				if (s2 == e2)
		  			return1(sign>0 ? Cnil : MAKE_FIXNUM(s1));
				i1 = string1->base_string.self[s1];
				i2 = string2->base_string.self[s2];
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
		}
	}
#undef start1p
#undef start2p
#undef start1
#undef end1
#undef start2
#undef end2
}
#else
static cl_object
string_cmp(cl_narg narg, int sign, int boundary, cl_va_list ARGS)
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
	while (s1 < e1) {
		if (s2 == e2)
		  return1(sign>0 ? Cnil : MAKE_FIXNUM(s1));
		i1 = string1->base_string.self[s1];
		i2 = string2->base_string.self[s2];
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
#endif

@(defun string< (&rest args)
@
	return string_cmp(narg, 1, 1, args);
@)

@(defun string> (&rest args)
@
	return string_cmp(narg,-1, 1, args);
@)

@(defun string<= (&rest args)
@
	return string_cmp(narg, 1, 0, args);
@)

@(defun string>= (&rest args)
@
	return string_cmp(narg,-1, 0, args);
@)

@(defun string/= (&rest args)
@
	return string_cmp(narg, 0, 1, args);
@)

static cl_object
string_compare(cl_narg narg, int sign, int boundary, cl_va_list ARGS)
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
	switch(type_of(string1)) {
	case t_string:
		switch(type_of(string2)) {
		case t_string:
			while (s1 < e1) {
				if (s2 == e2)
		  			return1(sign>0 ? Cnil : MAKE_FIXNUM(s1));
				i1 = CHAR_CODE(string1->string.self[s1]);
				i1 = toupper(i1);
				i2 = CHAR_CODE(string2->string.self[s2]);
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
		case t_base_string:
			while (s1 < e1) {
				if (s2 == e2)
		  			return1(sign>0 ? Cnil : MAKE_FIXNUM(s1));
				i1 = CHAR_CODE(string1->string.self[s1]);
				i1 = toupper(i1);
				i2 = string2->base_string.self[s2];
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
		}
	case t_base_string:
		switch(type_of(string2)) {
		case t_string:
			while (s1 < e1) {
				if (s2 == e2)
		  			return1(sign>0 ? Cnil : MAKE_FIXNUM(s1));
				i1 = string1->base_string.self[s1];
				i1 = toupper(i1);
				i2 = CHAR_CODE(string2->string.self[s2]);
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
		case t_base_string:
			while (s1 < e1) {
				if (s2 == e2)
		  			return1(sign>0 ? Cnil : MAKE_FIXNUM(s1));
				i1 = string1->base_string.self[s1];
				i1 = toupper(i1);
				i2 = string2->base_string.self[s2];
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
		}
	}
#undef start1p
#undef start2p
#undef start1
#undef end1
#undef start2
#undef end2
}

@(defun string-lessp (&rest args)
@
	return string_compare(narg, 1, 1, args);
@)

@(defun string-greaterp (&rest args)
@
	return string_compare(narg,-1, 1, args);
@)

@(defun string-not-greaterp (&rest args)
@
	return string_compare(narg, 1, 0, args);
@)

@(defun string-not-lessp (&rest args)
@
	return string_compare(narg,-1, 0, args);
@)

@(defun string-not-equal (&rest args)
@
	return string_compare(narg, 0, 1, args);
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

	case t_symbol:
		if (Null(char_bag))
			return(FALSE);
		FEwrong_type_argument(@'sequence', char_bag);

	default:
		FEwrong_type_argument(@'sequence', char_bag);
	}
}

static cl_object
string_trim0(bool left_trim, bool right_trim, cl_object char_bag, cl_object strng)
{
	cl_object res;
	cl_index i, j, k;

	strng = cl_string(strng);
	i = 0;
	j = strng->base_string.fillp - 1;
	if (left_trim)
		for (;  i <= j;  i++)
			if (!member_char(strng->base_string.self[i], char_bag))
				break;
	if (right_trim)
		for (;  j >= i;  --j)
			if (!member_char(strng->base_string.self[j], char_bag))
				break;
	k = j - i + 1;
	res = cl_alloc_simple_base_string(k);
	memcpy(res->base_string.self, strng->base_string.self+i, k);
	@(return res)
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
	conv  = copy_simple_string(strng);
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


#ifdef ECL_UNICODE
static cl_object
nstring_case(cl_narg narg, int (*casefun)(int, bool *), cl_va_list ARGS)
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

	assert_type_string(strng);
	if (startp == Cnil) start = MAKE_FIXNUM(0);
	get_string_start_end(strng, start, end, &s, &e);
	b = TRUE;
	switch(type_of(strng)) {
	case t_string:
		for (i = s;  i < e;  i++)
			strng->string.self[i] = CODE_CHAR((*casefun)(CHAR_CODE(strng->string.self[i]), &b));
		break;
	case t_base_string:
		for (i = s;  i < e;  i++)
			strng->base_string.self[i] = (*casefun)(strng->base_string.self[i], &b);
		break;
	}
	@(return strng)
#undef startp
#undef start
#undef end
}
#else
static cl_object
nstring_case(cl_narg narg, int (*casefun)(int, bool *), cl_va_list ARGS)
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

	assert_type_base_string(strng);
	if (startp == Cnil) start = MAKE_FIXNUM(0);
	get_string_start_end(strng, start, end, &s, &e);
	b = TRUE;
	for (i = s;  i < e;  i++)
		strng->base_string.self[i] = (*casefun)(strng->base_string.self[i], &b);
	@(return strng)
#undef startp
#undef start
#undef end
}
#endif

@(defun nstring-upcase (&rest args)
@
	return nstring_case(narg, char_upcase, args);
@)

@(defun nstring-downcase (&rest args)
@
	return nstring_case(narg, char_downcase, args);
@)

@(defun nstring-capitalize (&rest args)
@
	return nstring_case(narg, char_capitalize, args);
@)

@(defun si::base_string_concatenate (&rest args)
	cl_index l;
	int i;
	char *vself;
#ifdef __GNUC__
	cl_object v, strings[narg];
#else
#define NARG_MAX 64
	cl_object v, strings[NARG_MAX];
#endif
@
#ifndef __GNUC__
	if (narg > NARG_MAX)
		FEerror("si::string_concatenate: Too many arguments, limited to ~A", 1, MAKE_FIXNUM(NARG_MAX));
#endif
	/* FIXME! We should use cl_va_start() instead of this ugly trick */
	for (i = 0, l = 0;  i < narg;  i++) {
		strings[i] = cl_base_string(cl_va_arg(args));
		l += strings[i]->base_string.fillp;
	}
	v = cl_alloc_simple_base_string(l);
	for (i = 0, vself = v->base_string.self;  i < narg;  i++, vself += l) {
		l = strings[i]->base_string.fillp;
		memcpy(vself, strings[i]->base_string.self, l);
	}
	@(return v)
@)

#ifdef ECL_UNICODE
@(defun si::extended_string_concatenate (&rest args)
	cl_index l;
	int i;
	char *vself;
#ifdef __GNUC__
	cl_object v, strings[narg];
#else
#define NARG_MAX 64
	cl_object v, strings[NARG_MAX];
#endif
@
#ifndef __GNUC__
	if (narg > NARG_MAX)
		FEerror("si::string_concatenate: Too many arguments, limited to ~A", 1, MAKE_FIXNUM(NARG_MAX));
#endif
	/* FIXME! We should use cl_va_start() instead of this ugly trick */
	for (i = 0, l = 0;  i < narg;  i++) {
		strings[i] = cl_extended_string(cl_va_arg(args));
		l += strings[i]->string.fillp;
	}
	v = cl_alloc_simple_extended_string(l);
	for (i = 0, vself = v->string.self;  i < narg;  i++, vself += l) {
		l = strings[i]->string.fillp;
		memcpy(vself, strings[i]->string.self, l);
	}
	@(return v)
@)
#endif

#ifdef ECL_UNICODE
int
ecl_string_push_extend(cl_object s, int c)
{
	cl_index new_length;

	switch(type_of(s)) {
	case t_string:
		if (s->string.fillp >= s->string.dim) {
			cl_object *p;
			if (!s->string.adjustable)
				FEerror("string-push-extend: the string ~S is not adjustable.",
					1, s);
			start_critical_section(); /* avoid losing p */
			if (s->string.dim >= ADIMLIM/2)
				FEerror("Can't extend the string.", 0);
			new_length = (s->string.dim + 1) * 2;
			p = (cl_object *)cl_alloc_align(sizeof (cl_object)*new_length, sizeof (cl_object));
			memcpy(p, s->string.self, s->string.dim * sizeof (cl_object));
			s->string.dim = new_length;
			adjust_displaced(s, p - s->string.self);
			end_critical_section();
		}
		s->string.self[s->string.fillp++] = CODE_CHAR(c);
		return c;
	case t_base_string:
		if (s->base_string.fillp >= s->base_string.dim) {
			char *p;
			if (!s->base_string.adjustable)
				FEerror("string-push-extend: the string ~S is not adjustable.",
					1, s);
			start_critical_section(); /* avoid losing p */
			if (s->base_string.dim >= ADIMLIM/2)
				FEerror("Can't extend the string.", 0);
			new_length = (s->base_string.dim + 1) * 2;
			p = (char *)cl_alloc_atomic(new_length+1); p[new_length] = 0;
			memcpy(p, s->base_string.self, s->base_string.dim * sizeof(char));
			s->base_string.dim = new_length;
			adjust_displaced(s, p - (char *)s->base_string.self);
			end_critical_section();
		}
		s->base_string.self[s->base_string.fillp++] = c;
		return c;
	default:
		FEtype_error_string(s);
	}
}
#else
int
ecl_string_push_extend(cl_object s, int c)
{
	char *p;
	cl_index new_length;

	if (type_of(s) != t_base_string) {
		FEtype_error_string(s);
	} else if (s->base_string.fillp >= s->base_string.dim) {
		if (!s->base_string.adjustable)
			FEerror("string-push-extend: the string ~S is not adjustable.",
				1, s);
		start_critical_section(); /* avoid losing p */
		if (s->base_string.dim >= ADIMLIM/2)
			FEerror("Can't extend the string.", 0);
		new_length = (s->base_string.dim + 1) * 2;
		p = (char *)cl_alloc_atomic(new_length+1); p[new_length] = 0;
		memcpy(p, s->base_string.self, s->base_string.dim * sizeof(char));
		s->base_string.dim = new_length;
		adjust_displaced(s, p - (char *)s->base_string.self);
		end_critical_section();
	}
	s->base_string.self[s->base_string.fillp++] = c;
	return c;
}
#endif
