/*
    pathname.d -- Pathnames.
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

/*
	O.S. DEPENDENT

	This file contains those functions that interpret namestrings.
*/

#include "ecl.h"
#include <ctype.h>

/******************************* EXPORTS ******************************/

cl_object @'*default-pathname-defaults*';
cl_object @':wild';
cl_object @':wild-inferiors';
cl_object @':newest';

cl_object @':host';
cl_object @':device';
cl_object @':directory';
cl_object @':name';
cl_object @':type';
cl_object @':version';
cl_object @':defaults';
cl_object @':unspecific';

cl_object @':absolute';
cl_object @':relative';
cl_object @':up';

/******************************* ------- ******************************/

static cl_object pathname_translations = Cnil;

static void
error_directory(cl_object d) {
	FEerror("make-pathname: ~A is not a valid directory", 1, d);
}

cl_object
make_pathname(cl_object host, cl_object device, cl_object directory,
	      cl_object name, cl_object type, cl_object version)
{
	cl_object x;

	switch (type_of(directory)) {
	case t_string:
		directory = list(2, @':absolute', directory);
		break;
	case t_symbol:
		if (directory == Cnil)
			break;
		if (directory == @':wild')
			directory = list(2, @':absolute', @':wild-inferiors');
		error_directory(directory);
		break;
	case t_cons:
		if (CAR(directory) == @':absolute' ||
		    CAR(directory) == @':relative')
			break;
	default:
		error_directory(directory);

	}
	x = cl_alloc_object(t_pathname);
	if (type_of(host) == t_string)
		x->pathname.logical = logical_hostname_p(host);
	else if (host == Cnil)
		x->pathname.logical = FALSE;
	else
		FEerror("make-pathname: ~A is not a valid hostname", 1, host);
	if (device != Cnil && device != @':unspecific' &&
	    !(!x->pathname.logical && type_of(device) == t_string))
		FEerror("make-pathname: ~A is not a valid device name", 1, device);
	if (name != Cnil && name != @':wild' && type_of(name) != t_string)
		FEerror("make-pathname: ~A is not a valid file name", 1, name);
	if (type != Cnil && type != @':wild' && type_of(type) != t_string)
		FEerror("make-pathname: ~A is not a valid file type", 1, type);
	if (version != @':unspecific' && version != Cnil)
		FEerror("make-pathname: version numbers not allowed", 0);
	x->pathname.host = host;
	x->pathname.device = device;
	x->pathname.directory = directory;
	x->pathname.name = name;
	x->pathname.type = type;
	x->pathname.version = @':unspecific';
	return(x);
}


static cl_object
tilde_expand(cl_object directory)
{
	cl_object head, prefix;

	/* INV: pathname is relative */
	if (endp(directory))
		goto RET;
	head = CADR(directory);
	if (type_of(head) != t_string)
		goto RET;
	if (head->string.fillp == 0 || head->string.self[0] != '~')
		goto RET;
	prefix = homedir_pathname(head)->pathname.directory;
	directory = append(prefix, CDDR(directory));
 RET:
	return directory;
}

#define WORD_INCLUDE_DELIM 1
#define WORD_ALLOW_ASTERISK  2
#define WORD_EMPTY_IS_NIL 4
#define WORD_LOGICAL 8

static cl_object
make_one(const char *s, cl_index end)
{
	cl_object x = cl_alloc_simple_string(end);
	x->string.self = (char *)cl_alloc(end+1);
	memcpy(x->string.self, s, end);
	x->string.self[end] = '\0';
	return(x);
}

/*
 * Parses a word from string `S' until either:
 *	1) character `DELIM' is found
 *	2) end of string is reached
 *	3) a non valid character is found
 * Output is either
 *	1) :error in case (3) above
 *	2) :wild, :wild-inferiors, :up
 *	3) "" or Cnil when word has no elements
 *	5) A non empty string
 */
static cl_object
parse_word(const char *s, char delim, int flags, cl_index start, cl_index end,
	   cl_index *end_of_word)
{
	cl_index i, j;
	bool wild_inferiors = FALSE;

	for (i = j = start; i < end && s[i] != delim; i++) {
		char c = s[i];
		bool valid_char;
		if (c == '*') {
			if (!(flags & WORD_ALLOW_ASTERISK))
				valid_char = FALSE; /* Asterisks not allowed in this word */
			else {
				wild_inferiors = (i > start && s[i-1] == '*');
				valid_char = TRUE; /* single "*" */
			}
		}			
#if 0
		else if (flags & WORD_LOGICAL)
			valid_char = is_upper(c) || is_digit(c) || c == '-';
#endif
		else
			valid_char = c != 0;
		if (!valid_char) {
			*end_of_word = start;
			return @':error';
		}
	}
	if (i < end)
		*end_of_word = i+1;
	else {
		*end_of_word = end;
		/* We have reached the end of the string without finding
		   the proper delimiter */
		if (flags & WORD_INCLUDE_DELIM) {
			*end_of_word = start;
			return Cnil;
		}
	}
	s += j;
	switch(i-j) {
	case 0:
		if (flags & WORD_EMPTY_IS_NIL)
			return Cnil;
		return null_string;
	case 1:
		if (s[0] == '*')
			return @':wild';
		break;
	case 2:
		if (s[0] == '*' && s[1] == '*')
			return @':wild-inferiors';
		if (!(flags & WORD_LOGICAL) && s[0] == '.' && s[1] == '.')
			return @':up';
		break;
	default:
		if (wild_inferiors)	/* '**' surrounded by other characters */
			return @':error';
	}
	return make_one(s, i-j);
}

/*
 * Parses a logical or physical directory tree. Output is always a
 * list of valid directory components, which may be just NIL.
 *
 * INV: When parsing of directory components has failed, a valid list
 * is also returned, and it will be later in the parsing of
 * pathname-name or pathname-type when the same error is detected.
 */

static cl_object
parse_directories(const char *s, int flags, cl_index start, cl_index end,
		  cl_index *end_of_dir)
{
	cl_index i, j;
	cl_object path = Cnil;
	cl_object *plast = &path;
	char delim = (flags & WORD_LOGICAL) ? ';' : '/';

	flags |= WORD_INCLUDE_DELIM | WORD_ALLOW_ASTERISK;
	*end_of_dir = start;
	for (i = j = start; i < end; j = i) {
		cl_object word = parse_word(s, delim, flags, j, end, &i);
		if (word == @':error' || word == Cnil)
			break;
		if (word == null_string) {	/* just "/" or ";" */
			if (j != start) {
				if (flags & WORD_LOGICAL)
					return @':error';
				continue;
			}
			word = (flags & WORD_LOGICAL) ? @':relative' : @':absolute';
		}
		*end_of_dir = i;
		plast = &CDR(*plast = CONS(word, Cnil));
	}
	return path;
}

bool
logical_hostname_p(cl_object host)
{
	if (type_of(host) != t_string)
		return FALSE;
	return assoc(host, pathname_translations) != Cnil;
}

/*
 * Parses a lisp namestring until the whole substring is parsed or an
 * error is found. It returns a valid pathname or NIL, plus the place
 * where parsing ended in *END_OF_PARSING.
 *
 * The rules are as follows:
 *
 * 1) If a hostname is supplied it determines whether the namestring
 *    will be parsed as logical or as physical.
 *
 * 2) If no hostname is supplied, first it tries parsing using logical
 *    pathname rules and, if no logical hostname is found, then it
 *    tries the physical pathname format.
 *
 * 3) Logical pathname syntax:
 *	[logical-hostname:][;][logical-directory-component;][pathname-name][.pathname-type]
 *
 * 4) Physical pathname syntax:
 *	[device:][[//hostname]/][directory-component/]*[pathname-name][.pathname-type]
 *
 *	logical-hostname, device, hostname = word
 *	logical-directory-component = word | wildcard-word
 *	directory-component = word | wildcard-word | '..' | '.'
 *	pathname-name, pathname-type = word | wildcard-word | ""
 *
 */
cl_object
parse_namestring(const char *s, cl_index start, cl_index end, cl_index *ep,
		 cl_object default_host)
{
	cl_object host, device, path, name, type, version;
	bool logical;

	/* We first try parsing as logical-pathname. In case of
	 * failure, physical-pathname parsing is performed only when
	 * there is no supplied *logical* host name. All other failures
	 * result in Cnil as output.
	 */
	host = parse_word(s, ':', WORD_LOGICAL | WORD_INCLUDE_DELIM, start, end, ep);
	if (default_host != Cnil) {
		if (host == Cnil || host == @':error')
			host = default_host;
		else if (!equal(default_host, host))
			return Cnil;
	}
	if (!logical_hostname_p(host))
		goto physical;
	/*
	 * Logical pathname format:
	 *	[logical-hostname:][;][logical-directory-component;][pathname-name][.pathname-type]
	 */
	logical = TRUE;
	device = Cnil;
	path = parse_directories(s, WORD_LOGICAL, *ep, end, ep);
	if (path == @':error')
		return Cnil;
	if (!endp(path) && CAR(path) != @':relative')
		path = CONS(@':absolute', path);
	name = parse_word(s, '.', WORD_LOGICAL | WORD_ALLOW_ASTERISK |
			  WORD_EMPTY_IS_NIL, *ep, end, ep);
	type = parse_word(s, '\0', WORD_LOGICAL | WORD_ALLOW_ASTERISK |
			  WORD_EMPTY_IS_NIL, *ep, end, ep);
	if (type == @':error')
		return Cnil;
	goto make_it;
 physical:
	/*
	 * Physical pathname format:
	 *	[device:][[//hostname]/][directory-component/]*[pathname-name][.pathname-type]
	 */
	logical = FALSE;
	device = parse_word(s, ':', WORD_INCLUDE_DELIM | WORD_EMPTY_IS_NIL,
			    start, end, ep);
	if (device == @':error')
		device = Cnil;
	else if (device != Cnil) {
		if (type_of(device) != t_string)
			return Cnil;
		if (strcmp(device->string.self, "file") == 0)
			device = Cnil;
	}
	start = *ep;
	if (start <= end - 2 && s[start] == '/' && s[start+1] == '/') {
		host = parse_word(s, '/', WORD_EMPTY_IS_NIL, start+2, end, ep);
		if (host != Cnil) {
			start = *ep;
			if (s[--start] == '/') *ep = start;
		}
	} else
		host = Cnil;
	if (host == @':error')
		host = Cnil;
	else if (host != Cnil) {
		if (type_of(host) != t_string)
			return Cnil;
	}
	path = parse_directories(s, 0, *ep, end, ep);
	if (path == @':error')
		return Cnil;
	if (!endp(path)) {
		if (CAR(path) == @':absolute') {
			/* According to ANSI CL, "/.." is erroneous */
			if (cadr(path) == @':up')
				return Cnil;
		} else  {
			/* If path is relative and we got here, then it
			   has no :RELATIVE/:ABSOLUTE in front of it and we add one.
			   Pathnames with hostnames are always absolute.
			*/
			path = CONS(host == Cnil? @':relative' : @':absolute', path);
			path = tilde_expand(path);
		}
	}
	name = parse_word(s, '.', WORD_ALLOW_ASTERISK | WORD_EMPTY_IS_NIL, *ep,
			  end, ep);
	type = parse_word(s, '\0', WORD_ALLOW_ASTERISK | WORD_EMPTY_IS_NIL, *ep,
			  end, ep);
	version = parse_word(s, '\0', WORD_ALLOW_ASTERISK | WORD_EMPTY_IS_NIL, *ep,
			     end, ep);
	if (version == @':error')
		return Cnil;
 make_it:
	if (*ep >= end) *ep = end;
	path = make_pathname(host, device, path, name, type, @':unspecific');
	path->pathname.logical = logical;
	return path;
}

cl_object
coerce_to_pathname(cl_object x)
{
	cl_object y;
	cl_index e;

L:
	switch (type_of(x)) {
	case t_string:
                /* !!!!! Bug Fix. NLG */
		y = parse_namestring(x->string.self, 0, x->string.fillp, &e,Cnil);
		if (y == Cnil || e != x->string.fillp)
		  FEerror("~S is not a valid pathname string", 1, x);
		return(y);

	case t_pathname:
		return(x);

	case t_stream:
		switch ((enum smmode)x->stream.mode) {
		case smm_closed:
		case smm_input:
		case smm_output:
		case smm_probe:
		case smm_io:
			x = x->stream.object1;
			/*
				The file was stored in stream.object1.
				See open.
			*/
			goto L;

		case smm_synonym:
			x = symbol_value(x->stream.object0);
			goto L;
		}
	default:
		FEerror("~S cannot be coerced to a pathname.", 1, x);
	}
}

cl_object
coerce_to_file_pathname(cl_object pathname)
{
	pathname = coerce_to_physical_pathname(pathname);
#ifndef cygwin
	if (pathname->pathname.device != Cnil)
		FEerror("Device ~S not yet supported.", 1,
			pathname->pathname.device);
	if (pathname->pathname.host != Cnil)
		FEerror("Access to remote files not yet supported.", 0);
#endif
	return pathname;
}

static cl_object translate_logical_pathname(cl_object x);

cl_object
coerce_to_physical_pathname(cl_object x)
{
	x = coerce_to_pathname(x);
	if (x->pathname.logical)
	  return translate_logical_pathname(x);
	return x;
}

cl_object
coerce_to_filename(cl_object pathname)
{
	cl_object namestring;

	pathname = coerce_to_file_pathname(pathname);
	namestring = coerce_to_namestring(pathname);
	if (namestring->string.fillp >= MAXPATHLEN - 16)
		FEerror("Too long filename: ~S.", 1, namestring);
	return namestring;
}

cl_object
default_device(cl_object host)
{
	return Cnil;
}

cl_object
merge_pathnames(cl_object path, cl_object defaults, cl_object default_version)
{
	cl_object host, device, directory, name, type, version;

	defaults = coerce_to_pathname(defaults);
	if (type_of(path) == t_string) {
		cl_index foo;
		cl_object aux = parse_namestring(path->string.self, 0,
						 path->string.fillp, &foo,
						 default_version->pathname.host);
		if (aux != Cnil) path = aux;
	}
	if (type_of(path) != t_pathname)
		path = coerce_to_pathname(path);
	if (Null(path->pathname.host))
		host = defaults->pathname.host;
	else
		host = path->pathname.host;
	if (Null(path->pathname.device))
		if (Null(path->pathname.host))
			device = defaults->pathname.device;
		else if (path->pathname.host == defaults->pathname.host)
			device = defaults->pathname.device;
		else
			device = default_device(path->pathname.host);
	else
		device = path->pathname.device;
	if (Null(path->pathname.directory))
		directory = defaults->pathname.directory;
	else if (CAR(path->pathname.directory) == @':absolute')
		directory = path->pathname.directory;
	else if (!Null(defaults->pathname.directory))
		directory = append(defaults->pathname.directory,
				   CDR(path->pathname.directory));
	else
		directory = path->pathname.directory;
	if (Null(path->pathname.name))
		name = defaults->pathname.name;
	else
		name = path->pathname.name;
	if (Null(path->pathname.type))
		type = defaults->pathname.type;
	else
		type = path->pathname.type;
	version = Cnil;
	/*
		In this implementation, version is not considered
	*/
	defaults = make_pathname(host, device, directory, name, type, version);
	defaults->pathname.logical = path->pathname.logical;
	return defaults;
}

static void
push_c_string(cl_object buffer, const char *s, cl_index length)
{
	cl_index fillp = buffer->string.fillp;
	cl_index dim = buffer->string.dim;
	char *dest = buffer->string.self;

	if (type_of(buffer) != t_string)
		internal_error("push_c_string");
	for (; length; length--, s++) {
		dest[fillp++] = *s;
		if (fillp >= dim) {
			char *new_dest = (char *)cl_alloc_atomic(dim += 32);
			memcpy(new_dest, dest, fillp);
			buffer->string.dim = dim;
			buffer->string.self = new_dest;
			dest = new_dest;
		}
	}
	buffer->string.fillp = fillp;
}

static void
push_string(cl_object buffer, cl_object string)
{
	string = coerce_to_string_designator(string);
	push_c_string(buffer, string->string.self, string->string.fillp);
}

/*
	namestring(x) converts a pathname to a namestring.
*/
cl_object
namestring(cl_object x)
{
	cl_object l, y;
	bool logical;
	cl_object buffer, host;

	buffer = cl_token;
	buffer->string.fillp = 0;
	logical = x->pathname.logical;
	host = x->pathname.host;
	if (logical) {
		if (host != Cnil) {
			push_string(buffer, host);
			push_c_string(buffer, ":", 1);
		}
	} else {
		if ((y = x->pathname.device) != Cnil) {
			push_string(buffer, y);
			push_c_string(buffer, ":", 1);
		}
		if (host != Cnil) {
			if (y == Cnil)
				push_c_string(buffer, "file:", 5);
			push_c_string(buffer, "//", 2);
			push_string(buffer, host);
		}
	}
	l = x->pathname.directory;
	if (endp(l))
		goto L;
	y = CAR(l);
	if (y == @':relative') {
		if (logical)
			push_c_string(buffer, ":", 1);
	} else if (y == @':absolute') {
		if (!logical)
			push_c_string(buffer, "/", 1);
	} else
		FEerror("namestring: ~A is not a valid directory list",1, l);
	l = CDR(l);
	for (;  !endp(l);  l = CDR(l)) {
		y = CAR(l);
		if (y == @':up') {
			push_c_string(buffer, "..", 2);
		} else if (y == @':wild') {
			push_c_string(buffer, "*", 1);
		} else if (y == @':wild-inferiors') {
			push_c_string(buffer, "**", 2);
		} else {
			push_string(buffer, y);
		}
		push_c_string(buffer, logical? ";" : "/", 1);
	}
L:
	if (Null(y = x->pathname.name))
		goto M;
	if (y == @':wild') {
		push_c_string(buffer, "*", 1);
		goto M;
	}
	if (type_of(y) != t_string)
		FEerror("~S is an illegal pathname name.", 1, y);
	push_string(buffer, y);
M:
	if (Null(y = x->pathname.type))
		goto N;
	if (y == @':wild') {
		push_c_string(buffer, ".*", 2);
		goto N;
	}
	if (type_of(y) != t_string)
		FEerror("~S is an illegal pathname type.", 1, y);
	push_c_string(buffer, ".", 1);
	push_string(buffer, y);
	/* INV: pathname.version is always @':unspecific' or Cnil */
N:
	return(copy_simple_string(cl_token));
}

cl_object
coerce_to_namestring(cl_object x)
{
	cl_object y;
L:
	switch (type_of(x)) {
	case t_string:
		if (x->string.self[0] != '~')
		  return(x);
		/* added by E. Wang */
		return(namestring(coerce_to_pathname(x)));

	case t_pathname:
		return(namestring(x));

	case t_stream:
		switch ((enum smmode)x->stream.mode) {
		case smm_input:
		case smm_output:
		case smm_probe:
		case smm_io:
			x = x->stream.object1;
			/*
				The file was stored in stream.object1.
				See open.
			*/
			goto L;

		case smm_synonym:
			x = symbol_value(x->stream.object0);
			goto L;

		default:
			goto CANNOT_COERCE;
		}

	default:
	CANNOT_COERCE:
		FEerror("~S cannot be coerced to a namestring.", 1, x);
	}
}

@(defun pathname (name)
@	/* INV: coerce_to_pathname() checks types */
	@(return coerce_to_pathname(name))
@)

@(defun parse_namestring (thing
	&o host
	   (defaults symbol_value(@'*default-pathname-defaults*'))
	&k (start MAKE_FIXNUM(0)) end junk_allowed
	&a x y)
	cl_index s, e, ee;
@
	/* INV: coerce_to_pathname() checks types */
	/* defaults is ignored */
	x = thing;
L:
	switch (type_of(x)) {
	case t_string:
		get_string_start_end(x, start, end, &s, &e);
		y = parse_namestring(x->string.self, s, e - s, &ee, host);
		if (Null(junk_allowed)) {
			if (y == Cnil || ee != e - s)
				FEerror("Cannot parse the namestring ~S~%\nfrom ~S to ~S.",
					3, x, start, end);
		} else {
			if (y == Cnil)
				@(return Cnil MAKE_FIXNUM(s + ee))
		}
		if (logical_hostname_p(host) && y != Cnil && !y->pathname.logical) {
			if (Null(junk_allowed))
				FEerror("A logical pathname was expected instead of ~S", 1, thing);
			else
				@(return Cnil MAKE_FIXNUM(s + ee));
		}
		start = MAKE_FIXNUM(s + ee);
		break;
		
	case t_pathname:
		y = x;
		break;
		
	case t_stream:
		switch ((enum smmode)x->stream.mode) {
		case smm_input:
		case smm_output:
		case smm_probe:
		case smm_io:
			x = x->stream.object1;
			/*
				The file was stored in stream.object1.
				See open.
			*/
			goto L;

		case smm_synonym:
			x = symbol_value(x->stream.object0);
			goto L;

		default:
			goto CANNOT_PARSE;
		}

	default:
	CANNOT_PARSE:
		FEerror("Object ~S does not contain a valid namestring.", 1, x);
	}
	@(return y start)
@)

@(defun merge_pathnames (path
	&o (defaults symbol_value(@'*default-pathname-defaults*'))
 	   (default_version @':newest'))
@
	/* INV: coerce_to_pathname() checks types */
	path = coerce_to_pathname(path);
	defaults = coerce_to_pathname(defaults);
	@(return merge_pathnames(path, defaults, default_version))
@)

@(defun make_pathname (&key host device directory name
			    type version defaults
		       &aux x)
@
	if (Null(defaults)) {
		defaults
		= symbol_value(@'*default-pathname-defaults*');
		defaults = coerce_to_pathname(defaults);
		defaults
		= make_pathname(defaults->pathname.host,
			        Cnil, Cnil, Cnil, Cnil, Cnil);
	} else
		defaults = coerce_to_pathname(defaults);
	x = make_pathname(host, device, directory, name, type, version);
	x = merge_pathnames(x, defaults, Cnil);
	@(return x)
@)

@(defun pathnamep (pname)
@
	@(return ((type_of(pname) == t_pathname)? Ct : Cnil))
@)

@(defun si::logical_pathname_p (pname)
@
	@(return ((type_of(pname) == t_pathname && pname->pathname.logical)?
		  Ct : Cnil))
@)

@(defun pathname_host (pname)
@
	/* INV: coerce_to_pathname() checks types */
	pname = coerce_to_pathname(pname);
	@(return pname->pathname.host)
@)

@(defun pathname_device (pname)
@
	/* INV: coerce_to_pathname() checks types */
	pname = coerce_to_pathname(pname);
	@(return pname->pathname.device)
@)

@(defun pathname_directory (pname)
@
	/* INV: coerce_to_pathname() checks types */
	pname = coerce_to_pathname(pname);
	@(return pname->pathname.directory)
@)

@(defun pathname_name (pname)
@
	/* INV: coerce_to_pathname() checks types */
	pname = coerce_to_pathname(pname);
	@(return pname->pathname.name)
@)

@(defun pathname_type (pname)
@
	/* INV: coerce_to_pathname() checks types */
	pname = coerce_to_pathname(pname);
	@(return pname->pathname.type)
@)

@(defun pathname_version (pname)
@
	/* INV: coerce_to_pathname() checks types */
	pname = coerce_to_pathname(pname);
	@(return  pname->pathname.version)
@)

@(defun namestring (pname)
@
	/* INV: coerce_to_pathname() checks types */
	@(return coerce_to_namestring(pname))
@)

@(defun file_namestring (pname)
@
	/* INV: coerce_to_filename() checks types */
	pname = coerce_to_filename(pname);
	@(return namestring(make_pathname(Cnil, Cnil, Cnil,
					     pname->pathname.name,
					     pname->pathname.type,
					     pname->pathname.version)))
@)

@(defun directory_namestring (pname)
@
	/* INV: coerce_to_pathname() checks types */
	pname = coerce_to_pathname(pname);
	@(return namestring(make_pathname(Cnil, Cnil,
					     pname->pathname.directory,
					     Cnil, Cnil, Cnil)))
@)

@(defun host_namestring (pname)
@
	/* INV: coerce_to_pathname() checks types */
	pname = coerce_to_pathname(pname);
	pname = pname->pathname.host;
	if (Null(pname) || pname == @':wild')
		pname = null_string;
	@(return pname)
@)

@(defun enough_namestring (path
	&o (defaults symbol_value(@'*default-pathname-defaults*')))
	cl_object newpath;
@
	/* INV: coerce_to_pathname() checks types */
	defaults = coerce_to_pathname(defaults);
	path = coerce_to_pathname(path);
	newpath
	= make_pathname(equalp(path->pathname.host, defaults->pathname.host) ?
			Cnil : path->pathname.host,
	                equalp(path->pathname.device,
			       defaults->pathname.device) ?
			Cnil : path->pathname.device,
	                equalp(path->pathname.directory,
			       defaults->pathname.directory) ?
			Cnil : path->pathname.directory,
	                equalp(path->pathname.name, defaults->pathname.name) ?
			Cnil : path->pathname.name,
	                equalp(path->pathname.type, defaults->pathname.type) ?
			Cnil : path->pathname.type,
	                equalp(path->pathname.version,
			       defaults->pathname.version) ?
			Cnil : path->pathname.version);
	newpath->pathname.logical = path->pathname.logical;
	@(return namestring(newpath))
@)

/* --------------- PATHNAME MATCHING ------------------ */

static bool path_item_match(cl_object a, cl_object mask);

static bool
do_path_item_match(const char *s, const char *p) {
	const char *next;
	while (*s) {
	  if (*p == '*') {
	    /* Match any group of characters */
	    next = p+1;
	    while (*s && *s != *next) s++;
	    if (do_path_item_match(s,next))
	      return TRUE;
	    /* starts back from the '*' */
	    if (!*s)
	      return FALSE;
	    s++;
	  } else if (*s != *p)
	    return FALSE;
	  else
	    s++, p++;
	}
	return (*p == 0);
}

static bool
path_item_match(cl_object a, cl_object mask) {
	if (mask == @':wild' || mask == Cnil)
		return TRUE;
	if (type_of(a) != t_string)
		return (a == mask);
	if (type_of(mask) != t_string)
		FEerror("~S is not supported as mask for pathname-match-p", 1, mask);
	return do_path_item_match(a->string.self, mask->string.self);
}

static bool
path_list_match(cl_object a, cl_object mask) {
	cl_object item_mask;
	while (!endp(mask)) {
		item_mask = CAR(mask);
		if (item_mask == @':wild-inferiors') {
			if (endp(mask))
				return TRUE;
			while (!endp(a)) {
				if (path_list_match(a, mask))
					return TRUE;
				a = CDR(a);
			}
			return FALSE;
		}
		if (endp(a))
			return FALSE;
		if (!path_item_match(CAR(a), item_mask))
			return FALSE;
		a = CDR(a);
		mask = CDR(mask);
	}
	if (!endp(a))
		return FALSE;
}		

bool
pathname_match_p(cl_object path, cl_object mask)
{
	path = coerce_to_pathname(path);
	mask = coerce_to_pathname(mask);
	if (path->pathname.logical != mask->pathname.logical)
		return FALSE;
#if 0
	/* INV: This was checked in the calling routine */
	if (!path_item_match(path->pathname.host, mask->pathname.host))
		return FALSE;
#endif
	if (!path_list_match(path->pathname.directory,
			     path->pathname.directory))
		return FALSE;
	if (!path_item_match(path->pathname.name, mask->pathname.name))
		return FALSE;
	if (!path_item_match(path->pathname.type, mask->pathname.type))
		return FALSE;
	if (!path_item_match(path->pathname.version, mask->pathname.version))
		return FALSE;
	return TRUE;
}

@(defun pathname_match_p (path mask)
@
	@(return (pathname_match_p(path, mask)? Ct : Cnil))
@)

/* --------------- PATHNAME TRANSLATIONS ------------------ */

static cl_object
coerce_to_from_pathname(cl_object x, cl_object host)
{
	cl_object y;
	cl_index e;

	switch (type_of(x)) {
	case t_string:
                /* !!!!! Bug Fix. NLG */
		y = parse_namestring(x->string.self, 0, x->string.fillp, &e, host);
		if (y == Cnil || e != x->string.fillp)
			FEerror("~S is not a valid pathname string", 1, x);
		x = y;
	case t_pathname:
		if (x->pathname.logical)
			return x;
	default:
		return Cnil;
	}
}

@(defun si::pathname_translations (host &optional (set OBJNULL))
	cl_index parsed_length, length;
	cl_object host, pair, l;
@
	/* Check that host is a valid host name */
	assert_type_string(host);
	parsed_length = host->string.fillp;
	parse_word(host->string.self, '\0', WORD_LOGICAL, 0, length, &parsed_length);
	if (parsed_length <= host->string.fillp)
		FEerror("Wrong host syntax ~S", 1, host);

	/* Find its translation list */
	pair = assoc(host, pathname_translations);
	if (set == OBJNULL)
		@(return ((pair == Cnil)? Cnil : CADR(pair)))

	/* Set the new translation list */
	assert_type_list(set);
	if (pair == Cnil) {
		pair = CONS(host, CONS(Cnil, Cnil));
		pathname_translations = CONS(pair, pathname_translations);
	}
	for (l = set, set = Cnil; !endp(l); l = CDR(l)) {
		cl_object item = CAR(l);
		cl_object from = coerce_to_from_pathname(car(item), host);
		cl_object to = coerce_to_pathname(cadr(item));
		if (type_of(from) != t_pathname || !from->pathname.logical)
		  FEerror("~S is not a valid from-pathname translation", 1, from);
		if (type_of(to) != t_pathname)
		  FEerror("~S is not a valid to-pathname translation", 1, from);
		set = CONS(CONS(from, CONS(to, Cnil)), set);
	}
	CADR(pair) = @nreconc(2, set, Cnil);
	@(return set)
@)

static cl_object
find_wilds(cl_object l, cl_object source_item, cl_object match)
{
	const char *a, *b;
	cl_index i, j, k, ia, ib;

	if (match == @':wild')
		return CONS(source_item, Cnil);
	if (type_of(match) != t_string || type_of(source_item) != t_string) {
		if (match != source_item)
			return @':error';
		return l;
	}
	a  = source_item->string.self;
	ia = source_item->string.fillp;
	b  = match->string.self;
	ib = match->string.fillp;
	for(i = j = 0; i < ia && j < ib; ) {
		if (b[j] == '*') {
			for (j++, k = i; k < ia && a[k] != b[j]; k++)
				;
			l = CONS(make_one(&a[i], k-i), l);
			i = k;
			continue;
		}
		if (a[i] != b[j])
			return @':error';
		i++, j++;
	}
	if (i < ia || j < ib)
		return @':error';
	return l;
}

static cl_object
find_list_wilds(cl_object a, cl_object b)
{
	cl_object l;

	for (l = Cnil; !endp(b); a=CDR(a), b=CDR(b)) {
		if (CAR(b) == @':wild-inferiors') {
			cl_object l2 = Cnil;
			b = CDR(b);
			while (!path_list_match(a, b)) {
				l2 = CONS(CAR(a),l2);
				a = CDR(a);
			}
			l = CONS(l2, l);
		} else {
			l = find_wilds(l, CAR(a), CAR(b));
		}
	}
	return @nreconc(2, l, Cnil);
}		

static cl_object
copy_wildcards(cl_object *wilds_list, cl_object pattern)
{
	char *s;
	cl_index i, l, j;
	bool new_string;
	cl_object wilds = *wilds_list;

	if (pattern == @':wild') {
		if (endp(wilds))
			return @':error';
		pattern = CAR(wilds);
		*wilds_list = CDR(wilds);
		return pattern;
	}
	if (pattern == @':wild-inferiors')
		return @':error';
	if (type_of(pattern) != t_string)
		return pattern;

	new_string = FALSE;
	s = pattern->string.self;
	l = pattern->string.fillp;
	cl_token->string.fillp = 0;

	for (j = i = 0; i < l; ) {
		if (s[i] != '*') {
			i++;
			continue;
		}
		if (i != j)
			push_c_string(cl_token, &s[j], i-j);
		new_string = TRUE;
		if (endp(wilds))
			return @':error';
		push_string(cl_token, CAR(wilds));
		wilds = CDR(wilds);
		j = i++;
	}
	/* Only create a new string when needed */
	if (new_string)
		pattern = copy_simple_string(cl_token);
	*wilds_list = wilds;
	return pattern;
}

static cl_object
copy_list_wildcards(cl_object *wilds, cl_object to)
{
	cl_object l = Cnil;

	while (!endp(to)) {
		cl_object d, mask = CAR(to);
		if (mask == @':wild-inferiors') {
			if (CONSP(CAR(*wilds)))
				l = append(CAR(*wilds), l);
			else
				return @':error';
			*wilds = CDR(*wilds);
		} else {
			d = copy_wildcards(wilds, CAR(to));
			if (d == @':error')
				return d;
			l = CONS(d, l);
		}
		to = CDR(to);
	}
	if (CONSP(l))
		l = @nreconc(2, l, Cnil);
}

cl_object
translate_pathname(cl_object source, cl_object from, cl_object to)
{
	cl_object wilds, out, d, *pc;

	source = coerce_to_pathname(source);
	from = coerce_to_pathname(from);
	to = coerce_to_pathname(to);

	if (source->pathname.logical != from->pathname.logical)
		goto error;
	out = cl_alloc_object(t_pathname);
	out->pathname.logical = to->pathname.logical;

	/* Match host names */
	if (!equal(source->pathname.host, from->pathname.host))
		goto error;
	out->pathname.host = to->pathname.host;

	/* Match devices */
	if (!equal(source->pathname.device, from->pathname.device))
		goto error;
	out->pathname.device = to->pathname.device;

	/* Match directories */
	wilds = find_list_wilds(source->pathname.directory,
				from->pathname.directory);
	if (wilds == @':error')	goto error;
	d = copy_list_wildcards(&wilds, to->pathname.directory);
	if (d == @':error') goto error;
	if (wilds != Cnil) goto error2;
	out->pathname.directory = d;

	/* Match name */
	wilds = find_wilds(Cnil, source->pathname.name, from->pathname.name);
	if (wilds == @':error') goto error2;
	d = copy_wildcards(&wilds, to->pathname.name);
	if (d == @':error') goto error;
	if (wilds != Cnil) goto error2;
	out->pathname.name = d;

	/* Match type */
	wilds = find_wilds(Cnil, source->pathname.type, from->pathname.type);
	if (wilds == @':error') goto error2;
	d = copy_wildcards(&wilds, to->pathname.type);
	if (d == @':error') goto error;
	if (wilds != Cnil) goto error2;
	out->pathname.type = d;

	/* Match version */
#if 0
	wilds = find_wilds(Cnil, source->pathname.version, from->pathname.version);
	if (wilds == @':error') goto error2;
	d = copy_wildcards(&wilds, to->pathname.version);
	if (d == @':error') goto error;
	if (wilds != Cnil) goto error2;
	out->pathname.version = d;
#else
	out->pathname.version = Cnil;
#endif
	return out;

 error:
	FEerror("~S is not a specialization of path ~S", 2, source, from);
 error2:
	FEerror("Number of wildcards in ~S do not match  ~S", 2, from, to);
}

@(defun translate_pathname (source from to)
@
	@(return translate_pathname(source, from, to))
@)

static cl_object
translate_logical_pathname(cl_object source)
{
	cl_object l, pair;
	source = coerce_to_pathname(source);
	if (!source->pathname.logical)
		goto error;
 begin:
	l = @si::pathname-translations(1, source->pathname.host, Cnil);
	for(; !endp(l); l = CDR(l)) {
		pair = CAR(l);
		if (pathname_match_p(source, CAR(pair))) {
			source = translate_pathname(source, CAR(pair), CADR(pair));
			if (source->pathname.logical)
				goto begin;
			return source;
		}
	}
 error:
	FEerror("~S admits no logical pathname translations", 1, source);
}

@(defun translate_logical_pathname (source)
@
	@(return translate_logical_pathname(source))
@)

void
init_pathname(void)
{
	register_root(&pathname_translations);
	SYM_VAL(@'*default-pathname-defaults*') =
	  make_pathname(Cnil, Cnil, Cnil, Cnil, Cnil, Cnil);
	@si::pathname-translations(2,make_simple_string("SYS"),
				   list(1,list(2,make_simple_string("*.*"),
					       make_simple_string("./*.*"))));
}
