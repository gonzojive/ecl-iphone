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

#include <string.h>
#include "ecl.h"
#include <ctype.h>
#ifndef MAXPATHLEN
# ifdef PATH_MAX
#   define MAXPATHLEN PATH_MAX
# else
#   error "Either MAXPATHLEN or PATH_MAX should be defined"
# endif
#endif

typedef int (*delim_fn)(int);

static cl_object
check_directory(cl_object directory, bool logical)
{
	/* INV: directory is always a list */
	cl_object ptr;
	int i;

	if (CAR(directory) != @':absolute'  && CAR(directory) != @':relative')
		return Cnil;
 BEGIN:
	for (i=0, ptr=directory; !endp(ptr); ptr = CDR(ptr), i++) {
		cl_object item = CAR(ptr);
		if (item == @':back') {
			if (i == 0)
				return @':error';
			item = nth(i-1, directory);
			if (item == @':absolute' || item == @':wild-inferiors')
				return @':error';
			if (i > 2)
				CDR(nthcdr(i-2, directory)) = CDR(ptr);
		} if (item == @':up') {
			if (i == 0)
				return @':error';
			item = nth(i-1, directory);
			if (item == @':absolute' || item == @':wild-inferiors')
				return @':error';
		} else if (item == @':relative' || item == @':absolute') {
			if (i > 0)
				return @':error';
		} else if (type_of(item) == t_string) {
			if (logical)
				continue;
			if (strcmp(item->string.self,".")==0) {
				if (i == 0)
					return @':error';
				CDR(nthcdr(i-1, directory)) = CDR(ptr);
			} else if (strcmp(item->string.self,"..") == 0) {
				CAR(directory) = @':back';
				goto BEGIN;
			}
		} else if (item != @':wild' && item != @':wild-inferiors') {
			return @':error';
		}
	}
	return directory;
}

cl_object
make_pathname(cl_object host, cl_object device, cl_object directory,
	      cl_object name, cl_object type, cl_object version)
{
	cl_object x;

	switch (type_of(directory)) {
	case t_string:
		directory = cl_list(2, @':absolute', directory);
		break;
	case t_symbol:
		if (directory == Cnil)
			break;
		if (directory == @':wild') {
			directory = cl_list(2, @':absolute', @':wild-inferiors');
			break;
		}
		goto ERROR;
	case t_cons: {
		cl_object aux = check_directory(cl_copy_list(directory), 1);
		if (aux != @':error') {
			directory = aux;
			break;
		}
	}
	default:
		goto ERROR;
	}
	x = cl_alloc_object(t_pathname);
	if (type_of(host) == t_string)
		x->pathname.logical = logical_hostname_p(host);
	else if (host == Cnil)
		x->pathname.logical = FALSE;
	else
		goto ERROR;
	if (device != Cnil && device != @':unspecific' &&
	    !(!x->pathname.logical && type_of(device) == t_string))
		goto ERROR;
	if (name != Cnil && name != @':wild' && type_of(name) != t_string)
		goto ERROR;
	if (type != Cnil && type != @':wild' && type_of(type) != t_string)
		goto ERROR;
	if (version != @':unspecific' && version != @':newest' &&
	    version != @':wild' && version != Cnil && !FIXNUMP(version))
	{
	ERROR:	cl_error(3, @'file-error', @':pathname',
			 cl_list(13, @'make-pathname',
				 @':host', host,
				 @':device', device,
				 @':directory', directory,
				 @':name', name,
				 @':type', type,
				 @':version', version));
	}
	x->pathname.host = host;
	x->pathname.device = device;
	x->pathname.directory = directory;
	x->pathname.name = name;
	x->pathname.type = type;
	x->pathname.version = version;
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
#define WORD_ALLOW_LEADING_DOT 16

static cl_object
make_one(const char *s, cl_index end)
{
	cl_object x = cl_alloc_simple_string(end);
	memcpy(x->string.self, s, end);
	return(x);
}

static int is_colon(int c) { return c == ':'; }
static int is_slash(int c) { return IS_DIR_SEPARATOR(c); }
static int is_semicolon(int c) { return c == ';'; }
static int is_dot(int c) { return c == '.'; }
static int is_null(int c) { return c == '\0'; }

static int
is_all_upper(cl_object s)
{
	int i;
	const char *text;
	for (i = 0, text = s->string.self; i <= s->string.dim; i++) {
		if (!isupper(text[i]))
			return 0;
	}
	return 1;
}

static int
is_all_lower(cl_object s)
{
	int i;
	const char *text;
	for (i = 0, text = s->string.self; i <= s->string.dim; i++) {
		if (!islower(text[i]))
			return 0;
	}
	return 1;
}

/*
 * Translates a string into the host's preferred case.
 * See CLHS 19.2.2.1.2.2 Common Case in Pathname Components.
 */

static cl_object
translate_common_case(cl_object str)
{
	if (type_of(str) != t_string) {
		/* Pathnames may contain some other objects, such as symbols,
		 * numbers, etc, which need not be translated */
		return str;
	} else if (is_all_upper(str)) {
		/* We use UN*X conventions, so lower case is default.
		 * However, this really should be conditionalised to the OS type,
		 * and it should translate to the _local_ case.
		 */
		return cl_string_downcase(1, str);
	} else if (is_all_lower(str)) {
		/* We use UN*X conventions, so lower case is default.
		 * However, this really should be conditionalised to the OS type,
		 * and it should translate to _opposite_ of the local case.
		 */
		return cl_string_upcase(1, str);
	} else {
		/* Mixed case goes unchanged */
		return str;
	}
}

static cl_object
translate_pathname_case(cl_object str, cl_object scase)
{
	if (scase == @':common') {
		return translate_common_case(str);
	} else if (scase == @':local') {
		return str;
	} else {
		FEerror("~S is not a valid pathname case specificer.~S"
			"Only :COMMON or :LOCAL are accepted.", 1, scase);
	}
}

static cl_object
translate_directory_case(cl_object list, cl_object scase)
{
	/* If the argument is really a list, translate all strings in it and
	 * return this new list, else assume it is a string and translate it.
	 */
	if (!CONSP(list)) {
		return translate_pathname_case(list,scase);
	} else {
		cl_object l;
		list = cl_copy_list(list);
		for (l = cl_copy_list(list); !endp(l); l = CDR(l)) {
			/* It is safe to pass anything to translate_pathname_case,
			 * because it will only transform strings, leaving other
			 * object (such as symbols) unchanged.*/
			CAR(l) = translate_pathname_case(CAR(l), scase);
		}
		return list;
	}
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
parse_word(const char *s, delim_fn delim, int flags, cl_index start,
	   cl_index end, cl_index *end_of_word)
{
	cl_index i, j;
	bool wild_inferiors = FALSE;

	i = j = start;
	if ((flags & WORD_ALLOW_LEADING_DOT) && (i < end) && delim(s[i]))
		i++;
	for (; i < end && !delim(s[i]); i++) {
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
		return cl_core.null_string;
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
	delim_fn delim = (flags & WORD_LOGICAL) ? is_semicolon : is_slash;

	flags |= WORD_INCLUDE_DELIM | WORD_ALLOW_ASTERISK;
	*end_of_dir = start;
	for (i = j = start; i < end; j = i) {
		cl_object part = parse_word(s, delim, flags, j, end, &i);
		if (part == @':error' || part == Cnil)
			break;
		if (part == cl_core.null_string) {  /* "/", ";" */
			if (j != start) {
				if (flags & WORD_LOGICAL)
					return @':error';
				continue;
			}
			part = (flags & WORD_LOGICAL) ? @':relative' : @':absolute';
		}
		*end_of_dir = i;
		plast = &CDR(*plast = CONS(part, Cnil));
	}
	return path;
}

bool
logical_hostname_p(cl_object host)
{
	if (type_of(host) != t_string)
		return FALSE;
	return !Null(@assoc(4, host, cl_core.pathname_translations, @':test', @'string-equal'));
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
	cl_object host, device, path, name, type, aux, version;
	bool logical;

	/* We first try parsing as logical-pathname. In case of
	 * failure, physical-pathname parsing is performed only when
	 * there is no supplied *logical* host name. All other failures
	 * result in Cnil as output.
	 */
	host = parse_word(s, is_colon, WORD_LOGICAL | WORD_INCLUDE_DELIM,
			  start, end, ep);
	if (default_host != Cnil) {
		if (host == Cnil || host == @':error')
			host = default_host;
	}
	if (!logical_hostname_p(host))
		goto physical;
	/*
	 * Logical pathname format:
	 *	[logical-hostname:][;][logical-directory-component;][pathname-name][.pathname-type]
	 */
	logical = TRUE;
	device = @':unspecific';
	path = parse_directories(s, WORD_LOGICAL, *ep, end, ep);
	if (CONSP(path)) {
		if (CAR(path) != @':relative' && CAR(path) != @':absolute')
			path = CONS(@':absolute', path);
		path = check_directory(path, TRUE);
	}
	if (path == @':error')
		return Cnil;
	name = parse_word(s, is_dot, WORD_LOGICAL | WORD_ALLOW_ASTERISK |
			  WORD_EMPTY_IS_NIL, *ep, end, ep);
	if (name == @':error')
		return Cnil;
	type = Cnil;
	version = Cnil;
	if (*ep == start || s[*ep-1] != '.')
		goto make_it;
	type = parse_word(s, is_dot, WORD_LOGICAL | WORD_ALLOW_ASTERISK |
			  WORD_EMPTY_IS_NIL, *ep, end, ep);
	if (type == @':error')
		return Cnil;
	if (*ep == start || s[*ep-1] != '.')
		goto make_it;
	aux = parse_word(s, is_null, WORD_LOGICAL | WORD_ALLOW_ASTERISK |
			 WORD_EMPTY_IS_NIL, *ep, end, ep);
	if (aux == @':error') {
		return Cnil;
	} else if (SYMBOLP(aux)) {
		version = aux;
	} else {
		version = cl_parse_integer(3, aux, @':junk-allowed', Ct);
		if (cl_integerp(version) != Cnil && number_plusp(version) &&
		    fix(VALUES(1)) == aux->string.fillp)
			;
		else if (string_equal(aux, (@':newest')->symbol.name))
			version = @':newest';
		else
			return Cnil;
	}
	goto make_it;
 physical:
	/*
	 * Physical pathname format:
	 *	[device:][[//hostname]/][directory-component/]*[pathname-name][.pathname-type]
	 */
	logical = FALSE;
	device = parse_word(s, is_colon, WORD_INCLUDE_DELIM|WORD_EMPTY_IS_NIL,
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
	if (start <= end - 2 && is_slash(s[start]) && is_slash(s[start+1])) {
		host = parse_word(s, is_slash, WORD_EMPTY_IS_NIL,
				  start+2, end, ep);
		if (host != Cnil) {
			start = *ep;
			if (is_slash(s[--start])) *ep = start;
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
	if (CONSP(path)) {
		if (CAR(path) != @':relative' && CAR(path) != @':absolute')
			path = CONS(@':relative', path);
		path = tilde_expand(path);
		path = check_directory(path, FALSE);
	}
	if (path == @':error')
		return Cnil;
	name = parse_word(s, is_dot, WORD_ALLOW_LEADING_DOT |
			  WORD_ALLOW_ASTERISK | WORD_EMPTY_IS_NIL,
			  *ep, end, ep);
	if (name == @':error')
		return Cnil;
	if (*ep == start || s[*ep-1] != '.') {
		type = Cnil;
	} else {
		type = parse_word(s, is_null, WORD_ALLOW_ASTERISK, *ep, end, ep);
		if (type == @':error')
			return Cnil;
	}
	version = @':newest';
 make_it:
	if (*ep >= end) *ep = end;
	path = make_pathname(host, device, path, name, type, version);
	path->pathname.logical = logical;
	return path;
}

cl_object
si_default_pathname_defaults(void)
{
	/* This routine outputs the value of *default-pathname-defaults*
	 * coerced to type PATHNAME. Special care is taken so that we do
	 * not enter an infinite loop when using PARSE-NAMESTRING, because
	 * this routine might itself try to use the value of this variable. */
	cl_object path = symbol_value(@'*default-pathname-defaults*');
	if (type_of(path) == t_string) {
		/* Avoids infinite loop by giving a third argument to
		 * parse-namestring */
		path = cl_parse_namestring(3, path, Cnil, Cnil);
	} else {
		path = cl_pathname(path);
	}
	@(return path)
}

cl_object
cl_pathname(cl_object x)
{
L:
	switch (type_of(x)) {
	case t_string:
		x = cl_parse_namestring(1, x);

	case t_pathname:
		break;

	case t_stream:
		switch ((enum ecl_smmode)x->stream.mode) {
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
		default:
			;/* Fall through to error message */
		}
	default:
		FEwrong_type_argument(cl_list(4, @'or', @'file-stream',
					      @'string', @'pathname'),
				      x);
	}
	@(return x)
}

cl_object
cl_logical_pathname(cl_object x)
{
	x = cl_pathname(x);
	if (!x->pathname.logical) {
		cl_error(9, @'simple-type-error', @':format-control',
			 make_simple_string("~S cannot be coerced to a logical pathname."),
			 @':format-arguments', cl_list(1, x),
			 @':expected-type', @'logical-pathname',
			 @':datum', x);
	}
	@(return x);
}

/* FIXME! WILD-PATHNAME-P is missing! */
@(defun wild-pathname-p (pathname &optional component)
	bool checked = 0;
@
	pathname = cl_pathname(pathname);
	if (component == Cnil || component == @':host') {
		if (pathname->pathname.host == @':wild')
			@(return Ct);
		checked = 1;
	}
	if (component == Cnil || component == @':device') {
		if (pathname->pathname.device == @':wild')
			@(return Ct);
		checked = 1;
	}
	if (component == Cnil || component == @':version') {
		if (pathname->pathname.version == @':wild')
			@(return Ct);
		checked = 1;
	}
	if (component == Cnil || component == @':name') {
		cl_object name = pathname->pathname.name;
		if (name != Cnil &&
		    (name == @':wild' || (!SYMBOLP(name) && member_char('*', name))))
			@(return Ct);
		checked = 1;
	}
	if (component == Cnil || component == @':type') {
		cl_object name = pathname->pathname.type;
		if (name != Cnil &&
		    (name == @':wild' || (!SYMBOLP(name) && member_char('*', name))))
			@(return Ct);
		checked = 1;
	}
	if (component == Cnil || component == @':directory') {
		cl_object list = pathname->pathname.directory;
		checked = 1;
		while (list != Cnil) {
			cl_object name = CAR(list);
			if (name != Cnil &&
			    (name == @':wild' || name == @':wild-inferiors' ||
			     (!SYMBOLP(name) && member_char('*', name))))
			{
				@(return Ct)
			}
			list = CDR(list);
		}
	}
	if (checked == 0) {
		FEerror("~A is not a valid pathname component", 1, component);
	}
	@(return Cnil)
@)

/*
 * coerce_to_file_pathname(P) converts P to a physical pathname,
 * for a file which is accesible in our filesystem.
 * INV: Wildcards are allowed.
 * INV: A fresh new copy of the pathname is created.
 */
cl_object
coerce_to_file_pathname(cl_object pathname)
{
	pathname = coerce_to_physical_pathname(pathname);
	pathname = cl_merge_pathnames(1, pathname);
#if !defined(cygwin) && !defined(mingw32)
	if (pathname->pathname.device != Cnil)
		FEerror("Device ~S not yet supported.", 1,
			pathname->pathname.device);
	if (pathname->pathname.host != Cnil)
		FEerror("Access to remote files not yet supported.", 0);
#endif
	return pathname;
}

/*
 * coerce_to_physical_pathname(P) converts P to a physical pathname,
 * performing the appropiate transformation if P was a logical pathname.
 */
cl_object
coerce_to_physical_pathname(cl_object x)
{
	x = cl_pathname(x);
	if (x->pathname.logical)
		return cl_translate_logical_pathname(1, x);
	return x;
}

/*
 * si_coerce_to_filename(P) converts P to a physical pathname and then to
 * a namestring. The output must always be a simple-string which can
 * be used by the C library.
 * INV: No wildcards are allowed.
 */
cl_object
si_coerce_to_filename(cl_object pathname)
{
	cl_object namestring;

	pathname = coerce_to_file_pathname(pathname);
	assert_non_wild_pathname(pathname);
	namestring = coerce_to_simple_string(cl_namestring(pathname));
	if (namestring->string.fillp >= MAXPATHLEN - 16)
		FEerror("Too long filename: ~S.", 1, namestring);
	return namestring;
}

#define default_device(host) Cnil

cl_object
merge_pathnames(cl_object path, cl_object defaults, cl_object default_version)
{
	cl_object host, device, directory, name, type, version;

	defaults = cl_pathname(defaults);
	path = cl_parse_namestring(1, path, Cnil, defaults);
	if (Null(host = path->pathname.host))
		host = defaults->pathname.host;
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
	if (Null(name = path->pathname.name))
		name = defaults->pathname.name;
	if (Null(type = path->pathname.type))
		type = defaults->pathname.type;
	version = path->pathname.version;
	if (Null(path->pathname.name)) {
		if (Null(version))
			version = defaults->pathname.version;
	}
	if (Null(version))
		version = default_version;
	/*
		In this implementation, version is not considered
	*/
	defaults = make_pathname(host, device, directory, name, type, version);
	return defaults;
}

static void
push_c_string(cl_object buffer, const char *s, cl_index length)
{
	for (; length; length--, s++) {
		ecl_string_push_extend(buffer, *s);
	}
}

static void
push_string(cl_object buffer, cl_object string)
{
	string = cl_string(string);
	push_c_string(buffer, string->string.self, string->string.fillp);
}

/*
	ecl_namestring(x, flag) converts a pathname to a namestring.
	if flag is true, then the pathname may be coerced to the requirements
	of the filesystem, removing fields that have no meaning (such as
	version, or type, etc); otherwise, when it is not possible to
	produce a readable representation of the pathname, NIL is returned.
*/
cl_object
ecl_namestring(cl_object x, int truncate_if_unreadable)
{
	bool logical;
	cl_object l, y;
	cl_object buffer, host;

	x = cl_pathname(x);

	/* INV: Pathnames can only be created by mergin, parsing namestrings
	 * or using make_pathname(). In all of these cases ECL will complain
	 * at creation time if the pathname has wrong components.
	 */
	buffer = make_string_output_stream(128);
	logical = x->pathname.logical;
	host = x->pathname.host;
	if (logical) {
		if ((y = x->pathname.device) != @':unspecific' &&
		    truncate_if_unreadable)
			return Cnil;
		if (host != Cnil) {
			si_do_write_sequence(host, buffer, MAKE_FIXNUM(0), Cnil);
			writestr_stream(":", buffer);
		}
	} else {
		if ((y = x->pathname.device) != Cnil) {
			si_do_write_sequence(y, buffer, MAKE_FIXNUM(0), Cnil);
			writestr_stream(":", buffer);
		}
		if (host != Cnil) {
			if (y == Cnil) {
				writestr_stream("file:", buffer);
			}
			writestr_stream("//", buffer);
			si_do_write_sequence(host, buffer, MAKE_FIXNUM(0), Cnil);
		}
	}
	l = x->pathname.directory;
	if (endp(l))
		goto NO_DIRECTORY;
	y = CAR(l);
	if (y == @':relative') {
		if (logical)
			writestr_stream(";", buffer);
	} else {
		if (!logical)
			writestr_stream("/", buffer);
	}
	for (l = CDR(l); !endp(l); l = CDR(l)) {
		y = CAR(l);
		if (y == @':up') {
			writestr_stream("..", buffer);
		} else if (y == @':wild') {
			writestr_stream("*", buffer);
		} else if (y == @':wild-inferiors') {
			writestr_stream("**", buffer);
		} else if (y != @':back') {
			si_do_write_sequence(y, buffer, MAKE_FIXNUM(0), Cnil);
		} else {
			FEerror("Directory :back has no namestring representation",0);
		}
		writestr_stream(logical? ";" : "/", buffer);
	}
NO_DIRECTORY:
	y = x->pathname.name;
	if (y != Cnil) {
		if (y == @':wild') {
			writestr_stream("*", buffer);
		} else {
			si_do_write_sequence(y, buffer, MAKE_FIXNUM(0), Cnil);
		}
	}
	y = x->pathname.type;
	if (y != Cnil) {
		if (y == @':wild') {
			writestr_stream(".*", buffer);
		} else {
			writestr_stream(".", buffer);
			si_do_write_sequence(y, buffer, MAKE_FIXNUM(0), Cnil);
		}
	}
	y = x->pathname.version;
	if (logical) {
		if (y != Cnil) {
			writestr_stream(".", buffer);
			if (y == @':wild') {
				writestr_stream("*", buffer);
			} else if (y == @':newest') {
				si_do_write_sequence(y->symbol.name, buffer,
						     MAKE_FIXNUM(0), Cnil);
			} else {
				/* Since the printer is not reentrant,
				 * we cannot use cl_write and friends.
				 */
				int n = fix(y), i;
				char b[FIXNUM_BITS/2];
				for (i = 0; n; i++) {
					b[i] = n%10 + '0';
					n = n/10;
				}
				if (i == 0)
					b[i++] = '0';
				while (i--) {
					writec_stream(b[i], buffer);
				}
			}
		}
	} else if (y != @':newest' && !truncate_if_unreadable) {
		return Cnil;
	}
	return get_output_stream_string(buffer);
}

cl_object
cl_namestring(cl_object x)
{
	@(return ecl_namestring(x, 1))
}

@(defun parse_namestring (thing
	&o host (defaults si_default_pathname_defaults())
	&k (start MAKE_FIXNUM(0)) end junk_allowed
	&a output)
	cl_index s, e, ee;
@
	if (host != Cnil) {
		host = cl_string(host);
	}			       
	if (type_of(thing) != t_string) {
		output = cl_pathname(thing);
	} else {
		cl_object default_host = host;
		if (default_host == Cnil && defaults != Cnil) {
			defaults = cl_pathname(defaults);
			default_host = defaults->pathname.host;
		}
		get_string_start_end(thing, start, end, &s, &e);
		output = parse_namestring(thing->string.self, s, e - s, &ee,
					  default_host);
		start = MAKE_FIXNUM(s + ee);
		if (output == Cnil || ee != e - s) {
			if (Null(junk_allowed)) {
				FEparse_error("Cannot parse the namestring ~S~%"
					      "from ~S to ~S.", Cnil,
					      3, thing, start, end);
			}
			goto OUTPUT;
		}
	}
	if (host != Cnil && output->pathname.host != host) {
		FEerror("The pathname ~S does not contain the required host ~S.",
			1, thing, host);
	}
  OUTPUT:
	@(return output start)
@)

@(defun merge_pathnames (path
	&o (defaults si_default_pathname_defaults())
 	   (default_version @':newest'))
@
	path = cl_pathname(path);
	defaults = cl_pathname(defaults);
	@(return merge_pathnames(path, defaults, default_version))
@)

@(defun make_pathname (&key (host OBJNULL) (device OBJNULL) (directory OBJNULL)
			    (name OBJNULL) (type OBJNULL) (version OBJNULL)
		            ((:case scase) @':local')
		            defaults
		       &aux x)
@
	if (Null(defaults)) {
		defaults = si_default_pathname_defaults();
		defaults = make_pathname(defaults->pathname.host,
					 Cnil, Cnil, Cnil, Cnil, Cnil);
	} else {
		defaults = cl_pathname(defaults);
	}
	x = make_pathname(host != OBJNULL? translate_pathname_case(host,scase)
			                 : defaults->pathname.host,
			  device != OBJNULL? translate_pathname_case(device,scase)
			                   : defaults->pathname.device,
			  directory != OBJNULL? translate_directory_case(directory,scase)
			                      : defaults->pathname.directory,
			  name != OBJNULL? translate_pathname_case(name,scase)
			                      : defaults->pathname.name,
			  type != OBJNULL? translate_pathname_case(type,scase)
                                              : defaults->pathname.type,
			  version != OBJNULL? version : defaults->pathname.version);
	@(return x)
@)

cl_object
cl_pathnamep(cl_object pname)
{
	@(return ((type_of(pname) == t_pathname)? Ct : Cnil))
}

cl_object
si_logical_pathname_p(cl_object pname)
{
	@(return ((type_of(pname) == t_pathname && pname->pathname.logical)?
		  Ct : Cnil))
}

@(defun pathname_host (pname &key ((:case scase) @':local'))
@
	pname = cl_pathname(pname);
	@(return translate_pathname_case(pname->pathname.host,scase))
@)

@(defun pathname_device (pname &key ((:case scase) @':local'))
@
	pname = cl_pathname(pname);
	@(return translate_pathname_case(pname->pathname.device,scase))
@)

@(defun pathname_directory (pname &key ((:case scase) @':local'))
@
	pname = cl_pathname(pname);
        @(return translate_directory_case(pname->pathname.directory,scase))
@)

@(defun pathname_name(pname &key ((:case scase) @':local'))
@
	pname = cl_pathname(pname);
	@(return translate_pathname_case(pname->pathname.name,scase))
@)

@(defun pathname_type(pname &key ((:case scase) @':local'))
@
	pname = cl_pathname(pname);
        @(return translate_pathname_case(pname->pathname.type,scase))
@)

cl_object
cl_pathname_version(cl_object pname)
{
	pname = cl_pathname(pname);
	@(return  pname->pathname.version)
}

cl_object
cl_file_namestring(cl_object pname)
{
	pname = cl_pathname(pname);
	@(return ecl_namestring(make_pathname(Cnil, Cnil, Cnil,
					      pname->pathname.name,
					      pname->pathname.type,
					      pname->pathname.version),
				1))
}

cl_object
cl_directory_namestring(cl_object pname)
{
	pname = cl_pathname(pname);
	@(return ecl_namestring(make_pathname(Cnil, Cnil,
					      pname->pathname.directory,
					      Cnil, Cnil, Cnil),
				1))
}

cl_object
cl_host_namestring(cl_object pname)
{
	pname = cl_pathname(pname);
	pname = pname->pathname.host;
	if (Null(pname) || pname == @':wild')
		pname = cl_core.null_string;
	@(return pname)
}

@(defun enough_namestring (path
	&o (defaults si_default_pathname_defaults()))
	cl_object newpath;
@
	defaults = cl_pathname(defaults);
	path = cl_pathname(path);
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
	@(return ecl_namestring(newpath, 1))
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
	if (mask == @':wild')
		return TRUE;
	if (type_of(a) != t_string || mask == Cnil)
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
		mask = CDR(mask);
		if (item_mask == @':wild-inferiors') {
			if (endp(mask))
				return TRUE;
			while (!endp(a)) {
				if (path_list_match(a, mask))
					return TRUE;
				a = CDR(a);
			}
			return FALSE;
		} else if (endp(a)) {
			/* A NIL directory should match against :absolute
			   or :relative, in order to perform suitable translations. */
			if (item_mask != @':absolute' && item_mask != @':relative')
				return FALSE;
		} else if (!path_item_match(CAR(a), item_mask)) {
			return FALSE;
		} else {
			a = CDR(a);
		}
	}
	if (!endp(a))
		return FALSE;
	return TRUE;
}

cl_object
cl_pathname_match_p(cl_object path, cl_object mask)
{
	path = cl_pathname(path);
	mask = cl_pathname(mask);
	if (path->pathname.logical != mask->pathname.logical)
		return Cnil;
#if 0
	/* INV: This was checked in the calling routine */
	if (!path_item_match(path->pathname.host, mask->pathname.host))
		return Cnil;
#endif
	if (!path_list_match(path->pathname.directory, mask->pathname.directory))
		return Cnil;
	if (!path_item_match(path->pathname.name, mask->pathname.name))
		return Cnil;
	if (!path_item_match(path->pathname.type, mask->pathname.type))
		return Cnil;
	if (!path_item_match(path->pathname.version, mask->pathname.version))
		return Cnil;
	return Ct;
}

/* --------------- PATHNAME TRANSLATIONS ------------------ */

static cl_object
coerce_to_from_pathname(cl_object x, cl_object host)
{
	switch (type_of(x)) {
	case t_string:
		x = cl_parse_namestring(2, x, host);
	case t_pathname:
		if (x->pathname.logical)
			return x;
	default:
		FEerror("~S is not a valid from-pathname translation", 1, x);
	}
}

@(defun si::pathname_translations (host &optional (set OBJNULL))
	cl_index parsed_length, length;
	cl_object pair, l;
@
	/* Check that host is a valid host name */
	assert_type_string(host);
	length = host->string.fillp;
	parse_word(host->string.self, is_null, WORD_LOGICAL, 0, length,
		   &parsed_length);
	if (parsed_length < host->string.fillp)
		FEerror("Wrong host syntax ~S", 1, host);

	/* Find its translation list */
	pair = @assoc(4, host, cl_core.pathname_translations, @':test', @'string-equal');
	if (set == OBJNULL)
		@(return ((pair == Cnil)? Cnil : CADR(pair)))

	/* Set the new translation list */
	assert_type_list(set);
	if (pair == Cnil) {
		pair = CONS(host, CONS(Cnil, Cnil));
		cl_core.pathname_translations = CONS(pair, cl_core.pathname_translations);
	}
	for (l = set, set = Cnil; !endp(l); l = CDR(l)) {
		cl_object item = CAR(l);
		cl_object from = coerce_to_from_pathname(cl_car(item), host);
		cl_object to = cl_pathname(cl_cadr(item));
		set = CONS(CONS(from, CONS(to, Cnil)), set);
	}
	CADR(pair) = @nreverse(set);
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
find_list_wilds(cl_object a, cl_object mask)
{
	cl_object l = Cnil, l2;

	while (!endp(mask)) {
		cl_object item_mask = CAR(mask);
		mask = CDR(mask);
		if (item_mask == @':wild-inferiors') {
			l2 = Cnil;
			while (!path_list_match(a, mask)) {
				if (endp(a))
					return @':error';
				l2 = CONS(CAR(a),l2);
				a = CDR(a);
			}
			l = CONS(l2, l);
		} else if (endp(a)) {
			/* A NIL directory should match against :absolute
			   or :relative, in order to perform suitable translations. */
			if (item_mask != @':absolute' && item_mask != @':relative')
				return @':error';
		} else {
			l2 = find_wilds(l, CAR(a), item_mask);
			if (l == @':error')
				return @':error';
			if (!Null(l2))
				l = CONS(l2, l);
			a = CDR(a);
		}
	}
	return @nreverse(l);
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
	cl_env.token->string.fillp = 0;

	for (j = i = 0; i < l; ) {
		if (s[i] != '*') {
			i++;
			continue;
		}
		if (i != j)
			push_c_string(cl_env.token, &s[j], i-j);
		new_string = TRUE;
		if (endp(wilds))
			return @':error';
		push_string(cl_env.token, CAR(wilds));
		wilds = CDR(wilds);
		j = i++;
	}
	/* Only create a new string when needed */
	if (new_string)
		pattern = copy_simple_string(cl_env.token);
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
			cl_object list = *wilds;
			if (endp(list))
				return @':error';
			else {
				cl_object dirlist = CAR(list);
				if (CONSP(dirlist))
					l = append(CAR(list), l);
				else if (!Null(CAR(list)))
					return @':error';
			}
			*wilds = CDR(list);
		} else {
			d = copy_wildcards(wilds, CAR(to));
			if (d == @':error')
				return d;
			l = CONS(d, l);
		}
		to = CDR(to);
	}
	if (CONSP(l))
		l = @nreverse(l);
	return l;
}

@(defun translate-pathname (source from to &key)
	cl_object wilds, out, d;
@
	source = cl_pathname(source);
	from = cl_pathname(from);
	to = cl_pathname(to);

	if (source->pathname.logical != from->pathname.logical)
		goto error;
	out = cl_alloc_object(t_pathname);
	out->pathname.logical = to->pathname.logical;

	/* Match host names */
	if (!cl_string_equal(2, source->pathname.host, from->pathname.host))
		goto error;
	out->pathname.host = to->pathname.host;

	/* Logical pathnames do not have devices. We just overwrite it. */
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
	if (to->pathname.version == @':wild') {
		if (out->pathname.version == @':wild')
			out->pathname.version = source->pathname.version;
		else
			goto error2;
	} else {
		out->pathname.version = to->pathname.version;
	}
	return out;

 error:
	FEerror("~S is not a specialization of path ~S", 2, source, from);
 error2:
	FEerror("Number of wildcards in ~S do not match  ~S", 2, from, to);
@)

@(defun translate-logical-pathname (source &key)
	cl_object l, pair;
	cl_object pathname;
@
	pathname = cl_pathname(source);
 begin:
	if (!pathname->pathname.logical) {
		@(return pathname)
	}
	l = @si::pathname-translations(1, pathname->pathname.host);
	for(; !endp(l); l = CDR(l)) {
		pair = CAR(l);
		if (!Null(cl_pathname_match_p(pathname, CAR(pair)))) {
			pathname = cl_translate_pathname(3, pathname, CAR(pair),
							 CADR(pair));
			goto begin;
		}
	}
	FEerror("~S admits no logical pathname translations", 1, pathname);
@)
