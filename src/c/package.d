/*
    package.d -- Packages.
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

#include <ecl/ecl.h>
#include <ecl/internal.h>

/******************************* ------- ******************************/
/*
 * NOTE 1: we only need to use the package locks when reading/writing the hash
 * tables, or changing the fields of a package.  We do not need the locks to
 * read lists from the packages (i.e. list of shadowing symbols, used
 * packages, etc), or from the global environment (cl_core.packages_list) if
 * we do not destructively modify them (For instance, use ecl_remove_eq
 * instead of ecl_delete_eq).
 */
/*
 * NOTE 2: Operations between locks must be guaranteed not fail, or, if
 * they signal an error, they should undo all locks they had before.
 */

#define	INTERNAL	1
#define	EXTERNAL	2
#define	INHERITED	3

static void
FEpackage_error(const char *message, cl_object package, int narg, ...)
{
	cl_va_list args;
	cl_va_start(args, narg, narg, 0);
	si_signal_simple_error(6,
			       @'package-error',
			       Cnil, /* not correctable */
			       make_constant_base_string(message), /* format control */
			       narg? cl_grab_rest_args(args) : cl_list(1,package), /* format args */
			       @':package', package); /* extra arguments */
}

void
CEpackage_error(const char *message, const char *continue_message, cl_object package, int narg, ...)
{
	cl_va_list args;
	cl_va_start(args, narg, narg, 0);
	si_signal_simple_error(6,
			       @'package-error',
			       make_constant_base_string(continue_message),
			       make_constant_base_string(message), /* format control */
			       narg? cl_grab_rest_args(args) : cl_list(1,package),
			       @':package', package);
}

static bool
member_string_eq(cl_object x, cl_object l)
{
	/* INV: l is a proper list */
	for (;  CONSP(l);  l = CDR(l))
		if (string_eq(x, CAR(l)))
			return(TRUE);
	return(FALSE);
}

/*
	Make_package(n, ns, ul) makes a package with name n,
	which must be a string or a symbol,
	and nicknames ns, which must be a list of strings or symbols,
	and uses packages in list ul, which must be a list of packages
	or package names i.e. strings or symbols.
*/
static cl_object
make_package_hashtable()
{
	cl_object h;
	cl_index hsize = 128;

	h = cl_alloc_object(t_hashtable);
	h->hash.lockable = 0;
	h->hash.test = htt_pack;
	h->hash.size = hsize;
	h->hash.rehash_size = make_singlefloat(1.5f);
	h->hash.threshold = make_singlefloat(0.75f);
	h->hash.factor = 0.7;
	h->hash.entries = 0;
	h->hash.data = NULL; /* for GC sake */
	h->hash.data = (struct ecl_hashtable_entry *)cl_alloc(hsize * sizeof(struct ecl_hashtable_entry));
	return cl_clrhash(h);
}

cl_object
make_package(cl_object name, cl_object nicknames, cl_object use_list)
{
	cl_object x, y, other;

	name = cl_string(name);
	assert_type_proper_list(nicknames);
	assert_type_proper_list(use_list);

	/* 1) Find a similarly named package in the list of packages to be
         *    created and use it.
	 */
	PACKAGE_OP_LOCK();
	if (cl_core.packages_to_be_created != OBJNULL) {
		cl_object *p = &cl_core.packages_to_be_created;
		for (x = *p; x != Cnil; ) {
			cl_object other_name = CAAR(x);
			if (equal(other_name, name) ||
			    funcall(5, @'member', other_name, nicknames,
				    @':test', @'string=') != Cnil)
			{
				*p = CDR(x);
				x = CDAR(x);
				goto INTERN;
			}
			p = &CDR(x);
			x = *p;
		}
	}

	/* 2) Otherwise, try to build a new package */
	if ((other = ecl_find_package_nolock(name)) != Cnil) {
	ERROR:	PACKAGE_OP_UNLOCK();
		CEpackage_error("A package with the name ~A already exists.",
				"Return existing package",
				other, 1, name);
		return other;
	}
	x = cl_alloc_object(t_package);
	x->pack.internal = make_package_hashtable();
	x->pack.external = make_package_hashtable();
	x->pack.name = name;
#ifdef ECL_THREADS
#if defined(_MSC_VER) || defined(mingw32)
	x->pack.lock = CreateMutex(NULL, FALSE, NULL);
#else
	{
	pthread_mutexattr_t attr;
	pthread_mutexattr_init(&attr);
	pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK_NP);
	pthread_mutex_init(&x->pack.lock, &attr);
	pthread_mutexattr_destroy(&attr);
	}
#endif /* _MSC_VER */
#endif
 INTERN:
	x->pack.nicknames = Cnil;
	x->pack.shadowings = Cnil;
	x->pack.uses = Cnil;
	x->pack.usedby = Cnil;
	x->pack.locked = FALSE;
	for (;  !endp(nicknames);  nicknames = CDR(nicknames)) {
		cl_object nick = cl_string(CAR(nicknames));
		if ((other = ecl_find_package_nolock(nick)) != Cnil) {
			name = nick;
			goto ERROR;
		}
		x->pack.nicknames = CONS(nick, x->pack.nicknames);
	}
	for (;  !endp(use_list);  use_list = CDR(use_list)) {
		y = si_coerce_to_package(CAR(use_list));
		x->pack.uses = CONS(y, x->pack.uses);
		y->pack.usedby = CONS(x, y->pack.usedby);
	}

	/* 3) Finally, add it to the list of packages */
	cl_core.packages = CONS(x, cl_core.packages);
	PACKAGE_OP_UNLOCK();
	return(x);
}

cl_object
rename_package(cl_object x, cl_object name, cl_object nicknames)
{
	cl_object y;

	name = cl_string(name);
	x = si_coerce_to_package(x);
	if (x->pack.locked)
		CEpackage_error("Cannot rename locked package ~S.",
				"Ignore lock and proceed", x, 0);

	PACKAGE_OP_LOCK();
	y = ecl_find_package_nolock(name);
	if ((y != Cnil) && (y != x)) {
	ERROR:	PACKAGE_OP_UNLOCK();
		FEpackage_error("A package with name ~S already exists.", x,
				1, name);
	}

	x->pack.name = name;
	x->pack.nicknames = Cnil;
	assert_type_proper_list(nicknames);
	for (;  !endp(nicknames);  nicknames = CDR(nicknames)) {
		cl_object nick = CAR(nicknames);
		y = ecl_find_package_nolock(nick);
		if (x == y)
			continue;
		if (y != Cnil) {
			name = nick;
			goto ERROR;
		}
		x->pack.nicknames = CONS(cl_string(nick), x->pack.nicknames);
	}
	PACKAGE_OP_UNLOCK();
	return(x);
}

/*
	ecl_find_package_nolock(n) seaches for a package with name n, where n is
	a valid string designator, or simply outputs n if it is a
	package.

	This is not a locking routine and someone may replace the list of
	packages while we are scanning it. Nevertheless, the list IS NOT
	be destructively modified, which means that we are on the safe side.
	Routines which need to ensure that the package list remains constant
	should enforce a global lock with PACKAGE_OP_LOCK().
*/
cl_object
ecl_find_package_nolock(cl_object name)
{
	cl_object l, p;

	if (type_of(name) == t_package)
		return name;
	name = cl_string(name);
	/* INV: cl_core.packages is a proper list */
	for (l = cl_core.packages; CONSP(l); l = CDR(l)) {
		p = CAR(l);
		if (string_eq(name, p->pack.name))
			return p;
		if (member_string_eq(name, p->pack.nicknames))
			return p;
	}
#ifdef ECL_RELATIVE_PACKAGE_NAMES
	/* Note that this function may actually be called _before_ symbols are set up
	 * and bound! */
	if (ecl_booted && SYM_VAL(@'si::*relative-package-names*') != Cnil) {
		return si_find_relative_package(1, name);
	}
#endif
	return Cnil;
}

cl_object
si_coerce_to_package(cl_object p)
{
	/* INV: ecl_find_package_nolock() signals an error if "p" is neither a package
	   nor a string */
	cl_object pp = ecl_find_package_nolock(p);
	if (Null(pp)) {
		FEpackage_error("There exists no package with name ~S", p, 0);
	}
	@(return pp);
}

cl_object
current_package(void)
{
	cl_object x;

	x = symbol_value(@'*package*');
	if (type_of(x) != t_package) {
		ECL_SETQ(@'*package*', cl_core.user_package);
		FEerror("The value of *PACKAGE*, ~S, was not a package",
			1, x);
	}
	return(x);
}

/*
	Intern(st, p) interns string st in package p.
*/
cl_object
_intern(const char *s, cl_object p)
{
	int intern_flag;
	cl_object str = make_constant_base_string(s);
	return intern(str, p, &intern_flag);
}

cl_object
intern(cl_object name, cl_object p, int *intern_flag)
{
	cl_object s, ul;

	/* FIXME! Symbols restricted to base string */
	name = ecl_check_cl_type(@'intern', name, t_base_string);
	p = si_coerce_to_package(p);
 TRY_AGAIN_LABEL:
	PACKAGE_LOCK(p);
	s = gethash_safe(name, p->pack.external, OBJNULL);
	if (s != OBJNULL) {
		*intern_flag = EXTERNAL;
		goto OUTPUT;
	}
	/* Keyword package has no intern section nor can it be used */
	if (p == cl_core.keyword_package) goto INTERN;
	s = gethash_safe(name, p->pack.internal, OBJNULL);
	if (s != OBJNULL) {
		*intern_flag = INTERNAL;
		goto OUTPUT;
	}
	for (ul=p->pack.uses; CONSP(ul); ul = CDR(ul)) {
		s = gethash_safe(name, CAR(ul)->pack.external, OBJNULL);
		if (s != OBJNULL) {
			*intern_flag = INHERITED;
			goto OUTPUT;
		}
	}
 INTERN:
	if (p->pack.locked) {
		PACKAGE_UNLOCK(p);
		CEpackage_error("Cannot intern symbol ~S in locked package ~S.",
				"Ignore lock and proceed", p, 2, name, p);
		goto TRY_AGAIN_LABEL;
	}
	s = make_symbol(name);
	s->symbol.hpack = p;
	*intern_flag = 0;
	if (p == cl_core.keyword_package) {
		s->symbol.stype = stp_constant;
		ECL_SET(s, s);
		sethash(name, p->pack.external, s);
	} else {
		sethash(name, p->pack.internal, s);
	}
 OUTPUT:
	PACKAGE_UNLOCK(p);
	return s;
}

/*
	ecl_find_symbol_nolock(st, len, p) searches for string st of length
	len in package p.
*/
cl_object
ecl_find_symbol_nolock(cl_object name, cl_object p, int *intern_flag)
{
	cl_object s, ul;

	/* FIXME! Symbols restricted to base string */
	name = ecl_check_cl_type(@'find-symbol', name, t_base_string);
	s = gethash_safe(name, p->pack.external, OBJNULL);
	if (s != OBJNULL) {
		*intern_flag = EXTERNAL;
		goto OUTPUT;
	}
	if (p == cl_core.keyword_package)
		goto NOTHING;
	s = gethash_safe(name, p->pack.internal, OBJNULL);
	if (s != OBJNULL) {
		*intern_flag = INTERNAL;
		goto OUTPUT;
	}
	for (ul=p->pack.uses; CONSP(ul); ul = CDR(ul)) {
		s = gethash_safe(name, CAR(ul)->pack.external, OBJNULL);
		if (s != OBJNULL) {
			*intern_flag = INHERITED;
			goto OUTPUT;
		}
	}
 NOTHING:
	*intern_flag = 0;
	s = Cnil;
 OUTPUT:
	return s;
}

cl_object
ecl_find_symbol(cl_object n, cl_object p, int *intern_flag)
{
	n = cl_string(n);
	p = si_coerce_to_package(p);
	PACKAGE_LOCK(p);
	n = ecl_find_symbol_nolock(n, p, intern_flag);
	PACKAGE_UNLOCK(p);
	return n;
}

bool
unintern(cl_object s, cl_object p)
{
	cl_object x, y, l, hash;
	bool output = FALSE;

	s = ecl_check_cl_type(@'unintern', s, t_symbol);

	p = si_coerce_to_package(p);

 TRY_AGAIN_LABEL:
	PACKAGE_LOCK(p);
	hash = p->pack.internal;
	x = gethash_safe(s->symbol.name, hash, OBJNULL);
	if (x == s)
		goto UNINTERN;
	hash = p->pack.external;
	x = gethash_safe(s->symbol.name, hash, OBJNULL);
	if (x != s)
		goto OUTPUT;
 UNINTERN:
	if (p->pack.locked) {
		PACKAGE_UNLOCK(p);
		CEpackage_error("Cannot unintern symbol ~S from locked package ~S.",
				"Ignore lock and proceed", p, 2, s, p);
		goto TRY_AGAIN_LABEL;
	}
	if (!member_eq(s, p->pack.shadowings))
		goto NOT_SHADOW;
	x = OBJNULL;
	for (l = p->pack.uses; CONSP(l); l = CDR(l)) {
		y = gethash_safe(s->symbol.name, CAR(l)->pack.external, OBJNULL);
		if (y != OBJNULL) {
			if (x == OBJNULL)
				x = y;
			else if (x != y) {
				PACKAGE_UNLOCK(p);
				FEpackage_error("Cannot unintern the shadowing symbol ~S~%"
						"from ~S,~%"
						"because ~S and ~S will cause~%"
						"a name conflict.", p, 4, s, p, x, y);
			}
		}
	}
	p->pack.shadowings = ecl_remove_eq(s, p->pack.shadowings);
 NOT_SHADOW:
	remhash(s->symbol.name, hash);
	if (s->symbol.hpack == p)
		s->symbol.hpack = Cnil;
	output = TRUE;
 OUTPUT:
	PACKAGE_UNLOCK(p);
	return output;
}

void
cl_export2(cl_object s, cl_object p)
{
	cl_object x, l, hash = OBJNULL;
	int intern_flag;

	s = ecl_check_cl_type(@'export', s, t_symbol);
	p = si_coerce_to_package(p);

	if (p->pack.locked)
		CEpackage_error("Cannot export symbol ~S from locked package ~S.",
				"Ignore lock and proceed", p, 2, s, p);
	PACKAGE_LOCK(p);
	x = ecl_find_symbol_nolock(s->symbol.name, p, &intern_flag);
	if (!intern_flag) {
		PACKAGE_UNLOCK(p);
		CEpackage_error("The symbol ~S is not accessible from ~S and cannot be exported.",
				"Import the symbol in the package and proceed.",
				p, 2, s, p);
	}
	if (x != s) {
		PACKAGE_UNLOCK(p);
		FEpackage_error("Cannot export the symbol ~S from ~S,~%"
				"because there is already a symbol with the same name~%"
				"in the package.", p, 2, s, p);
	}
	if (intern_flag == EXTERNAL)
		goto OUTPUT;
	if (intern_flag == INTERNAL)
		hash = p->pack.internal;
	for (l = p->pack.usedby; CONSP(l); l = CDR(l)) {
		x = ecl_find_symbol_nolock(s->symbol.name, CAR(l), &intern_flag);
		if (intern_flag && s != x &&
		    !member_eq(x, CAR(l)->pack.shadowings)) {
			PACKAGE_UNLOCK(p);
			FEpackage_error("Cannot export the symbol ~S~%"
					"from ~S,~%"
					"because it will cause a name conflict~%"
					"in ~S.", p, 3, s, p, CAR(l));
		}
	}
	if (hash != OBJNULL)
		remhash(s->symbol.name, hash);
	sethash(s->symbol.name, p->pack.external, s);
 OUTPUT:
	PACKAGE_UNLOCK(p);
}

cl_object
cl_delete_package(cl_object p)
{
	cl_object hash, list;
	cl_index i;

	/* 1) Try to remove the package from the global list */
	p = ecl_find_package_nolock(p);
	if (Null(p)) {
		CEpackage_error("Package ~S not found. Cannot delete it.",
				"Ignore error and continue", p, 0);
		@(return Cnil);
	}
	if (p->pack.locked)
		CEpackage_error("Cannot delete locked package ~S.",
				"Ignore lock and proceed", p, 0);
	if (p == cl_core.lisp_package || p == cl_core.keyword_package) {
		FEpackage_error("Cannot remove package ~S", p, 0);
	}

	/* 2) Now remove the package from the other packages that use it
	 *    and empty the package.
	 */
	if (Null(p->pack.name)) {
		@(return Cnil)
	}
	for (list = p->pack.uses; !endp(list); list = CDR(list))
		unuse_package(CAR(list), p);
	for (list = p->pack.usedby; !endp(list); list = CDR(list))
		unuse_package(p, CAR(list));
	PACKAGE_LOCK(p);
	for (hash = p->pack.internal, i = 0; i < hash->hash.size; i++)
		if (hash->hash.data[i].key != OBJNULL) {
			cl_object s = hash->hash.data[i].value;
			if (s->symbol.hpack == p)
				s->symbol.hpack = Cnil;
		}
	cl_clrhash(p->pack.internal);
	for (hash = p->pack.external, i = 0; i < hash->hash.size; i++)
		if (hash->hash.data[i].key != OBJNULL) {
			cl_object s = hash->hash.data[i].value;
			if (s->symbol.hpack == p)
				s->symbol.hpack = Cnil;
		}
	cl_clrhash(p->pack.external);
	p->pack.shadowings = Cnil;
	p->pack.name = Cnil;
	PACKAGE_UNLOCK(p);

	/* 2) Only at the end, remove the package from the list of packages. */
	PACKAGE_OP_LOCK();
	cl_core.packages = ecl_remove_eq(p, cl_core.packages);
	PACKAGE_OP_UNLOCK();
	@(return Ct)
}

void
cl_unexport2(cl_object s, cl_object p)
{
	int intern_flag;
	cl_object x;

	s = ecl_check_cl_type(@'unexport', s, t_symbol);
	p = si_coerce_to_package(p);
	if (p == cl_core.keyword_package)
		FEpackage_error("Cannot unexport a symbol from the keyword package.",
				cl_core.keyword_package, 0);
	if (p->pack.locked)
		CEpackage_error("Cannot unexport symbol ~S from locked package ~S.",
				"Ignore lock and proceed", p, 2, s, p);
	PACKAGE_LOCK(p);
	x = ecl_find_symbol_nolock(s->symbol.name, p, &intern_flag);
	if (intern_flag == 0) {
		PACKAGE_UNLOCK(p);
		FEpackage_error("Cannot unexport ~S because it does not belong to package ~S.",
				p, 2, s, p);
	}
	if (intern_flag != EXTERNAL) {
		/* According to ANSI & Cltl, internal symbols are
		   ignored in unexport */
		(void)0;
	} else {
		remhash(s->symbol.name, p->pack.external);
		sethash(s->symbol.name, p->pack.internal, s);
	}
	PACKAGE_UNLOCK(p);
}

void
cl_import2(cl_object s, cl_object p)
{
	int intern_flag;
	cl_object x;

	s = ecl_check_cl_type(@'import', s, t_symbol);
	p = si_coerce_to_package(p);
	if (p->pack.locked)
		CEpackage_error("Cannot import symbol ~S into locked package ~S.",
				"Ignore lock and proceed", p, 2, s, p);
	PACKAGE_LOCK(p);
	x = ecl_find_symbol_nolock(s->symbol.name, p, &intern_flag);
	if (intern_flag) {
		if (x != s) {
			PACKAGE_UNLOCK(p);
			CEpackage_error("Cannot import the symbol ~S "
					"from package ~A,~%"
					"because there is already a symbol with the same name~%"
					"in the package.",
					"Ignore conflict and proceed", p, 2, s, p);
		}
		if (intern_flag == INTERNAL || intern_flag == EXTERNAL)
			goto OUTPUT;
	}
	sethash(s->symbol.name, p->pack.internal, s);
	if (Null(s->symbol.hpack))
		s->symbol.hpack = p;
 OUTPUT:
	PACKAGE_UNLOCK(p);
}

void
shadowing_import(cl_object s, cl_object p)
{
	int intern_flag;
	cl_object x;

	s = ecl_check_cl_type(@'shadowing-import', s, t_symbol);
	p = si_coerce_to_package(p);
	if (p->pack.locked)
		CEpackage_error("Cannot shadowing-import symbol ~S into locked package ~S.",
				"Ignore lock and proceed", p, 2, s, p);

	PACKAGE_LOCK(p);
	x = ecl_find_symbol_nolock(s->symbol.name, p, &intern_flag);
	if (intern_flag && intern_flag != INHERITED) {
		if (x == s) {
			if (!member_eq(x, p->pack.shadowings))
				p->pack.shadowings
				= CONS(x, p->pack.shadowings);
			goto OUTPUT;
		}
		if(member_eq(x, p->pack.shadowings))
			p->pack.shadowings = ecl_remove_eq(x, p->pack.shadowings);
		if (intern_flag == INTERNAL)
			remhash(x->symbol.name, p->pack.internal);
		else
			remhash(x->symbol.name, p->pack.external);
		if (x->symbol.hpack == p)
			x->symbol.hpack = Cnil;
	}
	p->pack.shadowings = CONS(s, p->pack.shadowings);
	sethash(s->symbol.name, p->pack.internal, s);
 OUTPUT:
	PACKAGE_UNLOCK(p);
}

void
shadow(cl_object s, cl_object p)
{
	int intern_flag;
	cl_object x;

	/* Contrary to CLTL, in ANSI CL, SHADOW operates on strings. */
	s = cl_string(s);
	p = si_coerce_to_package(p);
	if (p->pack.locked)
		CEpackage_error("Cannot shadow symbol ~S in locked package ~S.",
				"Ignore lock and proceed", p, 2, s, p);
	PACKAGE_LOCK(p);
	x = ecl_find_symbol_nolock(s, p, &intern_flag);
	if (intern_flag != INTERNAL && intern_flag != EXTERNAL) {
		x = make_symbol(s);
		sethash(x->symbol.name, p->pack.internal, x);
		x->symbol.hpack = p;
	}
	p->pack.shadowings = CONS(x, p->pack.shadowings);
	PACKAGE_UNLOCK(p);
}

void
use_package(cl_object x, cl_object p)
{
	struct ecl_hashtable_entry *hash_entries;
	cl_index i, hash_length;
	int intern_flag;

	x = si_coerce_to_package(x);
	if (x == cl_core.keyword_package)
		FEpackage_error("Cannot use keyword package.", cl_core.keyword_package, 0);
	p = si_coerce_to_package(p);
	if (p->pack.locked)
		CEpackage_error("Cannot use package ~S in locked package ~S.",
				"Ignore lock and proceed",
				p, 2, x, p);
	if (p == cl_core.keyword_package)
		FEpackage_error("Cannot use in keyword package.", cl_core.keyword_package, 0);
	if (p == x)
		return;
	if (member_eq(x, p->pack.uses))
		return;

	PACKAGE_LOCK(x);
	PACKAGE_LOCK(p);
	hash_entries = x->pack.external->hash.data;
	hash_length = x->pack.external->hash.size;
	for (i = 0;  i < hash_length;  i++)
		if (hash_entries[i].key != OBJNULL) {
			cl_object here = hash_entries[i].value;
			cl_object there = ecl_find_symbol_nolock(here->symbol.name, p, &intern_flag);
			if (intern_flag && here != there
			    && ! member_eq(there, p->pack.shadowings)) {
				PACKAGE_UNLOCK(x);
				PACKAGE_UNLOCK(p);
				FEpackage_error("Cannot use ~S~%"
						"from ~S,~%"
						"because ~S and ~S will cause~%"
						"a name conflict.", p, 4, x, p, here, there);
			}
		}
	p->pack.uses = CONS(x, p->pack.uses);
	x->pack.usedby = CONS(p, x->pack.usedby);
	PACKAGE_UNLOCK(x);
	PACKAGE_UNLOCK(p);
}

void
unuse_package(cl_object x, cl_object p)
{
	x = si_coerce_to_package(x);
	p = si_coerce_to_package(p);
	if (p->pack.locked)
		CEpackage_error("Cannot unuse package ~S from locked package ~S.",
				"Ignore lock and proceed",
				p, 2, x, p);
	PACKAGE_LOCK(x);
	PACKAGE_LOCK(p);
	p->pack.uses = ecl_remove_eq(x, p->pack.uses);
	x->pack.usedby = ecl_remove_eq(p, x->pack.usedby);
	PACKAGE_UNLOCK(p);
	PACKAGE_UNLOCK(x);
}

@(defun make_package (pack_name &key nicknames (use CONS(cl_core.lisp_package, Cnil)))
@
	/* INV: make_package() performs type checking */
	@(return make_package(pack_name, nicknames, use))
@)

cl_object
si_select_package(cl_object pack_name)
{
	cl_object p = si_coerce_to_package(pack_name);
	@(return (ECL_SETQ(@'*package*', p)))
}

cl_object
cl_find_package(cl_object p)
{
	@(return ecl_find_package_nolock(p))
}

cl_object
cl_package_name(cl_object p)
{
	/* FIXME: name should be a fresh one */
	p = si_coerce_to_package(p);
	@(return p->pack.name)
}

cl_object
cl_package_nicknames(cl_object p)
{
	/* FIXME: list should be a fresh one */
	p = si_coerce_to_package(p);
	@(return p->pack.nicknames)
}

@(defun rename_package (pack new_name &o new_nicknames)
@
	/* INV: rename_package() type checks and coerces pack to package */
	@(return rename_package(pack, new_name, new_nicknames))
@)

cl_object
cl_package_use_list(cl_object p)
{
	return cl_copy_list(si_coerce_to_package(p)->pack.uses);
}

cl_object
cl_package_used_by_list(cl_object p)
{
	return cl_copy_list(si_coerce_to_package(p)->pack.usedby);
}

cl_object
cl_package_shadowing_symbols(cl_object p)
{
	return cl_copy_list(si_coerce_to_package(p)->pack.shadowings);
}

cl_object
si_package_lock(cl_object p, cl_object t)
{
	p = si_coerce_to_package(p);
	p->pack.locked = (t != Cnil);
	@(return p)
}

cl_object
cl_list_all_packages()
{
	return cl_copy_list(cl_core.packages);
}

@(defun intern (strng &optional (p current_package()) &aux sym)
	int intern_flag;
@
	sym = intern(strng, p, &intern_flag);
	if (intern_flag == INTERNAL)
		@(return sym @':internal')
	if (intern_flag == EXTERNAL)
		@(return sym @':external')
	if (intern_flag == INHERITED)
		@(return sym @':inherited')
	@(return sym Cnil)
@)

@(defun find_symbol (strng &optional (p current_package()))
	cl_object x;
	int intern_flag;
@
	x = ecl_find_symbol(strng, p, &intern_flag);
	if (intern_flag == INTERNAL)
		@(return x @':internal')
	if (intern_flag == EXTERNAL)
		@(return x @':external')
	if (intern_flag == INHERITED)
		@(return x @':inherited')
	@(return Cnil Cnil)
@)

@(defun unintern (symbl &optional (p current_package()))
@
	@(return (unintern(symbl, p) ? Ct : Cnil))
@)

@(defun export (symbols &o (pack current_package()))
	cl_object l;
@
BEGIN:
	switch (type_of(symbols)) {
	case t_symbol:
		if (Null(symbols))
			break;
		cl_export2(symbols, pack);
		break;

	case t_cons:
		pack = si_coerce_to_package(pack);
		for (l = symbols;  !endp(l);  l = CDR(l))
			cl_export2(CAR(l), pack);
		break;

	default:
		symbols = ecl_type_error(@'export',"argument",symbols,
					 cl_list(3,@'or',@'symbol',@'list'));
		goto BEGIN;
	}
	@(return Ct)
@)

@(defun unexport (symbols &o (pack current_package()))
	cl_object l;
@
BEGIN:
	switch (type_of(symbols)) {
	case t_symbol:
		if (Null(symbols))
			break;
		cl_unexport2(symbols, pack);
		break;

	case t_cons:
		pack = si_coerce_to_package(pack);
		for (l = symbols;  !endp(l);  l = CDR(l))
			cl_unexport2(CAR(l), pack);
		break;

	default:
		symbols = ecl_type_error(@'unexport',"argument",symbols,
					 cl_list(3,@'or',@'symbol',@'list'));
		goto BEGIN;
	}
	@(return Ct)
@)

@(defun import (symbols &o (pack current_package()))
	cl_object l;
@
BEGIN:
	switch (type_of(symbols)) {
	case t_symbol:
		if (Null(symbols))
			break;
		cl_import2(symbols, pack);
		break;

	case t_cons:
		pack = si_coerce_to_package(pack);
		for (l = symbols;  !endp(l);  l = CDR(l))
			cl_import2(CAR(l), pack);
		break;

	default:
		symbols = ecl_type_error(@'import',"argument",symbols,
					 cl_list(3,@'or',@'symbol',@'list'));
		goto BEGIN;
	}
	@(return Ct)
@)

@(defun shadowing_import (symbols &o (pack current_package()))
	cl_object l;
@
BEGIN:
	switch (type_of(symbols)) {
	case t_symbol:
		if (Null(symbols))
			break;
		shadowing_import(symbols, pack);
		break;

	case t_cons:
		pack = si_coerce_to_package(pack);
		for (l = symbols;  !endp(l);  l = CDR(l))
			shadowing_import(CAR(l), pack);
		break;

	default:
		symbols = ecl_type_error(@'shadowing-import',"argument",symbols,
					 cl_list(3,@'or',@'symbol',@'list'));
		goto BEGIN;
	}
	@(return Ct)
@)

@(defun shadow (symbols &o (pack current_package()))
	cl_object l;
@
BEGIN:
	switch (type_of(symbols)) {
	case t_base_string:
	case t_symbol:
	case t_character:
		/* Arguments to SHADOW may be: string designators ... */
		if (Null(symbols))
			break;
		shadow(symbols, pack);
		break;
	case t_cons:
		/* ... or lists of string designators */
		pack = si_coerce_to_package(pack);
		for (l = symbols;  !endp(l);  l = CDR(l))
			shadow(CAR(l), pack);
		break;
	default:
		symbols = ecl_type_error(@'shadow',"",symbols,
					 cl_list(3,@'or',@'symbol',@'list'));
		goto BEGIN;
	}
	@(return Ct)
@)

@(defun use_package (pack &o (pa current_package()))
	cl_object l;
@
BEGIN:
	switch (type_of(pack)) {
	case t_symbol:
		if (Null(pack))
			break;
	case t_character:
	case t_base_string:
	case t_package:
		use_package(pack, pa);
		break;

	case t_cons:
		pa = si_coerce_to_package(pa);
		for (l = pack;  !endp(l);  l = CDR(l))
			use_package(CAR(l), pa);
		break;

	default:
		assert_type_package(pack);
		goto BEGIN;
	}
	@(return Ct)
@)

@(defun unuse_package (pack &o (pa current_package()))
	cl_object l;
@
BEGIN:
	switch (type_of(pack)) {
	case t_symbol:
		if (Null(pack))
			break;
	case t_character:
	case t_base_string:
	case t_package:
		unuse_package(pack, pa);
		break;

	case t_cons:
		pa = si_coerce_to_package(pa);
		for (l = pack;  !endp(l);  l = CDR(l))
			unuse_package(CAR(l), pa);
		break;

	default:
		assert_type_package(pack);
		goto BEGIN;
	}
	@(return Ct)
@)

cl_object
si_package_hash_tables(cl_object p)
{
	cl_object he, hi, u;
	assert_type_package(p);
	PACKAGE_LOCK(p);
	he = si_copy_hash_table(p->pack.external);
	hi = si_copy_hash_table(p->pack.internal);
	u = cl_copy_list(p->pack.uses);
	PACKAGE_UNLOCK(p);
	@(return he hi u)
}
