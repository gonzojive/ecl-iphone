/* -*- mode: c; c-basic-offset: 8 -*- */
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
#include <ecl/ecl-inl.h>

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
	loop_for_on_unsafe(l) {
		if (ecl_string_eq(x, ECL_CONS_CAR(l)))
			return TRUE;
	} end_loop_for_on;
	return FALSE;
}

#if defined(__cplusplus) || (defined(__GNUC__) && !defined(__STRICT_ANSI__))
#define INLINE inline
#else
#define INLINE
#endif

static INLINE void
symbol_remove_package(cl_object s, cl_object p)
{
	if (Null(s))
		s = Cnil_symbol;
	if (s->symbol.hpack == p)
		s->symbol.hpack = Cnil;
}

static INLINE void
symbol_add_package(cl_object s, cl_object p)
{
	if (Null(s))
		s = Cnil_symbol;
	if (s->symbol.hpack == Cnil)
		s->symbol.hpack = p;
}

/*
	ecl_make_package(n, ns, ul) makes a package with name n,
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
	h->hash.rehash_size = ecl_make_singlefloat(1.5f);
	h->hash.threshold = ecl_make_singlefloat(0.75f);
	h->hash.factor = 0.7;
	h->hash.entries = 0;
	h->hash.data = NULL; /* for GC sake */
	h->hash.data = (struct ecl_hashtable_entry *)cl_alloc(hsize * sizeof(struct ecl_hashtable_entry));
	return cl_clrhash(h);
}

cl_object
ecl_make_package(cl_object name, cl_object nicknames, cl_object use_list)
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
		cl_object l = cl_core.packages_to_be_created;
		cl_object tail = l;
		while (CONSP(l)) {
			cl_object pair = ECL_CONS_CAR(l);
			cl_object other_name = ECL_CONS_CAR(pair);
			if (ecl_equal(other_name, name) ||
			    funcall(5, @'member', other_name, nicknames,
				    @':test', @'string=') != Cnil)
			{
				x = ECL_CONS_CDR(pair);
				pair = ECL_CONS_CDR(l);
				if (l == tail) {
					cl_core.packages_to_be_created = pair;
				} else {
					ECL_RPLACD(tail, pair);
				}
				goto INTERN;
			}
			tail = l;
			l = ECL_CONS_CDR(l);
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
	x->pack.name = name;
	x->pack.nicknames = Cnil;
	x->pack.shadowings = Cnil;
	x->pack.uses = Cnil;
	x->pack.usedby = Cnil;
	x->pack.locked = FALSE;
	loop_for_in(nicknames) {
		cl_object nick = cl_string(ECL_CONS_CAR(nicknames));
		if ((other = ecl_find_package_nolock(nick)) != Cnil) {
			name = nick;
			goto ERROR;
		}
		x->pack.nicknames = CONS(nick, x->pack.nicknames);
	} end_loop_for_in;
	loop_for_in(use_list) {
		y = si_coerce_to_package(ECL_CONS_CAR(use_list));
		x->pack.uses = CONS(y, x->pack.uses);
		y->pack.usedby = CONS(x, y->pack.usedby);
	} end_loop_for_in;

	/* 3) Finally, add it to the list of packages */
	cl_core.packages = CONS(x, cl_core.packages);
	PACKAGE_OP_UNLOCK();
	return(x);
}

cl_object
ecl_rename_package(cl_object x, cl_object name, cl_object nicknames)
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
		PACKAGE_OP_UNLOCK();
		FEpackage_error("A package with name ~S already exists.", x,
				1, name);
	}
	x->pack.name = name;
	x->pack.nicknames = Cnil;
	while (!Null(nicknames)) {
		cl_object nick;
		if (!CONSP(nicknames)) {
			PACKAGE_OP_UNLOCK();
			FEtype_error_list(nicknames);
		}
		nick = ECL_CONS_CAR(nicknames);
		y = ecl_find_package_nolock(nick);
		if (ecl_find_package_nolock(nick) != x)
			x->pack.nicknames = CONS(cl_string(nick), x->pack.nicknames);
		nicknames = ECL_CONS_CDR(nicknames);
	}
	PACKAGE_OP_UNLOCK();
	return x;
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
	l = cl_core.packages;
	loop_for_on_unsafe(l) {
		p = ECL_CONS_CAR(l);
		if (ecl_string_eq(name, p->pack.name))
			return p;
		if (member_string_eq(name, p->pack.nicknames))
			return p;
	} end_loop_for_on;
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
ecl_current_package(void)
{
	cl_object x;

	x = ecl_symbol_value(@'*package*');
	if (type_of(x) != t_package) {
		ECL_SETQ(@'*package*', cl_core.user_package);
		FEerror("The value of *PACKAGE*, ~S, was not a package",
			1, x);
	}
	return(x);
}

/*
	Ecl_Intern(st, p) interns string st in package p.
*/
cl_object
_ecl_intern(const char *s, cl_object p)
{
	int intern_flag;
	cl_object str = make_constant_base_string(s);
	return ecl_intern(str, p, &intern_flag);
}

cl_object
ecl_intern(cl_object name, cl_object p, int *intern_flag)
{
	cl_object s, ul;

	name = ecl_check_type_string(@'intern', name);
#ifdef ECL_UNICODE
	if (ecl_fits_in_base_string(name)) {
		name = si_copy_to_simple_base_string(name);
	}
#endif
	p = si_coerce_to_package(p);
 TRY_AGAIN_LABEL:
	PACKAGE_LOCK(p);
	s = ecl_gethash_safe(name, p->pack.external, OBJNULL);
	if (s != OBJNULL) {
		*intern_flag = EXTERNAL;
		goto OUTPUT;
	}
	/* Keyword package has no intern section nor can it be used */
	if (p == cl_core.keyword_package) goto INTERN;
	s = ecl_gethash_safe(name, p->pack.internal, OBJNULL);
	if (s != OBJNULL) {
		*intern_flag = INTERNAL;
		goto OUTPUT;
	}
	ul = p->pack.uses;
	loop_for_on_unsafe(ul) {
		s = ecl_gethash_safe(name, ECL_CONS_CAR(ul)->pack.external, OBJNULL);
		if (s != OBJNULL) {
			*intern_flag = INHERITED;
			goto OUTPUT;
		}
	} end_loop_for_on;
 INTERN:
	if (p->pack.locked) {
		PACKAGE_UNLOCK(p);
		CEpackage_error("Cannot intern symbol ~S in locked package ~S.",
				"Ignore lock and proceed", p, 2, name, p);
		goto TRY_AGAIN_LABEL;
	}
	s = cl_make_symbol(name);
	s->symbol.hpack = p;
	*intern_flag = 0;
	if (p == cl_core.keyword_package) {
		ecl_symbol_type_set(s, ecl_symbol_type(s) | stp_constant);
		ECL_SET(s, s);
		ecl_sethash(name, p->pack.external, s);
	} else {
		ecl_sethash(name, p->pack.internal, s);
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

	name = ecl_check_type_string(@'find-symbol', name);
#ifdef ECL_UNICODE
	if (ecl_fits_in_base_string(name)) {
		name = si_copy_to_simple_base_string(name);
	}
#endif
	s = ecl_gethash_safe(name, p->pack.external, OBJNULL);
	if (s != OBJNULL) {
		*intern_flag = EXTERNAL;
		goto OUTPUT;
	}
	if (p == cl_core.keyword_package)
		goto NOTHING;
	s = ecl_gethash_safe(name, p->pack.internal, OBJNULL);
	if (s != OBJNULL) {
		*intern_flag = INTERNAL;
		goto OUTPUT;
	}
	ul = p->pack.uses;
	loop_for_on_unsafe(ul) {
		s = ecl_gethash_safe(name, ECL_CONS_CAR(ul)->pack.external, OBJNULL);
		if (s != OBJNULL) {
			*intern_flag = INHERITED;
			goto OUTPUT;
		}
	} end_loop_for_on;
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
ecl_unintern(cl_object s, cl_object p)
{
	cl_object x, y, l, hash;
	bool output = FALSE;
	cl_object name = ecl_symbol_name(s);

	p = si_coerce_to_package(p);

 TRY_AGAIN_LABEL:
	PACKAGE_LOCK(p);
	hash = p->pack.internal;
	x = ecl_gethash_safe(name, hash, OBJNULL);
	if (x == s)
		goto UNINTERN;
	hash = p->pack.external;
	x = ecl_gethash_safe(name, hash, OBJNULL);
	if (x != s)
		goto OUTPUT;
 UNINTERN:
	if (p->pack.locked) {
		PACKAGE_UNLOCK(p);
		CEpackage_error("Cannot unintern symbol ~S from locked package ~S.",
				"Ignore lock and proceed", p, 2, s, p);
		goto TRY_AGAIN_LABEL;
	}
	if (!ecl_member_eq(s, p->pack.shadowings))
		goto NOT_SHADOW;
	x = OBJNULL;
	l = p->pack.uses;
	loop_for_on_unsafe(l) {
		y = ecl_gethash_safe(name, ECL_CONS_CAR(l)->pack.external, OBJNULL);
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
	} end_loop_for_on;
	p->pack.shadowings = ecl_remove_eq(s, p->pack.shadowings);
 NOT_SHADOW:
	ecl_remhash(name, hash);
	symbol_remove_package(s, p);
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
	cl_object name = ecl_symbol_name(s);
	p = si_coerce_to_package(p);
	if (p->pack.locked)
		CEpackage_error("Cannot export symbol ~S from locked package ~S.",
				"Ignore lock and proceed", p, 2, s, p);
	PACKAGE_LOCK(p);
	x = ecl_find_symbol_nolock(name, p, &intern_flag);
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
	l = p->pack.usedby;
	loop_for_on_unsafe(l) {
		x = ecl_find_symbol_nolock(name, ECL_CONS_CAR(l), &intern_flag);
		if (intern_flag && s != x &&
		    !ecl_member_eq(x, CAR(l)->pack.shadowings)) {
			PACKAGE_UNLOCK(p);
			FEpackage_error("Cannot export the symbol ~S~%"
					"from ~S,~%"
					"because it will cause a name conflict~%"
					"in ~S.", p, 3, s, p, CAR(l));
		}
	} end_loop_for_on;
	if (hash != OBJNULL)
		ecl_remhash(name, hash);
	ecl_sethash(name, p->pack.external, s);
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
	list = p->pack.uses;
	loop_for_on_unsafe(list) {
		ecl_unuse_package(ECL_CONS_CAR(list), p);
	} end_loop_for_on;
	list = p->pack.usedby;
	loop_for_on_unsafe(list) {
		ecl_unuse_package(p, ECL_CONS_CAR(list));
	} end_loop_for_on;
	PACKAGE_LOCK(p);
	for (hash = p->pack.internal, i = 0; i < hash->hash.size; i++)
		if (hash->hash.data[i].key != OBJNULL) {
			cl_object s = hash->hash.data[i].value;
			symbol_remove_package(s, p);
		}
	cl_clrhash(p->pack.internal);
	for (hash = p->pack.external, i = 0; i < hash->hash.size; i++)
		if (hash->hash.data[i].key != OBJNULL) {
			cl_object s = hash->hash.data[i].value;
			symbol_remove_package(s, p);
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
	cl_object name = ecl_symbol_name(s);
	p = si_coerce_to_package(p);
	if (p == cl_core.keyword_package)
		FEpackage_error("Cannot unexport a symbol from the keyword package.",
				cl_core.keyword_package, 0);
	if (p->pack.locked)
		CEpackage_error("Cannot unexport symbol ~S from locked package ~S.",
				"Ignore lock and proceed", p, 2, s, p);
	PACKAGE_LOCK(p);
	x = ecl_find_symbol_nolock(name, p, &intern_flag);
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
		ecl_remhash(name, p->pack.external);
		ecl_sethash(name, p->pack.internal, s);
	}
	PACKAGE_UNLOCK(p);
}

void
cl_import2(cl_object s, cl_object p)
{
	int intern_flag;
	cl_object x;
	cl_object name = ecl_symbol_name(s);
	p = si_coerce_to_package(p);
	if (p->pack.locked)
		CEpackage_error("Cannot import symbol ~S into locked package ~S.",
				"Ignore lock and proceed", p, 2, s, p);
	PACKAGE_LOCK(p);
	x = ecl_find_symbol_nolock(name, p, &intern_flag);
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
	ecl_sethash(name, p->pack.internal, s);
	symbol_add_package(s, p);
 OUTPUT:
	PACKAGE_UNLOCK(p);
}

void
ecl_shadowing_import(cl_object s, cl_object p)
{
	int intern_flag;
	cl_object x;
	cl_object name = ecl_symbol_name(s);
	p = si_coerce_to_package(p);
	if (p->pack.locked)
		CEpackage_error("Cannot shadowing-import symbol ~S into locked package ~S.",
				"Ignore lock and proceed", p, 2, s, p);

	PACKAGE_LOCK(p);
	x = ecl_find_symbol_nolock(name, p, &intern_flag);
	if (intern_flag && intern_flag != INHERITED) {
		if (x == s) {
			if (!ecl_member_eq(x, p->pack.shadowings))
				p->pack.shadowings
				= CONS(x, p->pack.shadowings);
			goto OUTPUT;
		}
		if(ecl_member_eq(x, p->pack.shadowings))
			p->pack.shadowings = ecl_remove_eq(x, p->pack.shadowings);
		if (intern_flag == INTERNAL)
			ecl_remhash(name, p->pack.internal);
		else
			ecl_remhash(name, p->pack.external);
		symbol_remove_package(x, p);
	}
	p->pack.shadowings = CONS(s, p->pack.shadowings);
	ecl_sethash(name, p->pack.internal, s);
 OUTPUT:
	PACKAGE_UNLOCK(p);
}

void
ecl_shadow(cl_object s, cl_object p)
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
		x = cl_make_symbol(s);
		ecl_sethash(s, p->pack.internal, x);
		x->symbol.hpack = p;
	}
	p->pack.shadowings = CONS(x, p->pack.shadowings);
	PACKAGE_UNLOCK(p);
}

void
ecl_use_package(cl_object x, cl_object p)
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
	if (ecl_member_eq(x, p->pack.uses))
		return;

	PACKAGE_LOCK(x);
	PACKAGE_LOCK(p);
	hash_entries = x->pack.external->hash.data;
	hash_length = x->pack.external->hash.size;
	for (i = 0;  i < hash_length;  i++)
		if (hash_entries[i].key != OBJNULL) {
			cl_object here = hash_entries[i].value;
			cl_object name = ecl_symbol_name(here);
			cl_object there = ecl_find_symbol_nolock(name, p, &intern_flag);
			if (intern_flag && here != there
			    && ! ecl_member_eq(there, p->pack.shadowings)) {
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
ecl_unuse_package(cl_object x, cl_object p)
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
	/* INV: ecl_make_package() performs type checking */
	@(return ecl_make_package(pack_name, nicknames, use))
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
	/* INV: ecl_rename_package() type checks and coerces pack to package */
	@(return ecl_rename_package(pack, new_name, new_nicknames))
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

@(defun intern (strng &optional (p ecl_current_package()) &aux sym)
	int intern_flag;
@
	sym = ecl_intern(strng, p, &intern_flag);
	if (intern_flag == INTERNAL)
		@(return sym @':internal')
	if (intern_flag == EXTERNAL)
		@(return sym @':external')
	if (intern_flag == INHERITED)
		@(return sym @':inherited')
	@(return sym Cnil)
@)

@(defun find_symbol (strng &optional (p ecl_current_package()))
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

@(defun unintern (symbl &optional (p ecl_current_package()))
@
	@(return (ecl_unintern(symbl, p) ? Ct : Cnil))
@)

@(defun export (symbols &o (pack ecl_current_package()))
@
BEGIN:
	switch (type_of(symbols)) {
	case t_symbol:
		cl_export2(symbols, pack);
		break;

	case t_list:
		pack = si_coerce_to_package(pack);
		loop_for_in(symbols) {
			cl_export2(ECL_CONS_CAR(symbols), pack);
		} end_loop_for_in;
		break;

	default:
		symbols = ecl_type_error(@'export',"argument",symbols,
					 cl_list(3,@'or',@'symbol',@'list'));
		goto BEGIN;
	}
	@(return Ct)
@)

@(defun unexport (symbols &o (pack ecl_current_package()))
@
BEGIN:
	switch (type_of(symbols)) {
	case t_symbol:
		cl_unexport2(symbols, pack);
		break;

	case t_list:
		pack = si_coerce_to_package(pack);
		loop_for_in(symbols) {
			cl_unexport2(ECL_CONS_CAR(symbols), pack);
		} end_loop_for_in;
		break;

	default:
		symbols = ecl_type_error(@'unexport',"argument",symbols,
					 cl_list(3,@'or',@'symbol',@'list'));
		goto BEGIN;
	}
	@(return Ct)
@)

@(defun import (symbols &o (pack ecl_current_package()))
@
BEGIN:
	switch (type_of(symbols)) {
	case t_symbol:
		cl_import2(symbols, pack);
		break;

	case t_list:
		pack = si_coerce_to_package(pack);
		loop_for_in(symbols) {
			cl_import2(ECL_CONS_CAR(symbols), pack);
		} end_loop_for_in;
		break;

	default:
		symbols = ecl_type_error(@'import',"argument",symbols,
					 cl_list(3,@'or',@'symbol',@'list'));
		goto BEGIN;
	}
	@(return Ct)
@)

@(defun shadowing_import (symbols &o (pack ecl_current_package()))
@
BEGIN:
	switch (type_of(symbols)) {
	case t_symbol:
		ecl_shadowing_import(symbols, pack);
		break;

	case t_list:
		pack = si_coerce_to_package(pack);
		loop_for_in(symbols) {
			ecl_shadowing_import(ECL_CONS_CAR(symbols), pack);
		} end_loop_for_in;
		break;

	default:
		symbols = ecl_type_error(@'shadowing-import',"argument",symbols,
					 cl_list(3,@'or',@'symbol',@'list'));
		goto BEGIN;
	}
	@(return Ct)
@)

@(defun shadow (symbols &o (pack ecl_current_package()))
@
BEGIN:
	switch (type_of(symbols)) {
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string:
	case t_symbol:
	case t_character:
		/* Arguments to SHADOW may be: string designators ... */
		ecl_shadow(symbols, pack);
		break;
	case t_list:
		/* ... or lists of string designators */
		pack = si_coerce_to_package(pack);
		loop_for_in(symbols) {
			ecl_shadow(ECL_CONS_CAR(symbols), pack);
		} end_loop_for_in;
		break;
	default:
		symbols = ecl_type_error(@'shadow',"",symbols,
					 cl_list(3,@'or',@'symbol',@'list'));
		goto BEGIN;
	}
	@(return Ct)
@)

@(defun use_package (pack &o (pa ecl_current_package()))
@
BEGIN:
	switch (type_of(pack)) {
	case t_symbol:
	case t_character:
	case t_base_string:
	case t_package:
		ecl_use_package(pack, pa);
		break;

	case t_list:
		pa = si_coerce_to_package(pa);
		loop_for_in(pack) {
			ecl_use_package(ECL_CONS_CAR(pack), pa);
		} end_loop_for_in;
		break;

	default:
		assert_type_package(pack);
		goto BEGIN;
	}
	@(return Ct)
@)

@(defun unuse_package (pack &o (pa ecl_current_package()))
@
BEGIN:
	switch (type_of(pack)) {
	case t_symbol:
	case t_character:
	case t_base_string:
	case t_package:
		ecl_unuse_package(pack, pa);
		break;

	case t_list:
		pa = si_coerce_to_package(pa);
		loop_for_in(pack) {
			ecl_unuse_package(ECL_CONS_CAR(pack), pa);
		} end_loop_for_in;
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
