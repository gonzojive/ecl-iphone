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


#include "ecl.h"

/******************************* EXPORTS ******************************/

cl_object lisp_package;
cl_object user_package;
cl_object keyword_package;
cl_object system_package;
#ifdef CLOS
cl_object clos_package;
#endif
#ifdef TK
cl_object tk_package;
#endif

/******************************* ------- ******************************/

#define	INTERNAL	1
#define	EXTERNAL	2
#define	INHERITED	3

static cl_object package_list = Cnil;
static cl_object uninterned_list = Cnil;

static void no_package(cl_object n) __attribute__((noreturn));
static void package_already(cl_object n) __attribute__((noreturn));

static void
no_package(cl_object n)
{
	FEerror("There is no package with the name ~A.", 1, n);
}

static void
package_already(cl_object n)
{
	FEerror("A package with the name ~A already exists.", 1, n);
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
	cl_index hsize = 128, i;

	h = cl_alloc_object(t_hashtable);
	h->hash.test = htt_pack;
	h->hash.size = hsize;
	h->hash.rehash_size = make_shortfloat(1.5);
	h->hash.threshold = make_shortfloat(0.7);
	h->hash.entries = 0;
	h->hash.data = NULL; /* for GC sake */
	h->hash.data = (struct hashtable_entry *)cl_alloc(hsize * sizeof(struct hashtable_entry));
	cl_clear_hash_table(h);
	return h;
}

cl_object
make_package(cl_object name, cl_object nicknames, cl_object use_list)
{
	cl_object x, y;
	cl_index i;

	name = cl_string(name);
	assert_type_proper_list(nicknames);
	assert_type_proper_list(use_list);

	if (find_package(name) != Cnil)
		package_already(name);
	x = cl_alloc_object(t_package);
	x->pack.name = name;
	x->pack.nicknames = Cnil;
	x->pack.shadowings = Cnil;
	x->pack.uses = Cnil;
	x->pack.usedby = Cnil;
	x->pack.locked = FALSE;
	for (;  !endp(nicknames);  nicknames = CDR(nicknames)) {
		cl_object nick = cl_string(CAR(nicknames));
		if (find_package(nick) != Cnil)
			package_already(nick);
		x->pack.nicknames = CONS(nick, x->pack.nicknames);
	}
	for (;  !endp(use_list);  use_list = CDR(use_list)) {
		if (type_of(CAR(use_list)) == t_package)
			y = CAR(use_list);
		else {
			y = find_package(CAR(use_list));
			if (Null(y))
				no_package(CAR(use_list));
		}
		x->pack.uses = CONS(y, x->pack.uses);
		y->pack.usedby = CONS(x, y->pack.usedby);
	}
	x->pack.internal = make_package_hashtable();
	x->pack.external = make_package_hashtable();
	package_list = CONS(x, package_list);
	return(x);
}

cl_object
rename_package(cl_object x, cl_object name, cl_object nicknames)
{
	cl_object y;

	/*
	   If we are trying to rename the package with either its name
	   or a nickname, then we are really trying to redefine the
	   package.  Therefore, do not signal the error.

	   Marco Antoniotti 19951028
	 */
	x = si_coerce_to_package(x);
	name = cl_string(name);
	y = find_package(name);
	if ((y != Cnil) && (y != x))
		package_already(name);

	x->pack.name = name;
	x->pack.nicknames = Cnil;
	assert_type_proper_list(nicknames);
	for (;  !endp(nicknames);  nicknames = CDR(nicknames)) {
		cl_object nick = CAR(nicknames);
		y = find_package(nick);
		if (x == y)
			continue;
		if (y != Cnil)
			package_already(nick);
		x->pack.nicknames = CONS(cl_string(nick), x->pack.nicknames);
	}
	return(x);
}

/*
	Find_package(n) seaches for a package with name n, where n is
	a valid string designator, or simply outputs n if it is a
	package.
*/
cl_object
find_package(cl_object name)
{
	cl_object l, p;

	if (type_of(name) == t_package)
		return name;
	name = cl_string(name);
	/* INV: package_list is a proper list */
	for (l = package_list; CONSP(l); l = CDR(l)) {
		p = CAR(l);
		if (string_eq(name, p->pack.name))
			return p;
		if (member_string_eq(name, p->pack.nicknames))
			return p;
	}
	return Cnil;
}

cl_object
si_coerce_to_package(cl_object p)
{
	cl_object pp = find_package(p);
	if (!Null(pp))
		@(return pp);
	FEwrong_type_argument(@'package', p);
}

cl_object
current_package(void)
{
	cl_object x;

	x = symbol_value(@'*package*');
	if (type_of(x) != t_package) {
		SYM_VAL(@'*package*') = user_package;
		FEerror("The value of *PACKAGE*, ~S, was not a package.",
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
	cl_object str = make_constant_string(s);
	return intern(str, p, &intern_flag);
}

cl_object
intern(cl_object name, cl_object p, int *intern_flag)
{
	cl_object s, ul;

	assert_type_string(name);
	p = si_coerce_to_package(p);
	s = gethash_safe(name, p->pack.external, OBJNULL);
	if (s != OBJNULL) {
		*intern_flag = EXTERNAL;
		return s;
	}
	/* Keyword package has no intern section nor can it be used */
	if (p == keyword_package) goto INTERN;
	s = gethash_safe(name, p->pack.internal, OBJNULL);
	if (s != OBJNULL) {
		*intern_flag = INTERNAL;
		return s;
	}
	for (ul=p->pack.uses; CONSP(ul); ul = CDR(ul)) {
		s = gethash_safe(name, CAR(ul)->pack.external, OBJNULL);
		if (s != OBJNULL) {
			*intern_flag = INHERITED;
			return s;
		}
	}
 INTERN:
	s = make_symbol(name);
	s->symbol.hpack = p;
	*intern_flag = 0;
	if (p == keyword_package) {
		s->symbol.stype = stp_constant;
		SYM_VAL(s) = s;
		sethash(name, p->pack.external, s);
	} else {
		sethash(name, p->pack.internal, s);
	}
	return s;
}

/*
	Find_symbol(st, len, p) searches for string st of length len in package p.
*/
cl_object
find_symbol(cl_object name, cl_object p, int *intern_flag)
{
	cl_object s, ul;
	
	name = cl_string(name);
	p = si_coerce_to_package(p);
	s = gethash_safe(name, p->pack.external, OBJNULL);
	if (s != OBJNULL) {
		*intern_flag = EXTERNAL;
		return s;
	}
	if (p == keyword_package) goto RETURN;
	s = gethash_safe(name, p->pack.internal, OBJNULL);
	if (s != OBJNULL) {
		*intern_flag = INTERNAL;
		return s;
	}
	for (ul=p->pack.uses; CONSP(ul); ul = CDR(ul)) {
		s = gethash_safe(name, CAR(ul)->pack.external, OBJNULL);
		if (s != OBJNULL) {
			*intern_flag = INHERITED;
			return s;
		}
	}
RETURN:
	*intern_flag = 0;
	return(Cnil);
}

static void
delete_eq(cl_object x, cl_object *lp)
{
	for (;  CONSP(*lp);  lp = &CDR((*lp)))
		if (CAR((*lp)) == x) {
			*lp = CDR((*lp));
			return;
		}
}

bool
unintern(cl_object s, cl_object p)
{
	cl_object x, y, l, hash;

	assert_type_symbol(s);
	p = si_coerce_to_package(p);
	hash = p->pack.internal;
	x = gethash_safe(s->symbol.name, hash, OBJNULL);
	if (x == s) {
		if (member_eq(s, p->pack.shadowings))
			goto L;
		goto UNINTERN;
	}
	hash = p->pack.external;
	x = gethash_safe(s->symbol.name, hash, OBJNULL);
	if (x == s) {
		if (member_eq(s, p->pack.shadowings))
			goto L;
		goto UNINTERN;
	}
	return(FALSE);

L:
	x = OBJNULL;
	for (l = p->pack.uses; CONSP(l); l = CDR(l)) {
		y = gethash_safe(s->symbol.name, CAR(l)->pack.external, OBJNULL);
		if (y != OBJNULL) {
			if (x == OBJNULL)
				x = y;
			else if (x != y)
FEerror("Cannot unintern the shadowing symbol ~S~%\
from ~S,~%\
because ~S and ~S will cause~%\
a name conflict.", 4, s, p, x, y);
		}
	}
	delete_eq(s, &p->pack.shadowings);

UNINTERN:
	remhash(s->symbol.name, hash);
	if (s->symbol.hpack == p)
		s->symbol.hpack = Cnil;
	if (s->symbol.stype != stp_ordinary)
		uninterned_list = CONS(s, uninterned_list);
	return(TRUE);
}

void
cl_export2(cl_object s, cl_object p)
{
	cl_object x, l, hash = OBJNULL;
	int intern_flag;
BEGIN:
	assert_type_symbol(s);
	p = si_coerce_to_package(p);
	x = find_symbol(s, p, &intern_flag);
	if (!intern_flag)
		FEerror("The symbol ~S is not accessible from ~S.", 2,
			s, p);
	if (x != s) {
		cl_import2(s, p); /*  signals an error  */
		goto BEGIN;
	}
	if (intern_flag == EXTERNAL)
		return;
	if (intern_flag == INTERNAL)
		hash = p->pack.internal;
	for (l = p->pack.usedby; CONSP(l); l = CDR(l)) {
		x = find_symbol(s, CAR(l), &intern_flag);
		if (intern_flag && s != x &&
		    !member_eq(x, CAR(l)->pack.shadowings))
FEerror("Cannot export the symbol ~S~%\
from ~S,~%\
because it will cause a name conflict~%\
in ~S.", 3, s, p, CAR(l));
	}
	if (hash != OBJNULL)
		remhash(s->symbol.name, hash);
	sethash(s->symbol.name, p->pack.external, s);
}

void
delete_package(cl_object p)
{
	cl_object hash, list;
	cl_index i;

	p = si_coerce_to_package(p);
	if (p == lisp_package || p == keyword_package)
		FEerror("Cannot remove package ~S", 1, p->pack.name);
	for (list = p->pack.uses; !endp(list); list = CDR(list))
		unuse_package(CAR(list), p);
	for (list = p->pack.usedby; !endp(list); list = CDR(list))
		unuse_package(p, CAR(list));
	for (hash = p->pack.internal, i = 0; i < hash->hash.size; i++)
		if (hash->hash.data[i].key != OBJNULL)
			unintern(hash->hash.data[i].value, p);
	for (hash = p->pack.external, i = 0; i < hash->hash.size; i++)
		if (hash->hash.data[i].key != OBJNULL)
			unintern(hash->hash.data[i].value, p);
	delete_eq(p, &package_list);
	p->pack.shadowings = Cnil;
	p->pack.internal = OBJNULL;
	p->pack.external = OBJNULL;
}

void
cl_unexport2(cl_object s, cl_object p)
{
	int intern_flag;
	cl_object x;

	if (p == keyword_package)
		FEerror("Cannot unexport a symbol from the keyword.", 0);
	assert_type_symbol(s);
	p = si_coerce_to_package(p);
	x = find_symbol(s, p, &intern_flag);
	if (intern_flag != EXTERNAL || x != s)
		/* According to ANSI & Cltl, internal symbols are
		   ignored in unexport */
		return;
	remhash(s->symbol.name, p->pack.external);
	sethash(s->symbol.name, p->pack.internal, s);
}

void
cl_import2(cl_object s, cl_object p)
{
	int intern_flag;
	cl_object x;

	assert_type_symbol(s);
	p = si_coerce_to_package(p);
	x = find_symbol(s, p, &intern_flag);
	if (intern_flag) {
		if (x != s)
			FEerror("Cannot import the symbol ~S~%\
from ~S,~%\
because there is already a symbol with the same name~%\
in the package.", 2, s, p);
		if (intern_flag == INTERNAL || intern_flag == EXTERNAL)
			return;
	}
	sethash(s->symbol.name, p->pack.internal, s);
	if (Null(s->symbol.hpack))
		s->symbol.hpack = p;
}

void
shadowing_import(cl_object s, cl_object p)
{
	int intern_flag;
	cl_object x;

	assert_type_symbol(s);
	p = si_coerce_to_package(p);
	x = find_symbol(s, p, &intern_flag);
	if (intern_flag && intern_flag != INHERITED) {
		if (x == s) {
			if (!member_eq(x, p->pack.shadowings))
				p->pack.shadowings
				= CONS(x, p->pack.shadowings);
			return;
		}
		if(member_eq(x, p->pack.shadowings))
			delete_eq(x, &p->pack.shadowings);
		if (intern_flag == INTERNAL)
			remhash(x->symbol.name, p->pack.internal);
		else
			remhash(x->symbol.name, p->pack.external);
		if (x->symbol.hpack == p)
			x->symbol.hpack = Cnil;
		if (x->symbol.stype != stp_ordinary)
			uninterned_list = CONS(x, uninterned_list);
	}
	p->pack.shadowings = CONS(s, p->pack.shadowings);
	sethash(s->symbol.name, p->pack.internal, s);
}

void
shadow(cl_object s, cl_object p)
{
	int intern_flag;
	cl_object x;

	assert_type_symbol(s);
	p = si_coerce_to_package(p);
	x = find_symbol(s, p, &intern_flag);
	if (intern_flag != INTERNAL && intern_flag != EXTERNAL) {
		x = make_symbol(s);
		sethash(x->symbol.name, p->pack.internal, x);
		x->symbol.hpack = p;
	}
	p->pack.shadowings = CONS(x, p->pack.shadowings);
}

void
use_package(cl_object x, cl_object p)
{
	struct hashtable_entry *hash_entries;
	cl_index i, hash_length;
	int intern_flag;

	x = si_coerce_to_package(x);
	if (x == keyword_package)
		FEerror("Cannot use keyword package.", 0);
	p = si_coerce_to_package(p);
	if (p == keyword_package)
		FEerror("Cannot use in keyword package.", 0);
	if (p == x)
		return;
	if (member_eq(x, p->pack.uses))
		return;
	hash_entries = x->pack.external->hash.data;
	hash_length = x->pack.external->hash.size;
	for (i = 0;  i < hash_length;  i++)
		if (hash_entries[i].key != OBJNULL) {
			cl_object here = hash_entries[i].value;
			cl_object there = find_symbol(here, p, &intern_flag);
			if (intern_flag && here != there
			    && ! member_eq(there, p->pack.shadowings))
FEerror("Cannot use ~S~%\
from ~S,~%\
because ~S and ~S will cause~%\
a name conflict.", 4, x, p, here, there);
		}
	p->pack.uses = CONS(x, p->pack.uses);
	x->pack.usedby = CONS(p, x->pack.usedby);
}

void
unuse_package(cl_object x, cl_object p)
{
	x = si_coerce_to_package(x);
	p = si_coerce_to_package(p);
	delete_eq(x, &p->pack.uses);
	delete_eq(p, &x->pack.usedby);
}

@(defun make_package (pack_name &key nicknames (use CONS(lisp_package, Cnil)))
@
	/* INV: make_package() performs type checking */
	@(return make_package(pack_name, nicknames, use))
@)

cl_object
si_select_package(cl_object pack_name)
{
	cl_object p;

	/* INV: find_package()/in_package() perform type checks */
	p = find_package(pack_name);
	if (Null(p))
		FEerror("Package ~s not found", 1, pack_name);
	@(return (SYM_VAL(@'*package*') = p))
}

cl_object
cl_find_package(cl_object p)
{
	@(return find_package(p))
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
	/* FIXME: list should be a fresh one */
	p = si_coerce_to_package(p);
	@(return p->pack.uses)
}

cl_object
cl_package_used_by_list(cl_object p)
{
	/* FIXME: list should be a fresh one */
	p = si_coerce_to_package(p);
	@(return p->pack.usedby)
}

cl_object
cl_package_shadowing_symbols(cl_object p)
{
	/* FIXME: list should be a fresh one */
	p = si_coerce_to_package(p);
	@(return p->pack.shadowings)
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
	return cl_copy_list(package_list);
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
	x = find_symbol(strng, p, &intern_flag);
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
		assert_type_symbol(symbols);
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
		assert_type_symbol(symbols);
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
		assert_type_symbol(symbols);
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
		assert_type_symbol(symbols);
		goto BEGIN;
	}
	@(return Ct)
@)

@(defun shadow (symbols &o (pack current_package()))
	cl_object l;
@
BEGIN:
	switch (type_of(symbols)) {
	case t_symbol:
		if (Null(symbols))
			break;
		shadow(symbols, pack);
		break;

	case t_cons:
		pack = si_coerce_to_package(pack);
		for (l = symbols;  !endp(l);  l = CDR(l))
			shadow(CAR(l), pack);
		break;

	default:
		assert_type_symbol(symbols);
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
	case t_string:
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

	case t_string:
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
si_package_internal(cl_object p, cl_object index)
{
	cl_fixnum j;
	cl_object hash;

	p = si_coerce_to_package(p);
	hash = p->pack.internal;
	if (!FIXNUMP(index) || (j = fix(index)) < 0 || j >= hash->hash.size)
		FEerror("~S is an illegal index to a package hashtable.",
			1, index);
	@(return ((hash->hash.data[j].key != OBJNULL)?
		   hash->hash.data[j].value : MAKE_FIXNUM(1)))
}

cl_object
si_package_external(cl_object p, cl_object index)
{
	cl_fixnum j;
	cl_object hash;

	p = si_coerce_to_package(p);
	hash = p->pack.external;
	if (!FIXNUMP(index) || (j = fix(index)) < 0 || j >= hash->hash.size)
		FEerror("~S is an illegal index to a package hashtable.",
			1, index);
	@(return ((hash->hash.data[j].key != OBJNULL)?
		  hash->hash.data[j].value : MAKE_FIXNUM(1)))
}

cl_object
si_package_hash_tables(cl_object p)
{
	assert_type_package(p);
	@(return p->pack.external p->pack.internal p->pack.uses)
}

cl_object
cl_delete_package(cl_object p)
{
	delete_package(p);
}

void
init_package(void)
{
	ecl_register_static_root(&package_list);
	ecl_register_static_root(&uninterned_list);

	lisp_package    = make_package(make_simple_string("COMMON-LISP"),
				       CONS(make_simple_string("CL"),
					    CONS(make_simple_string("LISP"),Cnil)),
				       Cnil);
	ecl_register_static_root(&lisp_package);
	user_package    = make_package(make_simple_string("COMMON-LISP-USER"),
				       CONS(make_simple_string("CL-USER"),
					    CONS(make_simple_string("USER"),Cnil)),
				       CONS(lisp_package, Cnil));
	ecl_register_static_root(&user_package);
	keyword_package = make_package(make_simple_string("KEYWORD"),
				       Cnil, Cnil);
	ecl_register_static_root(&keyword_package);
	system_package  = make_package(make_simple_string("SI"),
				       CONS(make_simple_string("SYSTEM"),
					    CONS(make_simple_string("SYS"),
						 Cnil)),
				       CONS(lisp_package, Cnil));
	ecl_register_static_root(&system_package);
#ifdef CLOS
	clos_package    = make_package(make_simple_string("CLOS"),
				       Cnil,
				       CONS(lisp_package, Cnil));
	ecl_register_static_root(&clos_package);
#endif
#ifdef TK
	tk_package      = make_package(make_simple_string("TK"),
				       Cnil,
				       CONS(lisp_package, Cnil));
	ecl_register_static_root(&tk_package);
#endif

	Cnil->symbol.hpack = lisp_package;
	cl_import2(Cnil, lisp_package);
	cl_export2(Cnil, lisp_package);

	Ct->symbol.hpack = lisp_package;
	cl_import2(Ct, lisp_package);
	cl_export2(Ct, lisp_package);
}
