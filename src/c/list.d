/*
    list.d -- List manipulating routines.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECLS is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include "ecls.h"
#include "ecls-inl.h"

/******************************* EXPORTS ******************************/

cl_object @':test';
cl_object @':test-not';
cl_object @':key';
cl_object @':initial-element';

/******************************* ------- ******************************/

#ifdef THREADS
#define test_function   clwp->lwp_test_function
#define item_compared   clwp->lwp_item_compared
#define tf              clwp->lwp_tf
#define key_function    clwp->lwp_key_function
#define kf              clwp->lwp_kf
#else
static cl_object test_function;
static cl_object item_compared;
static bool (*tf)();
static cl_object key_function;
static cl_object (*kf)();
#endif THREADS

#define TEST(x)         (*tf)(x)

#define saveTEST  \
	cl_object old_test_function = test_function;  \
	cl_object old_item_compared = item_compared;  \
	bool (*old_tf)() = tf;  \
	cl_object old_key_function = key_function;  \
	cl_object (*old_kf)() = kf;  \
	volatile bool eflag = FALSE

#define protectTEST  \
	if (frs_push(FRS_PROTECT, Cnil)) {  \
		eflag = TRUE;  \
		goto L;  \
	}

#define restoreTEST  \
L:  \
	frs_pop();  \
	test_function = old_test_function;  \
	item_compared = old_item_compared;  \
	tf = old_tf;  \
	key_function = old_key_function;  \
	kf = old_kf;  \
	if (eflag) unwind(nlj_fr, nlj_tag);

static bool
test_compare(cl_object x)
{
	cl_object test = funcall(3, test_function, item_compared, (*kf)(x));
	return (test != Cnil);
}

static bool
test_compare_not(cl_object x)
{
	cl_object test = funcall(3, test_function, item_compared, (*kf)(x));
	return (test == Cnil);
}

static bool
test_eql(cl_object x)
{
	return(eql(item_compared, (*kf)(x)));
}

static cl_object
apply_key_function(cl_object x)
{
	return funcall(2, key_function, x);
}

cl_object
identity(cl_object x)
{
	return(x);
}

static void
setupTEST(cl_object item, cl_object test, cl_object test_not, cl_object key)
{
	item_compared = item;
	if (test != Cnil) {
		if (test_not != Cnil)
		    FEerror("Both :TEST and :TEST-NOT are specified.", 0);
		test_function = test;
		tf = test_compare;
	} else if (test_not != Cnil) {
		test_function = test_not;
		tf = test_compare_not;
	} else
		tf = test_eql;
	if (key != Cnil) {
		key_function = key;
		kf = apply_key_function;
	} else
		kf = identity;
}

#define PREDICATE2(f)  \
cl_return f ## _if(int narg, cl_object pred, cl_object arg, cl_object key, cl_object val) \
{  \
	if (narg < 2)  \
		FEtoo_few_arguments(&narg);  \
	return f(narg+2, pred, arg, @':test', @'funcall', key, val);  \
}  \
\
cl_return f ## _if_not(int narg, cl_object pred, cl_object arg, cl_object key, cl_object val) \
{  \
	if (narg < 2)  \
		FEtoo_few_arguments(&narg);  \
	return f(narg+2, pred, arg, @':test-not', @'funcall', key, val);  \
}

#define PREDICATE3(f)  \
cl_return f ## _if(int narg, cl_object arg1, cl_object pred, cl_object arg3, cl_object key, cl_object val) \
{  \
	if (narg < 3)  \
		FEtoo_few_arguments(&narg);  \
	return f(narg+2, arg1, pred, arg3, @':test', @'funcall', key, val);  \
}  \
\
cl_return f ## _if_not(int narg, cl_object arg1, cl_object pred, cl_object arg3, cl_object key, \
	cl_object val)  \
{  \
	if (narg < 3)  \
		FEtoo_few_arguments(&narg);  \
	return f(narg+2, arg1, pred, arg3, @':test-not', @'funcall', key, val);  \
}

@(defun car (x)
@
	if (Null(x))
		@(return Cnil)
	if (ATOM(x))
		FEtype_error_list(x);
	@(return CAR(x))
@)

cl_object
car(cl_object x)
{
	if (Null(x))
		return(x);
	if (CONSP(x))
		return(CAR(x));
	FEtype_error_list(x);
}

@(defun cdr (x)
@
	if (Null(x))
		@(return Cnil)
	if (ATOM(x))
		FEtype_error_list(x);
	@(return CDR(x))
@)

cl_object
cdr(cl_object x)
{
	if (Null(x))
		return(x);
	if (CONSP(x))
		return(CDR(x));
	FEtype_error_list(x);
}

@(defun list (&rest args)
	cl_object list = Cnil, z;
@
	if (narg-- != 0) {
		list = z = CONS(va_arg(args, cl_object), Cnil);
		while (narg-- > 0) 
			z = CDR(z) = CONS(va_arg(args, cl_object), Cnil);
		}
	@(return list)
@)

cl_object
list(int narg, ...)
{
	cl_object p = Cnil, *z = &p;
	va_list args;

	va_start(args, narg);
	while (narg-- > 0)
		z = &CDR(*z = CONS(va_arg(args, cl_object), Cnil));
	return(p);
}

@(defun listX (&rest args)
	cl_object p = Cnil, *z=&p;
@
	if (narg == 0)
		FEtoo_few_arguments(&narg);
	while (--narg > 0)
		z = &CDR( *z = CONS(cl_nextarg(args), Cnil));
	*z = va_arg(args, cl_object);
	@(return p)
@)

cl_object
listX(int narg, ...)
{
	cl_object p = Cnil, *z = &p;
	va_list args;

	va_start(args, narg);
	while (--narg > 0)
		z = &CDR( *z = CONS(cl_nextarg(args), Cnil));
	*z = va_arg(args, cl_object);
	return(p);
}

static void
copy_list_to(cl_object x, cl_object **z)
{
	cl_object *y;

	y = *z;
	loop_for_in(x) {
		y = &CDR(*y = CONS(CAR(x), Cnil));
	} end_loop_for_in;
	*z = y;
}

@(defun append (&rest rest)
	cl_object x, *lastcdr;
@
	if (narg == 0)
		x = Cnil;
	else {
		lastcdr = &x;
		while (narg-- > 1)
			copy_list_to(cl_nextarg(rest), &lastcdr);
		*lastcdr = cl_nextarg(rest);
	}
	@(return x)
@)

cl_object
append(cl_object x, cl_object y)
{
	cl_object w, *z;

	z = &w;
	copy_list_to(x, &z);
	*z = y;
	return(w);
}

#if 1
/* Open coded CARs and CDRs */
#define car(foo) \
	(void)foo; \
	if (x != Cnil) { \
	   if (CONSP(x)) \
	     x = x->cons.car; \
	   else \
	     goto E; \
	}
#define cdr(foo) \
	(void)foo; \
	if (x != Cnil) { \
	   if (CONSP(x)) \
	     x = x->cons.cdr; \
	   else \
	     goto E; \
	}
#define defcxr(name, arg, code) \
cl_object name(cl_object foo) { \
	cl_object arg = foo; \
	code; return x; \
E:	FEtype_error_list(arg);} \
cl_return clL##name(int narg, cl_object arg) { \
	check_arg(1); \
	return1(name(arg)); \
}
#else
#define defcxr(name, arg, code) \
cl_object name(cl_object arg) { return code; } \
cl_return clL##name(int narg, cl_object arg) { \
	check_arg(1); \
	return1(name(arg)); \
}
#endif

defcxr(caar, x, car(car(x)))
defcxr(cadr, x, car(cdr(x)))
defcxr(cdar, x, cdr(car(x)))
defcxr(cddr, x, cdr(cdr(x)))
defcxr(caaar, x, car(car(car(x))))
defcxr(caadr, x, car(car(cdr(x))))
defcxr(cadar, x, car(cdr(car(x))))
defcxr(caddr, x, car(cdr(cdr(x))))
defcxr(cdaar, x, cdr(car(car(x))))
defcxr(cdadr, x, cdr(car(cdr(x))))
defcxr(cddar, x, cdr(cdr(car(x))))
defcxr(cdddr, x, cdr(cdr(cdr(x))))
defcxr(caaaar, x, car(car(car(car(x)))))
defcxr(caaadr, x, car(car(car(cdr(x)))))
defcxr(caadar, x, car(car(cdr(car(x)))))
defcxr(caaddr, x, car(car(cdr(cdr(x)))))
defcxr(cadaar, x, car(cdr(car(car(x)))))
defcxr(cadadr, x, car(cdr(car(cdr(x)))))
defcxr(caddar, x, car(cdr(cdr(car(x)))))
defcxr(cadddr, x, car(cdr(cdr(cdr(x)))))
defcxr(cdaaar, x, cdr(car(car(car(x)))))
defcxr(cdaadr, x, cdr(car(car(cdr(x)))))
defcxr(cdadar, x, cdr(car(cdr(car(x)))))
defcxr(cdaddr, x, cdr(car(cdr(cdr(x)))))
defcxr(cddaar, x, cdr(cdr(car(car(x)))))
defcxr(cddadr, x, cdr(cdr(car(cdr(x)))))
defcxr(cdddar, x, cdr(cdr(cdr(car(x)))))
defcxr(cddddr, x, cdr(cdr(cdr(cdr(x)))))
#undef car
#undef cdr

#define LENTH(n) (int narg, cl_object x) {\
	check_arg(1);\
	return1(nth(n, x));\
}
cl_return @fifth	LENTH(4)
cl_return @sixth	LENTH(5)
cl_return @seventh	LENTH(6)
cl_return @eighth	LENTH(7)
cl_return @ninth	LENTH(8)
cl_return @tenth	LENTH(9)
#undef LENTH

@(defun cons (car cdr)
@
	@(return CONS(car, cdr))
@)

static bool
tree_equal(cl_object x, cl_object y)
{
	cs_check(x);

BEGIN:
	if (CONSP(x))
		if (CONSP(y))
			if (tree_equal(CAR(x), CAR(y))) {
				x = CDR(x);
				y = CDR(y);
				goto BEGIN;
			} else
				return(FALSE);
		else
			return(FALSE);
	else {
		item_compared = x;
		if (TEST(y))
			return(TRUE);
		else
			return(FALSE);
	}
}

@(defun tree_equal (x y &key test test_not)
@
	setupTEST(Cnil, test, test_not, Cnil);
	if (tree_equal(x, y))
		@(return Ct)
	else
		@(return Cnil)
@)

@(defun endp (x)
@
	if (Null(x))
		@(return Ct)
	if (CONSP(x))
		@(return Cnil)
	FEtype_error_list(x);
@)

bool
endp1(cl_object x)
{
	if (CONSP(x))
		return(FALSE);
	if (Null(x))
		return(TRUE);
	FEtype_error_list(x);
}

cl_object
list_length(cl_object x)
{
	cl_fixnum n;
	cl_object fast, slow;

	/* INV: A list's length always fits in a fixnum */
	fast = slow = x;
	for (n = 0; CONSP(fast); n++, fast = CDR(fast)) {
		if (n & 1) {
			/* Circular list? */
			if (slow == fast) return Cnil;
			slow = CDR(slow);
		}
	}
	if (fast != Cnil)
		FEtype_error_proper_list(x);
	return MAKE_FIXNUM(n);
}

@(defun list_length (x)
@
	@(return list_length(x))
@)

@(defun nth (n x)
@
	@(return nth(fixint(n), x))
@)

cl_object
nth(cl_fixnum n, cl_object x)
{
	if (n < 0)
		FEtype_error_index(MAKE_FIXNUM(n));
	/* INV: No need to check for circularity since we visit
	   at most `n' conses */
	for (; n > 0 && CONSP(x); n--)
		x = CDR(x);
	if (x == Cnil)
		return Cnil;
	if (CONSP(x))
		return CAR(x);
	FEtype_error_list(x);
}

@(defun nthcdr (n x)
@
	@(return nthcdr(fixint(n), x))
@)

cl_object
nthcdr(cl_fixnum n, cl_object x)
{
	if (n < 0)
		FEtype_error_index(MAKE_FIXNUM(n));
	while (n-- > 0 && !ENDP(x))
		x = CDR(x);
	return(x);
}

@(defun last (l &optional (k MAKE_FIXNUM(1)))
	cl_object r;
	cl_fixnum n;
@
	n = fixnnint(k);
	r = l;
	loop_for_on(l) {
		if (n) n--; else r = CDR(r);
	} end_loop_for_on;
	@(return r)
@)

@(defun make_list (size &key initial_element &aux x)
	cl_fixnum i;
@
	if (!FIXNUMP(size))
		FEerror("Cannot make a list of the size ~D.", 1, size);
	i = fixnnint(size);
	while (i-- > 0)
		x = CONS(initial_element, x);
	@(return x)
@)

@(defun copy_list (x)
@
	@(return copy_list(x))
@)

/*
	Copy_list(x) copies list x.
*/
cl_object
copy_list(cl_object x)
{
	cl_object copy;
	cl_object *y = &copy;

	loop_for_on(x) {
		y = &CDR(*y = CONS(CAR(x), Cnil));
	} end_loop_for_on;
	*y = x;
	return copy;
}

@(defun copy_alist (x)
@
	@(return copy_alist(x))
@)

/*
	Copy_alist(x) copies alist x.
*/
cl_object
copy_alist(cl_object x)
{
	cl_object copy;
	cl_object *y = &copy;

	loop_for_on(x) {
		cl_object pair = CAR(x);
		if (ATOM(pair))
			FEtype_error_alist(x);
		*y = CONS(CONS(CAR(pair), CDR(pair)), Cnil);
		y = &CDR(*y);
	} end_loop_for_on;
	*y = x;
	return copy;
}

@(defun copy_tree (x)
@
	@(return copy_tree(x))
@)

/*
	Copy_tree(x) returns a copy of tree x.
*/
cl_object
copy_tree(cl_object x)
{
	cs_check(x);
	if (ATOM(x))
		return x;
	return CONS(copy_tree(CAR(x)), copy_tree(CDR(x)));
}

@(defun revappend (x y)
@
	loop_for_in(x) {
		y = CONS(CAR(x),y);
	} end_loop_for_in;
	@(return y)
@)

@(defun nconc (&rest lists)
	cl_object x, l,*lastcdr;
@	
	if (narg < 1)
		@(return Cnil)
	lastcdr = &x;
	while (narg-- > 1) {
		*lastcdr = l = va_arg(lists, cl_object);
		loop_for_on(l) {
			lastcdr = &CDR(l);
		} end_loop_for_on;
	}
	*lastcdr = va_arg(lists, cl_object);
	@(return x)
@)

cl_object
nconc(cl_object l, cl_object y)
{
	cl_object x = l, x1;

	if (x == Cnil)
		return y;
	/* INV: This loop is run at least once */
	loop_for_on(x) {
		x1 = x;
	} end_loop_for_on;
	CDR(x1) = y;
	return l;
}

@(defun nreconc (l y)
	cl_object x, z;
@
	/* INV: when a circular list is "reconc'ed", the pointer ends
	   up at the beginning of the original list, hence we need no
	   slow pointer */
	for (x = l; CONSP(x); ) {
		z = x;
		x = CDR(x);
		if (x == l) FEcircular_list(l);
		CDR(z) = y;
		y = z;
	}
	if (x != Cnil)
		FEtype_error_proper_list(l);
	@(return y)
@)

/* CONTINUE HERE!!!! */
@(defun butlast (lis &optional (nn MAKE_FIXNUM(1)))
	cl_object r, res = Cnil, *fill = &res;
	cl_fixnum delay;
@
	/* INV: No list has more than MOST_POSITIVE_FIX elements */
	if (!FIXNUMP(nn))
		@(return Cnil)
	delay = fixnnint(nn);
	r = lis;
	loop_for_on(lis) {
		if (delay)
			delay--;
		else {
			fill = &CDR(*fill = CONS(CAR(r), Cnil));
			r = CDR(r);
		}
	} end_loop_for_on;
	@(return res)
@)

@(defun nbutlast (lis &optional (nn MAKE_FIXNUM(1)))
	cl_fixnum delay;
	cl_object x, r;
@
	/* INV: No list has more than MOST_POSITIVE_FIX elements */
	if (!FIXNUMP(nn))
		@(return Cnil)
	/* We add 1 because at the end `r' must point to the
	   cons that must be modified */
	delay = fixnnint(nn)+1;
	r = x = lis;
	loop_for_on(x) {
		if (delay) delay--; else r = CDR(r);
	} end_loop_for_on;
	if (delay > 0)
		/* nn > length(lis) */
		lis = Cnil;
	else
		CDR(r) = Cnil;
	@(return lis)
@)

@(defun ldiff (x y)
	cl_object res = Cnil, *fill = &res;
@
	loop_for_on(x) {
		if (x == y)
			break;
		else
			fill = &CDR(*fill = CONS(CAR(x), Cnil));
	} end_loop_for_on;
	@(return res)
@)

@(defun rplaca (x v)
@
	assert_type_cons(x);
	CAR(x) = v;
	@(return x)
@)

@(defun rplacd (x v)
@
	assert_type_cons(x);
	CDR(x) = v;
	@(return x)
@)

@(defun subst (new old tree &key test test_not key)
	saveTEST;
@
	protectTEST;
	setupTEST(old, test, test_not, key);
	tree = subst(new, tree);
	restoreTEST;
	@(return tree)
@)


/*
	Subst(new, tree) returns
	the result of substituting new in tree.
*/
cl_object
subst(cl_object new, cl_object tree)
{
	cs_check(new);

	if (TEST(tree))
		return(new);
	else if (CONSP(tree))
		return(CONS(subst(new, CAR(tree)), subst(new, CDR(tree))));
	else
		return(tree);
}

PREDICATE3(@subst)

@(defun nsubst (new old tree &key test test_not key)
	saveTEST;
@
	protectTEST;
	setupTEST(old, test, test_not, key);
	nsubst(new, &tree);
	restoreTEST;
	@(return tree)
@)

/*
	Nsubst(new, treep) stores
	the result of nsubstituting new in *treep
	to *treep.
*/
void
nsubst(cl_object new, cl_object *treep)
{
	cs_check(new);

	if (TEST(*treep))
		*treep = new;
	else if (CONSP(*treep)) {
		nsubst(new, &CAR(*treep));
		nsubst(new, &CDR(*treep));
	}
}

PREDICATE3(@nsubst)

@(defun sublis (alist tree &key test test_not key)
	saveTEST;
@
	protectTEST;
	setupTEST(Cnil, test, test_not, key);
	tree = sublis(alist, tree);
	restoreTEST;
	@(return tree)
@)

/*
	Sublis(alist, tree) returns
	result of substituting tree by alist.
*/
cl_object
sublis(cl_object alist, cl_object tree)
{
	cl_object x = alist;

	cs_check(alist);
	loop_for_in(x) {
		item_compared = car(CAR(x));
		if (TEST(tree)) return(cdr(CAR(x)));
	} end_loop_for_in;
	if (CONSP(tree))
		return(CONS(sublis(alist, CAR(tree)), sublis(alist, CDR(tree))));
	else
		return(tree);
}

@(defun nsublis (alist tree &key test test_not key)
	saveTEST;
@
	protectTEST;
	setupTEST(Cnil, test, test_not, key);
	nsublis(alist, &tree);
	restoreTEST;
	@(return tree)
@)

/*
	Nsublis(alist, treep) stores
	the result of substiting *treep by alist
	to *treep.
*/
void
nsublis(cl_object alist, cl_object *treep)
{
	cl_object x = alist;

	cs_check(alist);
	loop_for_in(x) {
		item_compared = car(CAR(x));
		if (TEST(*treep)) {
			*treep = CDAR(x);
			return;
		}
	} end_loop_for_in;
	if (CONSP(*treep)) {
		nsublis(alist, &CAR(*treep));
		nsublis(alist, &CDR(*treep));
	}
}

@(defun member (item list &key test test_not key)
	saveTEST;
@
	protectTEST;
	setupTEST(item, test, test_not, key);
	loop_for_in(list) {
		if (TEST(CAR(list)))
			goto L;
	} end_loop_for_in;
	restoreTEST;
	@(return list)
@)

bool
member_eq(cl_object x, cl_object l)
{
	loop_for_in(l) {
		if (x == CAR(l))
			return(TRUE);
	} end_loop_for_in;
	return(FALSE);
}

@(defun si::memq (x l)
@
	loop_for_in(l) {
		if (x == CAR(l))
			@(return l)
	} end_loop_for_in;
	@(return Cnil)
@)

/* Added for use by the compiler, instead of open coding them. Beppe */
cl_object
memq(cl_object x, cl_object l)
{
	loop_for_in(l) {
		if (x == CAR(l))
			return(l);
	} end_loop_for_in;
	return(Cnil);
}

cl_object
memql(cl_object x, cl_object l)
{
	loop_for_in(l) {
		if (eql(x, CAR(l)))
			return(l);
	} end_loop_for_in;
	return(Cnil);
}

cl_object
member(cl_object x, cl_object l)
{
	loop_for_in(l) {
		if (equal(x, CAR(l)))
			return(l);
	} end_loop_for_in;
	return(Cnil);
}
/* End of addition. Beppe */

PREDICATE2(@member)

@(defun si::member1 (item list &key test test_not key)
	saveTEST;
@
	protectTEST;
	if (key != Cnil)
		item = funcall(2, key, item);
	setupTEST(item, test, test_not, key);
	loop_for_in(list) {
		if (TEST(CAR(list)))
			goto L;
	} end_loop_for_in;
	restoreTEST;
	@(return list)
@)

@(defun tailp (y x)
@
	loop_for_on(x) {
		if (x == y)
			@(return Ct)
	} end_loop_for_on;
	@(return ((x == y)? Ct : Cnil))
@)

cl_return
@adjoin(int narg, cl_object item, cl_object list, cl_object k1, cl_object v1,
	 cl_object k2, cl_object v2, cl_object k3, cl_object v3)
{
	cl_object output;

	if (narg < 2)
		FEtoo_few_arguments(&narg);
	output = @si::member1(narg, item, list, k1, v1, k2, v2, k3, v3);
	if (Null(output))
		output = CONS(item, list);
	else
		output = list;
	return1(output);
}

@(defun acons (x y z)
@
	@(return CONS(CONS(x, y), z))
@)

@(defun pairlis (keys data &optional a_list)
	cl_object k, d;
@
	k = keys;
	d = data;
	loop_for_in(k) {
		if (ENDP(d))
			goto error;
		a_list = CONS(CONS(CAR(k), CAR(d)), a_list);
		d = CDR(d);
	} end_loop_for_in;
	if (!ENDP(d))
error:	    FEerror("The keys ~S and the data ~S are not of the same length",
		    2, keys, data);
	@(return a_list)
@)


@(defun assoc (item a_list &key test test_not key)
	saveTEST;
@
	protectTEST;
	setupTEST(item, test, test_not, key);
	loop_for_in(a_list) {
	  cl_object pair = CAR(a_list);
	  if (Null(pair))
	    ;
	  else if (ATOM(pair))
	    FEtype_error_alist(pair);
	  else if (TEST(CAAR(a_list))) {
	    a_list = CAR(a_list);
	    goto L;
	  }
	} end_loop_for_in;
	restoreTEST;
	@(return a_list)
@)

@(defun rassoc (item a_list &key test test_not key)
	saveTEST;
@
	protectTEST;
	setupTEST(item, test, test_not, key);
	loop_for_in(a_list) {
	  cl_object pair = CAR(a_list);
	  if (Null(pair))
	    ;
	  else if (ATOM(pair))
	    FEtype_error_alist(pair);
	  else if (TEST(CDAR(a_list))) {
	    a_list = CAR(a_list);
	    goto L;
	  }
	} end_loop_for_in;
	restoreTEST;
	@(return a_list)
@)

/* Added for use by the compiler, instead of open coding them. Beppe */
cl_object
assq(cl_object x, cl_object l)
{
	loop_for_in(l) {
		if (x == CAAR(l))
			return(CAR(l));
	} end_loop_for_in;
	return(Cnil);
}

cl_object
assql(cl_object x, cl_object l)
{
	loop_for_in(l) {
		if (eql(x, CAAR(l)))
			return(CAR(l));
	} end_loop_for_in;
	return(Cnil);
}

cl_object
assoc(cl_object x, cl_object l)
{
	loop_for_in(l) {
		if (equal(x, CAAR(l)))
			return(CAR(l));
	} end_loop_for_in;
	return(Cnil);
}

cl_object
assqlp(cl_object x, cl_object l)
{
	loop_for_in(l) {
		if (equalp(x, CAR(CAR(l))))
			return(CAR(l));
	} end_loop_for_in;
	return(Cnil);
}
/* End of addition. Beppe */

PREDICATE2(@assoc)
PREDICATE2(@rassoc)
