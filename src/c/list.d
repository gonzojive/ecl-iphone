/*
    list.d -- List manipulating routines.
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
#include "ecl-inl.h"

#ifdef THREADS
#define test_function   clwp->lwp_test_function
#define item_compared   clwp->lwp_item_compared
#define tf              clwp->lwp_tf
#define key_function    clwp->lwp_key_function
#define kf              clwp->lwp_kf
#else
static cl_object test_function;
static cl_object item_compared;
static bool (*tf)(cl_object);
static cl_object key_function;
static cl_object (*kf)(cl_object);
#endif /* THREADS */

#define TEST(x)         (*tf)(x)

#define saveTEST  \
	cl_object old_test_function = test_function;  \
	cl_object old_item_compared = item_compared;  \
	bool (*old_tf)(cl_object) = tf;  \
	cl_object old_key_function = key_function;  \
	cl_object (*old_kf)(cl_object) = kf;  \
	volatile bool eflag = FALSE

#define restoreTEST  \
	test_function = old_test_function;  \
	item_compared = old_item_compared;  \
	tf = old_tf;  \
	key_function = old_key_function;  \
	kf = old_kf;  \

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

static cl_object
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
		kf = cl_identity;
}

#define PREDICATE2(f,name)  \
cl_return f ## _if(int narg, cl_object pred, cl_object arg, cl_object key, cl_object val) \
{  \
	if (narg < 2)  \
		FEwrong_num_arguments(name);  \
	return f(narg+2, pred, arg, @':test', @'funcall', key, val);  \
}  \
\
cl_return f ## _if_not(int narg, cl_object pred, cl_object arg, cl_object key, cl_object val) \
{  \
	if (narg < 2)  \
		FEwrong_num_arguments(name);  \
	return f(narg+2, pred, arg, @':test-not', @'funcall', key, val);  \
}

#define PREDICATE3(f,name)  \
cl_return f ## _if(int narg, cl_object arg1, cl_object pred, cl_object arg3, cl_object key, cl_object val) \
{  \
	if (narg < 3)  \
		FEwrong_num_arguments(name);  \
	return f(narg+2, arg1, pred, arg3, @':test', @'funcall', key, val);  \
}  \
\
cl_return f ## _if_not(int narg, cl_object arg1, cl_object pred, cl_object arg3, cl_object key, \
	cl_object val)  \
{  \
	if (narg < 3)  \
		FEwrong_num_arguments(name);  \
	return f(narg+2, arg1, pred, arg3, @':test-not', @'funcall', key, val);  \
}

cl_object
cl_car(cl_object x)
{
	if (Null(x))
		return1(x);
	if (CONSP(x))
		return1(CAR(x));
	FEtype_error_list(x);
}

cl_object
cl_cdr(cl_object x)
{
	if (Null(x))
		return1(x);
	if (CONSP(x))
		return1(CDR(x));
	FEtype_error_list(x);
}

@(defun list (&rest args)
	cl_object list = Cnil, z;
@
	if (narg-- != 0) {
		list = z = CONS(cl_va_arg(args), Cnil);
		while (narg-- > 0) 
			z = CDR(z) = CONS(cl_va_arg(args), Cnil);
		}
	@(return list)
@)

@(defun list* (&rest args)
	cl_object p = Cnil, *z=&p;
@
	if (narg == 0)
		FEwrong_num_arguments(narg);
	while (--narg > 0)
		z = &CDR( *z = CONS(cl_va_arg(args), Cnil));
	*z = cl_va_arg(args);
	@(return p)
@)

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
			copy_list_to(cl_va_arg(rest), &lastcdr);
		*lastcdr = cl_va_arg(rest);
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
cl_object cl_##name(cl_object foo) { \
	cl_object arg = foo; \
	code; return1(x); \
E:	FEtype_error_list(arg);}

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

#define LENTH(n) (cl_object x) {\
	return1(nth(n, x));\
}
cl_return @fifth	LENTH(4)
cl_return @sixth	LENTH(5)
cl_return @seventh	LENTH(6)
cl_return @eighth	LENTH(7)
cl_return @ninth	LENTH(8)
cl_return @tenth	LENTH(9)
#undef LENTH

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

cl_object
cl_endp(cl_object x)
{
	if (Null(x))
		@(return Ct)
	if (CONSP(x))
		@(return Cnil)
	FEtype_error_list(x);
}

bool
endp(cl_object x)
{
	if (CONSP(x))
		return(FALSE);
	if (Null(x))
		return(TRUE);
	FEtype_error_list(x);
}

cl_object
cl_list_length(cl_object x)
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
	@(return MAKE_FIXNUM(n));
}

cl_object
cl_nth(cl_object n, cl_object x)
{
	@(return nth(fixint(n), x))
}

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

cl_object
cl_nthcdr(cl_object n, cl_object x)
{
	@(return nthcdr(fixint(n), x))
}

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

cl_object
cl_copy_list(cl_object x)
{
	cl_object copy;
	cl_object *y = &copy;

	loop_for_on(x) {
		y = &CDR(*y = CONS(CAR(x), Cnil));
	} end_loop_for_on;
	*y = x;
	@(return copy);
}

cl_object
cl_copy_alist(cl_object x)
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
	@(return copy);
}

static cl_object
do_copy_tree(cl_object x)
{
	cs_check(x);
	if (ATOM(x))
		return x;
	return CONS(do_copy_tree(CAR(x)), do_copy_tree(CDR(x)));
}

cl_object
cl_copy_tree(cl_object x)
{
	@(return do_copy_tree(x))
}

cl_object
cl_revappend(cl_object x, cl_object y)
{
	loop_for_in(x) {
		y = CONS(CAR(x),y);
	} end_loop_for_in;
	@(return y)
}

@(defun nconc (&rest lists)
	cl_object x, l,*lastcdr;
@	
	if (narg < 1)
		@(return Cnil)
	lastcdr = &x;
	while (narg-- > 1) {
		*lastcdr = l = cl_va_arg(lists);
		loop_for_on(l) {
			lastcdr = &CDR(l);
		} end_loop_for_on;
	}
	*lastcdr = cl_va_arg(lists);
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

cl_object
cl_nreconc(cl_object l, cl_object y)
{
	cl_object x, z;
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
}

/* CONTINUE HERE!!!! */
@(defun butlast (lis &optional (nn MAKE_FIXNUM(1)))
	cl_object r, res = Cnil, *fill = &res;
	cl_fixnum delay;
@
	/* INV: No list has more than MOST_POSITIVE_FIXNUM elements */
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
	/* INV: No list has more than MOST_POSITIVE_FIXNUM elements */
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

cl_object
cl_ldiff(cl_object x, cl_object y)
{
	cl_object res = Cnil, *fill = &res;

	loop_for_on(x) {
		if (x == y)
			break;
		else
			fill = &CDR(*fill = CONS(CAR(x), Cnil));
	} end_loop_for_on;
	@(return res)
}

cl_object
cl_rplaca(cl_object x, cl_object v)
{
	assert_type_cons(x);
	CAR(x) = v;
	@(return x)
}

cl_object
cl_rplacd(cl_object x, cl_object v)
{
	assert_type_cons(x);
	CDR(x) = v;
	@(return x)
}

@(defun subst (new_obj old_obj tree &key test test_not key)
	saveTEST;
@
	CL_UNWIND_PROTECT_BEGIN {
		setupTEST(old_obj, test, test_not, key);
		tree = subst(new_obj, tree);
	} CL_UNWIND_PROTECT_EXIT {
		restoreTEST;
	} CL_UNWIND_PROTECT_END;
	@(return tree)
@)


/*
	Subst(new, tree) returns
	the result of substituting new in tree.
*/
cl_object
subst(cl_object new_obj, cl_object tree)
{
	cs_check(new_obj);

	if (TEST(tree))
		return(new_obj);
	else if (CONSP(tree))
		return(CONS(subst(new_obj, CAR(tree)), subst(new_obj, CDR(tree))));
	else
		return(tree);
}

PREDICATE3(@subst, @'subst')

@(defun nsubst (new_obj old_obj tree &key test test_not key)
	saveTEST;
@
	CL_UNWIND_PROTECT_BEGIN {
		setupTEST(old_obj, test, test_not, key);
		nsubst(new_obj, &tree);
	} CL_UNWIND_PROTECT_EXIT {
		restoreTEST;
	} CL_UNWIND_PROTECT_END;
	@(return tree)
@)

/*
	Nsubst(new, treep) stores
	the result of nsubstituting new in *treep
	to *treep.
*/
void
nsubst(cl_object new_obj, cl_object *treep)
{
	cs_check(new_obj);

	if (TEST(*treep))
		*treep = new_obj;
	else if (CONSP(*treep)) {
		nsubst(new_obj, &CAR(*treep));
		nsubst(new_obj, &CDR(*treep));
	}
}

PREDICATE3(@nsubst, @'nsubst')

@(defun sublis (alist tree &key test test_not key)
	saveTEST;
@
	CL_UNWIND_PROTECT_BEGIN {
		setupTEST(Cnil, test, test_not, key);
		tree = sublis(alist, tree);
	} CL_UNWIND_PROTECT_EXIT {
		restoreTEST;
	} CL_UNWIND_PROTECT_END;
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
		item_compared = cl_car(CAR(x));
		if (TEST(tree)) return(cl_cdr(CAR(x)));
	} end_loop_for_in;
	if (CONSP(tree))
		return(CONS(sublis(alist, CAR(tree)), sublis(alist, CDR(tree))));
	else
		return(tree);
}

@(defun nsublis (alist tree &key test test_not key)
	saveTEST;
@
	CL_UNWIND_PROTECT_BEGIN {
		setupTEST(Cnil, test, test_not, key);
		nsublis(alist, &tree);
	} CL_UNWIND_PROTECT_EXIT {
		restoreTEST;
	} CL_UNWIND_PROTECT_END;
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
		item_compared = cl_car(CAR(x));
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
	CL_UNWIND_PROTECT_BEGIN {
		setupTEST(item, test, test_not, key);
		loop_for_in(list) {
			if (TEST(CAR(list)))
				break;
		} end_loop_for_in;
	} CL_UNWIND_PROTECT_EXIT {
		restoreTEST;
	} CL_UNWIND_PROTECT_END;
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

cl_object
si_memq(cl_object x, cl_object l)
{
	loop_for_in(l) {
		if (x == CAR(l))
			@(return l)
	} end_loop_for_in;
	@(return Cnil)
}

/* Added for use by the compiler, instead of open coding them. Beppe */
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

PREDICATE2(@member, @'member')

@(defun si::member1 (item list &key test test_not key)
	saveTEST;
@
	CL_UNWIND_PROTECT_BEGIN {
		if (key != Cnil)
			item = funcall(2, key, item);
		setupTEST(item, test, test_not, key);
		loop_for_in(list) {
			if (TEST(CAR(list)))
				break;
		} end_loop_for_in;
	} CL_UNWIND_PROTECT_EXIT {
		restoreTEST;
	} CL_UNWIND_PROTECT_END;
	@(return list)
@)

cl_object
cl_tailp(cl_object y, cl_object x)
{
	loop_for_on(x) {
		if (x == y)
			@(return Ct)
	} end_loop_for_on;
	@(return ((x == y)? Ct : Cnil))
}

cl_return
@adjoin(int narg, cl_object item, cl_object list, cl_object k1, cl_object v1,
	 cl_object k2, cl_object v2, cl_object k3, cl_object v3)
{
	cl_object output;

	if (narg < 2)
		FEwrong_num_arguments(narg);
	output = @si::member1(narg, item, list, k1, v1, k2, v2, k3, v3);
	if (Null(output))
		output = CONS(item, list);
	else
		output = list;
	return1(output);
}

cl_object
cl_cons(cl_object x, cl_object y)
{
	@(return CONS(x, y))
}

cl_object
cl_acons(cl_object x, cl_object y, cl_object z)
{
	@(return CONS(CONS(x, y), z))
}

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
	CL_UNWIND_PROTECT_BEGIN {
		setupTEST(item, test, test_not, key);
		loop_for_in(a_list) {
			cl_object pair = CAR(a_list);
			if (Null(pair))
				;
			else if (ATOM(pair))
				FEtype_error_alist(pair);
			else if (TEST(CAAR(a_list))) {
				a_list = CAR(a_list);
				break;
			}
		} end_loop_for_in;
	} CL_UNWIND_PROTECT_EXIT {
		restoreTEST;
	} CL_UNWIND_PROTECT_END;
	@(return a_list)
@)

@(defun rassoc (item a_list &key test test_not key)
	saveTEST;
@
	CL_UNWIND_PROTECT_BEGIN {
		setupTEST(item, test, test_not, key);
		loop_for_in(a_list) {
			cl_object pair = CAR(a_list);
			if (Null(pair))
				;
			else if (ATOM(pair))
				FEtype_error_alist(pair);
			else if (TEST(CDAR(a_list))) {
				a_list = CAR(a_list);
				break;
			}
		} end_loop_for_in;
	} CL_UNWIND_PROTECT_EXIT {
		restoreTEST;
	} CL_UNWIND_PROTECT_END;
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

PREDICATE2(@assoc, @'assoc')
PREDICATE2(@rassoc, @'rassoc')
