/* -*- mode: c; c-basic-offset: 8 -*- */
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

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>

struct cl_test {
	bool (*test_c_function)(struct cl_test *, cl_object);
	cl_object (*key_c_function)(struct cl_test *, cl_object);
	cl_object test_function;
	cl_object item_compared;
	cl_object key_function;
	cl_object frame_key;
	struct ecl_stack_frame frame_key_aux;
	cl_object frame_test;
	struct ecl_stack_frame frame_test_aux;
	cl_object frame_args[3];
};

static cl_object subst(struct cl_test *t, cl_object new_obj, cl_object tree);
static cl_object nsubst(struct cl_test *t, cl_object new_obj, cl_object tree);
static cl_object sublis(struct cl_test *t, cl_object alist, cl_object tree);
static cl_object nsublis(struct cl_test *t, cl_object alist, cl_object tree);
static cl_object do_assoc(struct cl_test *t, cl_object alist);

#define TEST(t,k) ((t)->test_c_function)((t),(k))

static bool
test_compare(struct cl_test *t, cl_object x)
{
	ecl_stack_frame_elt_set(t->frame_test, 0, t->item_compared);
	x = (t->key_c_function)(t, x);
	ecl_stack_frame_elt_set(t->frame_test, 1, x);
	return ecl_apply_from_stack_frame(t->frame_test, t->test_function) != Cnil;
}

static bool
test_compare_not(struct cl_test *t, cl_object x)
{
	ecl_stack_frame_elt_set(t->frame_test, 0, t->item_compared);
	x = (t->key_c_function)(t, x);
	ecl_stack_frame_elt_set(t->frame_test, 1, x);
	return ecl_apply_from_stack_frame(t->frame_test, t->test_function) == Cnil;
}

static bool
test_eq(struct cl_test *t, cl_object x)
{
	return (t->item_compared == (t->key_c_function)(t, x));
}

static bool
test_eql(struct cl_test *t, cl_object x)
{
	return ecl_eql(t->item_compared, (t->key_c_function)(t, x));
}

static bool
test_equal(struct cl_test *t, cl_object x)
{
	return ecl_equal(t->item_compared, (t->key_c_function)(t, x));
}

static bool
test_equalp(struct cl_test *t, cl_object x)
{
	return ecl_equalp(t->item_compared, (t->key_c_function)(t, x));
}

static cl_object
key_function(struct cl_test *t, cl_object x)
{
	ecl_stack_frame_elt_set(t->frame_key, 0, x);
	return ecl_apply_from_stack_frame(t->frame_key, t->key_function);
}

static cl_object
key_identity(struct cl_test *t, cl_object x)
{
	return x;
}

static void
setup_test(struct cl_test *t, cl_object item, cl_object test,
	   cl_object test_not, cl_object key)
{
	t->item_compared = item;
	t->test_function = t->key_function =Cnil;
	if (test != Cnil) {
		if (test_not != Cnil)
		    FEerror("Both :TEST and :TEST-NOT are specified.", 0);
		t->test_function = si_coerce_to_function(test);
		if (t->test_function == SYM_FUN(@'eq')) {
			t->test_c_function = test_eq;
		} else if (t->test_function == SYM_FUN(@'eql')) {
			t->test_c_function = test_eql;
		} else if (t->test_function == SYM_FUN(@'equal')) {
			t->test_c_function = test_equal;
		} else if (t->test_function == SYM_FUN(@'equalp')) {
			t->test_c_function = test_equalp;
		} else {
			t->test_c_function = test_compare;
		}
	} else if (test_not != Cnil) {
		t->test_function = si_coerce_to_function(test_not);
		t->test_c_function = test_compare_not;
	} else {
		t->test_c_function = test_eql;
	}
	if (key != Cnil) {
		t->key_function = key;
		t->key_c_function = key_function;
	} else {
		t->key_c_function = key_identity;
	}
	if (t->test_function != Cnil) {
		t->frame_test = (cl_object)&(t->frame_test_aux);
		t->frame_test_aux.t = t_frame;
		t->frame_test_aux.bottom = t->frame_args;
		t->frame_test_aux.top = t->frame_args + 2;
		t->frame_test_aux.stack = 0;
	}
	if (t->key_function != Cnil) {
		t->frame_key = (cl_object)&(t->frame_key_aux);
		t->frame_key_aux.t = t_frame;
		t->frame_key_aux.bottom = t->frame_args + 2;
		t->frame_key_aux.top = t->frame_args + 3;
		t->frame_key_aux.stack = 0;
	}
}

static void close_test(struct cl_test *t)
{
	/* No need to call ecl_stack_frame_close since this frame is not allocated
	 * in the lisp stack. */
	/*
	if (t->key_function != Cnil) {
		ecl_stack_frame_close(t->frame_key);
	}
	if (t->test_function != Cnil) {
		ecl_stack_frame_close(t->frame_test);
	}
	*/
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
		return1(ECL_CONS_CDR(x));
	FEtype_error_list(x);
}

@(defun list (&rest args)
	cl_object head = Cnil;
@
	if (narg--) {
		cl_object tail = head = ecl_list1(cl_va_arg(args));
		while (narg--) {
			cl_object cons = ecl_list1(cl_va_arg(args));
			ECL_RPLACD(tail, cons);
			tail = cons;
		}
	}
	@(return head)
@)

@(defun list* (&rest args)
	cl_object head;
@
	if (narg == 0)
		FEwrong_num_arguments(@'list*');
	head = cl_va_arg(args);
	if (--narg) {
		cl_object tail = head = ecl_list1(head);
		while (--narg) {
			cl_object cons = ecl_list1(cl_va_arg(args));
			ECL_RPLACD(tail, cons);
			tail = cons;
		}
		ECL_RPLACD(tail, cl_va_arg(args));
	}
	@(return head)
@)

static cl_object
append_into(cl_object tail, cl_object l)
{
	if (!Null(ECL_CONS_CDR(tail))) {
		/* (APPEND '(1 . 2) 3) */
		FEtype_error_proper_list(tail);
	}
	while (CONSP(l)) {
		cl_object cons = ecl_list1(ECL_CONS_CAR(l));
		ECL_RPLACD(tail, cons);
		tail = cons;
		l = ECL_CONS_CDR(l);
	}
	ECL_RPLACD(tail, l);
	return tail;
}

@(defun append (&rest rest)
	cl_object head = Cnil, tail = Cnil;
@
	while (narg--) {
		cl_object other = cl_va_arg(rest);
		if (Null(head)) {
			head = other;
			if (Null(head))
				continue;
			if (!LISTP(head)) {
				if (narg) {
					/* (APPEND atom whatever) */
					FEtype_error_list(other);
				}
				/* (APPEND atom) */
				break;
			}
			other = ECL_CONS_CDR(head);
			tail = head = ecl_list1(ECL_CONS_CAR(head));
		}
		tail = append_into(tail, other);
	}
	@(return head)
@)

cl_object
ecl_append(cl_object x, cl_object y)
{
	cl_object head, tail;
	if (Null(x))
		return y;
	if (!LISTP(x))
		FEtype_error_list(x);
	head = tail = ecl_list1(ECL_CONS_CAR(x));
	append_into(append_into(tail, ECL_CONS_CDR(x)), y);
	return head;
}

/* Open coded CARs and CDRs */
#define car(foo) \
	foo; \
	if (!LISTP(x)) goto E; \
	if (!Null(x)) x = ECL_CONS_CAR(x);
#define cdr(foo) \
	foo; \
	if (!LISTP(x)) goto E; \
	if (!Null(x)) x = ECL_CONS_CDR(x);
#define defcxr(name, arg, code) \
cl_object cl_##name(cl_object foo) { \
	register cl_object arg = foo; \
	code; return1(arg); \
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
	return1(ecl_nth(n, x));\
}
cl_object @fifth	LENTH(4)
cl_object @sixth	LENTH(5)
cl_object @seventh	LENTH(6)
cl_object @eighth	LENTH(7)
cl_object @ninth	LENTH(8)
cl_object @tenth	LENTH(9)
#undef LENTH

static bool
tree_equal(struct cl_test *t, cl_object x, cl_object y)
{
BEGIN:
	if (CONSP(x)) {
		if (CONSP(y)) {
			if (tree_equal(t, ECL_CONS_CAR(x), ECL_CONS_CAR(y))) {
				x = ECL_CONS_CDR(x);
				y = ECL_CONS_CDR(y);
				goto BEGIN;
			} else {
				return(FALSE);
			}
		} else {
			return(FALSE);
		}
	} else {
		t->item_compared = x;
		if (TEST(t, y))
			return(TRUE);
		else
			return(FALSE);
	}
}

@(defun tree_equal (x y &key test test_not)
	struct cl_test t;
	cl_object output;
@
	setup_test(&t, Cnil, test, test_not, Cnil);
	output = tree_equal(&t, x, y)? Ct : Cnil;
	close_test(&t);
	@(return output)
@)

cl_object
cl_endp(cl_object x)
{
	if (Null(x))
		@(return Ct)
	if (LISTP(x))
		@(return Cnil)
	FEtype_error_list(x);
}

bool
ecl_endp(cl_object x)
{
	if (Null(x))
		return(TRUE);
	if (LISTP(x))
		return(FALSE);
	FEtype_error_list(x);
}

cl_object
cl_list_length(cl_object x)
{
	cl_fixnum n;
	cl_object fast, slow;
	/* INV: A list's length always fits in a fixnum */
	fast = slow = x;
	for (n = 0; !Null(fast); n++, fast = ECL_CONS_CDR(fast)) {
		if (!LISTP(fast)) {
			FEtype_error_list(fast);
		}
		if (n & 1) {
			/* Circular list? */
			if (slow == fast) @(return Cnil);
			slow = ECL_CONS_CDR(slow);
		}
	}
	@(return MAKE_FIXNUM(n));
}

cl_object
cl_nth(cl_object n, cl_object x)
{
	@(return ecl_nth(fixint(n), x))
}

cl_object
ecl_nth(cl_fixnum n, cl_object x)
{
	if (n < 0)
		FEtype_error_index(x, MAKE_FIXNUM(n));
	/* INV: No need to check for circularity since we visit
	   at most `n' conses */
	for (; n > 0 && CONSP(x); n--)
		x = ECL_CONS_CDR(x);
	if (Null(x))
		return Cnil;
	if (!LISTP(x))
		FEtype_error_list(x);
	return ECL_CONS_CAR(x);
}

cl_object
cl_nthcdr(cl_object n, cl_object x)
{
	@(return ecl_nthcdr(fixint(n), x))
}

cl_object
ecl_nthcdr(cl_fixnum n, cl_object x)
{
	if (n < 0)
		FEtype_error_index(x, MAKE_FIXNUM(n));
	while (n-- > 0 && !Null(x)) {
		if (LISTP(x)) {
			x = ECL_CONS_CDR(x);
		} else {
			FEtype_error_list(x);
		}
	}
	return x;
}

cl_object
ecl_last(cl_object l, cl_index n)
{
	/* The algorithm is very simple. We run over the list with
	 * two pointers, "l" and "r". The separation between both
	 * must be "n", so that when "l" finds no more conses, "r"
	 * contains the output. */
	cl_object r;
	for (r = l; n && CONSP(r); n--, r = ECL_CONS_CDR(r))
		;
	/* If "l" has not moved, we have to ensure that it is a list */
	if (r == l) {
		if (!LISTP(r)) FEtype_error_list(l);
		while (CONSP(r)) {
			r = ECL_CONS_CDR(r);
		}
		return r;
	} else if (n == 0) {
		while (CONSP(r)) {
			r = ECL_CONS_CDR(r);
			l = ECL_CONS_CDR(l);
		}
		return l;
	} else {
		return l;
	}
}

@(defun last (l &optional (k MAKE_FIXNUM(1)))
	cl_object r;
	cl_fixnum n;
@
	if (type_of(k) == t_bignum)
		@(return l)
	@(return ecl_last(l, fixnnint(k)))
@)

@(defun make_list (size &key initial_element &aux x)
	cl_fixnum i;
@
	/* INV: fixnnint() signals a type-error if SIZE is not a integer >=0 */
	i = fixnnint(size);
	while (i-- > 0)
		x = CONS(initial_element, x);
	@(return x)
@)

cl_object
cl_copy_list(cl_object x)
{
	cl_object copy;
	if (!LISTP(x)) {
		FEtype_error_list(x);
	}
	copy = Cnil;
	if (!Null(x)) {
		cl_object tail = copy = ecl_list1(CAR(x));
		while (x = ECL_CONS_CDR(x), CONSP(x)) {
			cl_object cons = ecl_list1(ECL_CONS_CAR(x));
			ECL_RPLACD(tail, cons);
			tail = cons;
		}
		ECL_RPLACD(tail, x);
	}
	@(return copy);
}

static cl_object
duplicate_pairs(cl_object x)
{
	cl_object p = ECL_CONS_CAR(x);
	if (CONSP(p))
		p = CONS(ECL_CONS_CAR(p), ECL_CONS_CDR(p));
	return ecl_list1(p);
}

cl_object
cl_copy_alist(cl_object x)
{
	cl_object copy;
	if (!LISTP(x)) {
		FEtype_error_list(x);
	}
	copy = Cnil;
	if (!Null(x)) {
		cl_object tail = copy = duplicate_pairs(x);
		while (x = ECL_CONS_CDR(x), !Null(x)) {
			if (!LISTP(x)) {
				FEtype_error_list(x);
			} else {
				cl_object cons = duplicate_pairs(x);
				tail = ECL_RPLACD(tail, cons);
				tail = cons;
			}
		}
	}
	@(return copy);
}

static cl_object
do_copy_tree(cl_object x)
{
	if (CONSP(x)) {
		x = CONS(do_copy_tree(ECL_CONS_CAR(x)),
			 do_copy_tree(ECL_CONS_CDR(x)));
	}
	return x;
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
		y = CONS(ECL_CONS_CAR(x),y);
	} end_loop_for_in;
	@(return y)
}

@(defun nconc (&rest lists)
	cl_object head = Cnil, tail = Cnil;
@	
	while (narg--) {
		cl_object new_tail, other = cl_va_arg(lists);
		if (Null(other)) {
			new_tail = tail;
		} else if (CONSP(other)) {
			new_tail = ecl_last(other, 1);
		} else {
			if (narg) FEtype_error_list(other);
			new_tail = tail;
		}
		if (Null(head)) {
			head = other;
		} else {
			ECL_RPLACD(tail, other);
		}
		tail = new_tail;
	}
	@(return head)
@)

cl_object
ecl_nconc(cl_object l, cl_object y)
{
	if (Null(l)) {
		return y;
	} else {
		ECL_RPLACD(ecl_last(l, 1), y);
		return l;
	}
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
		x = ECL_CONS_CDR(x);
		if (x == l) FEcircular_list(l);
		ECL_RPLACD(z, y);
		y = z;
	}
	if (x != Cnil)
		FEtype_error_list(x);
	@(return y)
}

cl_object
ecl_butlast(cl_object l, cl_index n)
{
	/* See LAST for details on this algorithm */
	cl_object r;
	for (r = l; n && CONSP(r); n--, r = ECL_CONS_CDR(r))
		;
	if (Null(r)) {
		return Cnil;
	} else if (!LISTP(r)) {
		/* We reach here either because l is shorter than n conses,
		 * or because it is not a list */
		if (r == l) FEtype_error_list(r);
		return Cnil;
	} else {
		/* We reach here because l has at least n conses and
		 * thus we can take CAR(l) */
		cl_object head, tail;
		head = tail = ecl_list1(CAR(l));
		while (l = ECL_CONS_CDR(l), r = ECL_CONS_CDR(r), CONSP(r)) {
			cl_object cons = ecl_list1(ECL_CONS_CAR(l));
			ECL_RPLACD(tail, cons);
			tail = cons;
		}
		return head;
	}
}

@(defun butlast (lis &optional (nn MAKE_FIXNUM(1)))
	cl_object r, res = Cnil, *fill = &res;
	cl_fixnum delay;
@
	/* INV: No list has more than MOST_POSITIVE_FIXNUM elements */
	if (type_of(nn) == t_bignum)
		@(return Cnil);
	/* INV: fixnnint() signas a type-error if NN is not an integer >=0 */
	@(return ecl_butlast(lis, fixnnint(nn)))
@)

cl_object
ecl_nbutlast(cl_object l, cl_index n)
{
	cl_object r;
	if (!LISTP(l))
		FEtype_error_list(l);
	for (n++, r = l; n && CONSP(r); n--, r = ECL_CONS_CDR(r))
		;
	if (n == 0) {
		cl_object tail = l;
		while (CONSP(r)) {
			tail = ECL_CONS_CDR(tail);
			r = ECL_CONS_CDR(r);
		}
		ECL_RPLACD(tail, Cnil);
		return l;
	}
	return Cnil;
}

@(defun nbutlast (lis &optional (nn MAKE_FIXNUM(1)))
@
	/* INV: No list has more than MOST_POSITIVE_FIXNUM elements */
	if (type_of(nn) == t_bignum)
		@(return Cnil)
	/* INV: fixnnint() signas a type-error if NN is not an integer >=0 */
	@(return ecl_nbutlast(lis, fixnnint(nn)))
@)

cl_object
cl_ldiff(cl_object x, cl_object y)
{
	cl_object head = Cnil;
	if (!LISTP(x)) {
		FEtype_error_list(x);
	}
	/* Here we use that, if X or Y are CONS, then (EQL X Y)
	 * only when X == Y */
	if (!Null(x) && (x != y)) {
		cl_object tail = head = ecl_list1(ECL_CONS_CAR(x));
		while (1) {
			x = ECL_CONS_CDR(x);
			if (!CONSP(x)) {
				if (!ecl_eql(x, y)) {
					ECL_RPLACD(tail, x);
				}
				break;
			} else if (x == y) {
				break;
			} else {
				cl_object cons = ecl_list1(ECL_CONS_CAR(x));
				ECL_RPLACD(tail, cons);
				tail = cons;
			}
		}
	}
	@(return head)
}

cl_object
cl_rplaca(cl_object x, cl_object v)
{
	assert_type_cons(x);
	ECL_RPLACA(x, v);
	@(return x)
}

cl_object
cl_rplacd(cl_object x, cl_object v)
{
	assert_type_cons(x);
	ECL_RPLACD(x, v);
	@(return x)
}

@(defun subst (new_obj old_obj tree &key test test_not key)
	struct cl_test t;
	cl_object output;
@
	setup_test(&t, old_obj, test, test_not, key);
	output = subst(&t, new_obj, tree);
	close_test(&t);
	@(return output)
@)


static cl_object
subst(struct cl_test *t, cl_object new_obj, cl_object tree)
{
	if (TEST(t, tree)) {
		return new_obj;
	} else if (ATOM(tree)) {
		return tree;
	} else {
		cl_object head, tail = Cnil;
		do {
			cl_object cons = subst(t, new_obj, ECL_CONS_CAR(tree));
			cons = ecl_cons(cons, tree = ECL_CONS_CDR(tree));
			if (Null(tail)) {
				head = cons;
			} else {
				ECL_RPLACD(tail, cons);
			}
			tail = cons;
			if (TEST(t, tree)) {
				ECL_RPLACD(tail, new_obj);
				return head;
			}
		} while (CONSP(tree));
		return head;
	}
}

@(defun nsubst (new_obj old_obj tree &key test test_not key)
	struct cl_test t;
@
	setup_test(&t, old_obj, test, test_not, key);
	tree = nsubst(&t, new_obj, tree);
	close_test(&t);
	@(return tree)
@)

static cl_object
nsubst_cons(struct cl_test *t, cl_object new_obj, cl_object tree)
{
	cl_object l = tree;
	do {
		cl_object o = ECL_CONS_CAR(l);
		if (TEST(t, o)) {
			ECL_RPLACA(l, new_obj);
		} else if (CONSP(o)) {
			nsubst_cons(t, new_obj, o);
		}
		o = ECL_CONS_CDR(l);
		if (TEST(t, o)) {
			ECL_RPLACD(l, new_obj);
			return tree;
		}
		l = o;
	} while (CONSP(l));
	return tree;
}

static cl_object
nsubst(struct cl_test *t, cl_object new_obj, cl_object tree)
{
	if (TEST(t, tree))
		return new_obj;
	if (CONSP(tree))
		return nsubst_cons(t, new_obj, tree);
	return tree;
}

@(defun sublis (alist tree &key test test_not key)
	/* t[0] is the test for the objects in the tree, configured
	   with test, test_not and key. t[1] is the test for searching
	   in the association list.
	 */
	struct cl_test t[2];
@
	setup_test(t, Cnil, Cnil, Cnil, key);
	setup_test(t+1, Cnil, test, test_not, Cnil);
	tree = sublis(t, alist, tree);
	close_test(t+1);
	close_test(t);
	@(return tree)
@)

/*
	Sublis(alist, tree) returns
	result of substituting tree by alist.
*/
static cl_object
sublis(struct cl_test *t, cl_object alist, cl_object tree)
{
	cl_object node;
	t[1].item_compared = (t[0].key_c_function)(t, tree);
	node = do_assoc(t+1, alist);
	if (!Null(node)) {
		return ECL_CONS_CDR(node);
	}
	if (CONSP(tree)) {
		tree = CONS(sublis(t, alist, ECL_CONS_CAR(tree)),
			    sublis(t, alist, ECL_CONS_CDR(tree)));
	}
	return tree;
}

@(defun nsublis (alist tree &key test test_not key)
	/* t[0] is the test for the objects in the tree, configured
	   with test, test_not and key. t[1] is the test for searching
	   in the association list.
	 */
	struct cl_test t[2];
@
	setup_test(t, Cnil, Cnil, Cnil, key);
	setup_test(t+1, Cnil, test, test_not, Cnil);
	tree = nsublis(t, alist, tree);
	close_test(t+1);
	close_test(t);
	@(return tree)
@)

/*
	Nsublis(alist, treep) stores
	the result of substiting *treep by alist
	to *treep.
*/
static cl_object
nsublis(struct cl_test *t, cl_object alist, cl_object tree)
{
	cl_object node;
	t[1].item_compared = (t[0].key_c_function)(t, tree);
	node = do_assoc(t+1, alist);
	if (!Null(node)) {
		return ECL_CONS_CDR(node);
	}
	if (CONSP(tree)) {
		ECL_RPLACA(tree, nsublis(t, alist, ECL_CONS_CAR(tree)));
		ECL_RPLACD(tree, nsublis(t, alist, ECL_CONS_CDR(tree)));
	}
	return tree;
}

@(defun member (item list &key test test_not key)
	struct cl_test t;
@
	setup_test(&t, item, test, test_not, key);
	loop_for_in(list) {
		if (TEST(&t, ECL_CONS_CAR(list)))
			break;
	} end_loop_for_in;
	close_test(&t);
	@(return list)
@)

bool
ecl_member_eq(cl_object x, cl_object l)
{
	loop_for_in(l) {
		if (x == ECL_CONS_CAR(l))
			return(TRUE);
	} end_loop_for_in;
	return(FALSE);
}

cl_object
si_memq(cl_object x, cl_object l)
{
	loop_for_in(l) {
		if (x == ECL_CONS_CAR(l))
			@(return l)
	} end_loop_for_in;
	@(return Cnil)
}

/* Added for use by the compiler, instead of open coding them. Beppe */
cl_object
ecl_memql(cl_object x, cl_object l)
{
	loop_for_in(l) {
		if (ecl_eql(x, ECL_CONS_CAR(l)))
			return(l);
	} end_loop_for_in;
	return(Cnil);
}

cl_object
ecl_member(cl_object x, cl_object l)
{
	loop_for_in(l) {
		if (ecl_equal(x, ECL_CONS_CAR(l)))
			return(l);
	} end_loop_for_in;
	return(Cnil);
}
/* End of addition. Beppe */

cl_object
si_member1(cl_object item, cl_object list, cl_object test, cl_object test_not, cl_object key)
{
	struct cl_test t;

	if (key != Cnil)
		item = funcall(2, key, item);
	setup_test(&t, item, test, test_not, key);
	loop_for_in(list) {
		if (TEST(&t, ECL_CONS_CAR(list)))
			break;
	} end_loop_for_in;
	close_test(&t);
	@(return list)
}

cl_object
cl_tailp(cl_object y, cl_object x)
{
	loop_for_on(x) {
		if (ecl_eql(x, y))
			@(return Ct)
	} end_loop_for_on;
	return cl_eql(x, y);
}

@(defun adjoin (item list &key test test_not key)
	cl_object output;
@
	if (narg < 2)
		FEwrong_num_arguments(@'adjoin');
	output = @si::member1(item, list, test, test_not, key);
	if (Null(output))
		output = CONS(item, list);
	else
		output = list;
	@(return output)
@)

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
		if (ecl_endp(d))
			goto error;
		a_list = CONS(CONS(ECL_CONS_CAR(k), ECL_CONS_CAR(d)), a_list);
		d = CDR(d);
	} end_loop_for_in;
	if (!ecl_endp(d))
error:	    FEerror("The keys ~S and the data ~S are not of the same length",
		    2, keys, data);
	@(return a_list)
@)


@(defun assoc (item a_list &key test test_not key)
	struct cl_test t;
@
	setup_test(&t, item, test, test_not, key);
	a_list = do_assoc(&t, a_list);
	close_test(&t);
	@(return a_list)
@)

static cl_object
do_assoc(struct cl_test *t, cl_object a_list)
{
	loop_for_in(a_list) {
		cl_object pair = ECL_CONS_CAR(a_list);
		if (!Null(pair)) {
			if (!LISTP(pair))
				FEtype_error_list(pair);
			if (TEST(t, ECL_CONS_CAR(pair)))
				return pair;
		}
	} end_loop_for_in;
	return Cnil;
}

@(defun rassoc (item a_list &key test test_not key)
	struct cl_test t;
@
	setup_test(&t, item, test, test_not, key);
	loop_for_in(a_list) {
		cl_object pair = ECL_CONS_CAR(a_list);
		if (!Null(pair)) {
			if (!LISTP(pair))
				FEtype_error_list(pair);
			if (TEST(&t, ECL_CONS_CDR(pair))) {
				a_list = pair;
				break;
			}
		}
	} end_loop_for_in;
	close_test(&t);
	@(return a_list)
@)

cl_object
ecl_remove_eq(cl_object x, cl_object l)
{
	cl_object head = Cnil, tail = Cnil;
	loop_for_on_unsafe(l) {
		if (ECL_CONS_CAR(l) != x) {
			cl_object cons = ecl_list1(ECL_CONS_CAR(l));
			if (Null(tail)) {
				head = tail = cons;
			} else {
				ECL_RPLACD(tail, cons);
				tail = cons;
			}
		}
	} end_loop_for_on;
	return head;
}

/* Added for use by the compiler, instead of open coding them. Beppe */
cl_object
ecl_assq(cl_object x, cl_object l)
{
	loop_for_in(l) {
		cl_object pair = ECL_CONS_CAR(l);
		if (x == CAR(pair))
			return pair;
	} end_loop_for_in;
	return(Cnil);
}

cl_object
ecl_assql(cl_object x, cl_object l)
{
	loop_for_in(l) {
		cl_object pair = ECL_CONS_CAR(l);
		if (ecl_eql(x, CAR(pair)))
			return pair;
	} end_loop_for_in;
	return(Cnil);
}

cl_object
ecl_assoc(cl_object x, cl_object l)
{
	loop_for_in(l) {
		cl_object pair = ECL_CONS_CAR(l);
		if (ecl_equal(x, CAR(pair)))
			return pair;
	} end_loop_for_in;
	return(Cnil);
}

cl_object
ecl_assqlp(cl_object x, cl_object l)
{
	loop_for_in(l) {
		cl_object pair = ECL_CONS_CAR(l);
		if (ecl_equalp(x, CAR(pair)))
			return pair;
	} end_loop_for_in;
	return(Cnil);
}
/* End of addition. Beppe */
