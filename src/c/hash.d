/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    hash.d  -- Hash tables.
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
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include "newhash.h"

static void corrupted_hash(cl_object hashtable) /*__attribute__((noreturn))*/;

#define SYMBOL_NAME(x) (Null(x)? Cnil_symbol->symbol.name : (x)->symbol.name)

static void
corrupted_hash(cl_object hashtable)
{
	FEerror("internal error, corrupted hashtable ~S", 1, hashtable);
}

static cl_hashkey
_hash_eql(cl_hashkey h, cl_object x)
{
 BEGIN:
	switch (type_of(x)) {
	case t_bignum:
#ifdef WITH_GMP
		return hash_string(h, (unsigned char*)x->big.big_limbs,
				   labs(x->big.big_size) * sizeof(mp_limb_t));
#else  /* WITH_GMP */
                return hash_word(h, (cl_index)(x->big.big_num));
#endif /* WITH_GMP */
	case t_ratio:
		h = _hash_eql(h, x->ratio.num);
		return _hash_eql(h, x->ratio.den);
#ifdef ECL_SHORT_FLOAT
	case t_shortfloat: {
		float f = ecl_short_float(x);
		return hash_string(h, (unsigned char*)&f, sizeof(f));
	}
#endif
	case t_singlefloat:
		return hash_string(h, (unsigned char*)&sf(x), sizeof(sf(x)));
	case t_doublefloat:
		return hash_string(h, (unsigned char*)&df(x), sizeof(df(x)));
#ifdef ECL_LONG_FLOAT
	case t_longfloat: {
		long double d = ecl_long_float(x);
		return hash_string(h, (unsigned char*)&d, sizeof(d));
	}
#endif
	case t_complex:
		h = _hash_eql(h, x->complex.real);
		return _hash_eql(h, x->complex.imag);
	case t_character:
		return hash_word(h, CHAR_CODE(x));
	default:
		return hash_word(h, ((cl_hashkey)x >> 2));
	}
}

static cl_hashkey
_hash_equal(int depth, cl_hashkey h, cl_object x)
{
	switch (type_of(x)) {
	case t_list:
		if (Null(x)) {
			return _hash_equal(depth, h, Cnil_symbol->symbol.name);
		}
		if (--depth == 0) {
			return h;
		} else {
			h = _hash_equal(depth, h, ECL_CONS_CAR(x));
			return _hash_equal(depth, h, ECL_CONS_CDR(x));
		}
	case t_symbol:
		x = x->symbol.name;
#ifdef ECL_UNICODE
	case t_base_string:
		return hash_base_string(x->base_string.self, x->base_string.fillp, h);
	case t_string:
		return hash_full_string(x->base_string.self, x->base_string.fillp, h);
#else
	case t_base_string:
		return hash_string(h, x->base_string.self, x->base_string.fillp);
#endif
	case t_pathname:
		h = _hash_equal(0, h, x->pathname.directory);
		h = _hash_equal(0, h, x->pathname.name);
		h = _hash_equal(0, h, x->pathname.type);
		h = _hash_equal(0, h, x->pathname.host);
		h = _hash_equal(0, h, x->pathname.device);
		return _hash_equal(0, h, x->pathname.version);
	case t_bitvector:
		/* Notice that we may round out some bits. We must do this
		 * because the fill pointer may be set in the middle of a byte.
		 * If so, the extra bits _must_ _not_ take part in the hash,
		 * because otherwise two bit arrays which are EQUAL might
		 * have different hash keys. */
		return hash_string(h, x->vector.self.ch, x->vector.fillp / 8);
	case t_random:
		return _hash_equal(0, h, x->random.value);
	default:
		return _hash_eql(h, x);
	}
}

static cl_hashkey
_hash_equalp(int depth, cl_hashkey h, cl_object x)
{
	cl_index i, len;
	switch (type_of(x)) {
	case t_character:
		return hash_word(h, toupper(CHAR_CODE(x)));
	case t_list:
		if (Null(x)) {
			return _hash_equalp(depth, h, Cnil_symbol->symbol.name);
		}
		if (--depth == 0) {
			return h;
		} else {
			h = _hash_equalp(depth, h, ECL_CONS_CAR(x));
			return _hash_equalp(depth, h, ECL_CONS_CDR(x));
		}
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string:
	case t_vector:
	case t_bitvector:
		len = x->vector.fillp;
		goto SCAN;
	case t_array:
		len = x->vector.dim;
	SCAN:	if (--depth) {
			for (i = 0; i < len; i++) {
				h = _hash_equalp(depth, h, ecl_aref(x, i));
			}
		}
		return h;
	case t_fixnum:
		return hash_word(h, fix(x));
#ifdef HAVE_SHORT_FLOAT
	case t_shortfloat: {
		/* FIXME! We should be more precise here! */
		return hash_word(h, (cl_index)sf(x));
		union { float f; cl_index w; } x;
		x.w = 0;
		x.f = ecl_short_float(x);
		return hash_word(h, x.w);
	}
#endif
	case t_singlefloat:
		/* FIXME! We should be more precise here! */
		return hash_word(h, (cl_index)sf(x));
	case t_doublefloat:
		/* FIXME! We should be more precise here! */
		return hash_word(h, (cl_index)df(x));
	case t_bignum:
		/* FIXME! We should be more precise here! */
	case t_ratio:
		h = _hash_equalp(0, h, x->ratio.num);
		return _hash_equalp(0, h, x->ratio.den);
	case t_complex:
		h = _hash_equalp(0, h, x->complex.real);
		return _hash_equalp(0, h, x->complex.imag);
	case t_instance:
	case t_hashtable:
		/* FIXME! We should be more precise here! */
		return hash_word(h, 42);
	default:
		return _hash_equal(depth, h, x);
	}
}

struct ecl_hashtable_entry *
ecl_search_hash(cl_object key, cl_object hashtable)
{
	cl_hashkey h;
	cl_index hsize, i, j, k;
	struct ecl_hashtable_entry *e;
	cl_object hkey, ho;
	int htest;
	bool b;

	htest = hashtable->hash.test;
	hsize = hashtable->hash.size;
	j = hsize;
	switch (htest) {
	case htt_eq:	h = (cl_hashkey)key >> 2; break;
	case htt_eql:	h = _hash_eql(0, key); break;
	case htt_equal:	h = _hash_equal(3, 0, key); break;
	case htt_equalp:h = _hash_equalp(3, 0, key); break;
	case htt_pack:	h = _hash_equal(3, 0, key);
			ho = MAKE_FIXNUM(h & 0xFFFFFFF);
			break;
	default:	corrupted_hash(hashtable);
	}
	i = h % hsize;
	for (k = 0; k < hsize;  i = (i + 1) % hsize, k++) {
		e = &hashtable->hash.data[i];
		hkey = e->key;
		if (hkey == OBJNULL) {
			if (e->value == OBJNULL)
				if (j == hsize)
					return(e);
				else
					return(&hashtable->hash.data[j]);
			else
				if (j == hsize)
					j = i;
				else if (j == i)
				  /* this was never returning --wfs
				     but looping around with j=0 */
				  return(e);
			continue;
		}
		switch (htest) {
		case htt_eq:	b = key == hkey; break;
		case htt_eql:	b = ecl_eql(key, hkey); break;
		case htt_equal: b = ecl_equal(key, hkey); break;
		case htt_equalp:b = ecl_equalp(key, hkey); break;
		case htt_pack:	b = (ho==hkey) && ecl_string_eq(key,SYMBOL_NAME(e->value));
				break;
		}
		if (b)
			return(&hashtable->hash.data[i]);
	}
	return(&hashtable->hash.data[j]);
}

cl_object
ecl_gethash(cl_object key, cl_object hashtable)
{
	cl_object output;

	assert_type_hash_table(hashtable);
	HASH_TABLE_LOCK(hashtable);
	output = ecl_search_hash(key, hashtable)->value;
	HASH_TABLE_UNLOCK(hashtable);
	return output;
}

cl_object
ecl_gethash_safe(cl_object key, cl_object hashtable, cl_object def)
{
	struct ecl_hashtable_entry *e;

	assert_type_hash_table(hashtable);
	HASH_TABLE_LOCK(hashtable);
	e = ecl_search_hash(key, hashtable);
	if (e->key != OBJNULL)
		def = e->value;
	HASH_TABLE_UNLOCK(hashtable);
	return def;
}

static void
add_new_to_hash(cl_object key, cl_object hashtable, cl_object value)
{
	int htest;
	cl_hashkey h;
	cl_index i, hsize;
	struct ecl_hashtable_entry *e;

	/* INV: hashtable has the right type */
	htest = hashtable->hash.test;
	hsize = hashtable->hash.size;
	switch (htest) {
	case htt_eq:	h = (cl_hashkey)key >> 2; break;
	case htt_eql:	h = _hash_eql(0, key); break;
	case htt_equal:	h = _hash_equal(3, 0, key); break;
	case htt_equalp:h = _hash_equalp(3, 0, key); break;
	case htt_pack:	h = _hash_equal(3, 0, key); break;
	default:	corrupted_hash(hashtable);
	}
	e = hashtable->hash.data;
	for (i = h % hsize; ; i = (i + 1) % hsize)
		if (e[i].key == OBJNULL) {
			hashtable->hash.entries++;
			if (htest == htt_pack)
				e[i].key = MAKE_FIXNUM(h & 0xFFFFFFF);
			else
				e[i].key = key;
			e[i].value = value;
			return;
		}
	corrupted_hash(hashtable);
}

void
ecl_sethash(cl_object key, cl_object hashtable, cl_object value)
{
	cl_index i;
	struct ecl_hashtable_entry *e;

	assert_type_hash_table(hashtable);
	HASH_TABLE_LOCK(hashtable);
	e = ecl_search_hash(key, hashtable);
	if (e->key != OBJNULL) {
		e->value = value;
		goto OUTPUT;
	}
	i = hashtable->hash.entries + 1;
	if (i >= hashtable->hash.size ||
	    i >= (hashtable->hash.size * hashtable->hash.factor)) {
		ecl_extend_hashtable(hashtable);
	}
	add_new_to_hash(key, hashtable, value);
 OUTPUT:
	HASH_TABLE_UNLOCK(hashtable);
}

void
ecl_extend_hashtable(cl_object hashtable)
{
	cl_object old, key;
	cl_index old_size, new_size, i;
	cl_object new_size_obj;

	assert_type_hash_table(hashtable);
	old_size = hashtable->hash.size;
	/* We do the computation with lisp datatypes, just in case the sizes contain
	 * weird numbers */
	if (FIXNUMP(hashtable->hash.rehash_size)) {
		new_size_obj = ecl_plus(hashtable->hash.rehash_size, MAKE_FIXNUM(old_size));
	} else {
		new_size_obj = ecl_times(hashtable->hash.rehash_size, MAKE_FIXNUM(old_size));
		new_size_obj = ecl_ceiling1(new_size_obj);
	}
	if (!FIXNUMP(new_size_obj)) {
		/* New size is too large */
		new_size = old_size * 2;
	} else {
		new_size = fix(new_size_obj);
	}
	old = cl_alloc_object(t_hashtable);
	old->hash = hashtable->hash;
	hashtable->hash.data = NULL; /* for GC sake */
	hashtable->hash.entries = 0;
	hashtable->hash.size = new_size;
	hashtable->hash.data = (struct ecl_hashtable_entry *)
	  cl_alloc(new_size * sizeof(struct ecl_hashtable_entry));
	for (i = 0;  i < new_size;  i++) {
		hashtable->hash.data[i].key = OBJNULL;
		hashtable->hash.data[i].value = OBJNULL;
	}
	for (i = 0;  i < old_size;  i++)
		if ((key = old->hash.data[i].key) != OBJNULL) {
			if (hashtable->hash.test == htt_pack)
				key = SYMBOL_NAME(old->hash.data[i].value);
			add_new_to_hash(key, hashtable, old->hash.data[i].value);
		}
}


@(defun make_hash_table (&key (test @'eql')
			      (size MAKE_FIXNUM(1024))
			      (rehash_size ecl_make_singlefloat(1.5))
			      (rehash_threshold ecl_make_singlefloat(0.7))
			      (lockable Cnil))
@
	@(return cl__make_hash_table(test, size, rehash_size, rehash_threshold,
				     lockable))
@)

static void
do_clrhash(cl_object ht)
{
	/*
	 * Fill a hash with null pointers and ensure it does not have
	 * any entry. We separate this routine because it is needed
	 * both by clrhash and hash table initialization.
	 */
	cl_index i;
	ht->hash.entries = 0;
	for(i = 0; i < ht->hash.size; i++) {
		ht->hash.data[i].key = OBJNULL;
		ht->hash.data[i].value = OBJNULL;
	}
}

cl_object
cl__make_hash_table(cl_object test, cl_object size, cl_object rehash_size,
		    cl_object rehash_threshold, cl_object lockable)
{
	int htt;
	cl_index hsize;
	cl_object h;
	double factor;
	/*
	 * Argument checking
	 */
	if (test == @'eq' || test == SYM_FUN(@'eq'))
		htt = htt_eq;
	else if (test == @'eql' || test == SYM_FUN(@'eql'))
		htt = htt_eql;
	else if (test == @'equal' || test == SYM_FUN(@'equal'))
		htt = htt_equal;
	else if (test == @'equalp' || test == SYM_FUN(@'equalp'))
		htt = htt_equalp;
	else
		FEerror("~S is an illegal hash-table test function.",
			1, test);
	hsize = ecl_fixnum_in_range(@'make-hash-table',"size",size,0,ATOTLIM);;
	if (hsize < 16) {
		hsize = 16;
	}
 AGAIN:
	if (ecl_minusp(rehash_size)) {
	ERROR1:
		rehash_size =
			ecl_type_error(@'make-hash-table',"rehash-size",
				       rehash_size,
				       c_string_to_object("(OR (INTEGER 1 *) (FLOAT 0 (1)))"));
		goto AGAIN;
	}
	if (floatp(rehash_size)) {
		if (ecl_number_compare(rehash_size, MAKE_FIXNUM(1)) < 0 ||
		    ecl_minusp(rehash_size)) {
			goto ERROR1;
		}
		rehash_size = ecl_make_doublefloat(ecl_to_double(rehash_size));
	} else if (!FIXNUMP(rehash_size)) {
		goto ERROR1;
	}
	while (!ecl_numberp(rehash_threshold) ||
	       ecl_minusp(rehash_threshold) ||
	       ecl_number_compare(rehash_threshold, MAKE_FIXNUM(1)) > 0)
	{
		rehash_threshold =
			ecl_type_error(@'make-hash-table',"rehash-threshold",
				       rehash_threshold,
				       c_string_to_object("(REAL 0 1)"));
	}
	/*
	 * Build actual hash.
	 */
	h = cl_alloc_object(t_hashtable);
	h->hash.test = htt;
	h->hash.size = hsize;
        h->hash.entries = 0;
	h->hash.data = NULL;	/* for GC sake */
	h->hash.data = (struct ecl_hashtable_entry *)
		cl_alloc(hsize * sizeof(struct ecl_hashtable_entry));
	do_clrhash(h);

	h->hash.rehash_size = rehash_size;
	h->hash.threshold = rehash_threshold;
	h->hash.factor = ecl_to_double(rehash_threshold);
	if (h->hash.factor < 0.1) {
		h->hash.factor = 0.1;
	}
	h->hash.lockable = !Null(lockable);
#ifdef ECL_THREADS
	if (h->hash.lockable) {
#if defined(_MSC_VER) || defined(mingw32)
		h->hash.lock = CreateMutex(NULL, FALSE, NULL);
#else
		pthread_mutex_init(&h->hash.lock, NULL);
#endif
	}
#endif
	return h;
}

cl_object
cl_hash_table_p(cl_object ht)
{
	@(return ((type_of(ht) == t_hashtable) ? Ct : Cnil))
}

@(defun gethash (key ht &optional (no_value Cnil))
	struct ecl_hashtable_entry e;
@
	assert_type_hash_table(ht);
	HASH_TABLE_LOCK(ht);
	e = *ecl_search_hash(key, ht);
	HASH_TABLE_UNLOCK(ht);
	if (e.key != OBJNULL)
		@(return e.value Ct)
	else
		@(return no_value Cnil)
@)

cl_object
si_hash_set(cl_object key, cl_object ht, cl_object val)
{
	/* INV: ecl_sethash() checks the type of hashtable */
	ecl_sethash(key, ht, val);
	@(return val)
}

bool
ecl_remhash(cl_object key, cl_object hashtable)
{
	struct ecl_hashtable_entry *e;
	bool output;

	assert_type_hash_table(hashtable);
	HASH_TABLE_LOCK(hashtable);
	e = ecl_search_hash(key, hashtable);
	if (e->key == OBJNULL) {
		output = FALSE;
	} else {
		e->key = OBJNULL;
		e->value = Cnil;
		hashtable->hash.entries--;
		output = TRUE;
	}
	HASH_TABLE_UNLOCK(hashtable);
	return output;
}

cl_object
cl_remhash(cl_object key, cl_object ht)
{
	/* INV: ecl_search_hash() checks the type of hashtable */
	@(return (ecl_remhash(key, ht)? Ct : Cnil));
}

cl_object
cl_clrhash(cl_object ht)
{
	assert_type_hash_table(ht);
	if (ht->hash.entries) {
		HASH_TABLE_LOCK(ht);
		do_clrhash(ht);
		HASH_TABLE_UNLOCK(ht);
	}
	@(return ht)
}

cl_object
cl_hash_table_test(cl_object ht)
{
	cl_object output;
	assert_type_hash_table(ht);
	switch(ht->hash.test) {
	    case htt_eq: output = @'eq'; break;
	    case htt_eql: output = @'eql'; break;
	    case htt_equal: output = @'equal'; break;
	    case htt_equalp: output = @'equalp'; break;
	    case htt_pack:
	    default: output = @'equal';
	}
	@(return output)
}

cl_object
cl_hash_table_size(cl_object ht)
{
	assert_type_hash_table(ht);
	@(return MAKE_FIXNUM(ht->hash.size))
}

cl_object
cl_hash_table_count(cl_object ht)
{
	assert_type_hash_table(ht);
	@(return (MAKE_FIXNUM(ht->hash.entries)))
}

static cl_object
si_hash_table_iterate(cl_narg narg, cl_object env)
{
	cl_object index = CAR(env);
	cl_object ht = CADR(env);
	cl_fixnum i;
	if (!Null(index)) {
		i = fix(index);
		if (i < 0)
			i = -1;
		for (; ++i < ht->hash.size; ) {
			struct ecl_hashtable_entry e = ht->hash.data[i];
			if (e.key != OBJNULL) {
				cl_object ndx = MAKE_FIXNUM(i);
				ECL_RPLACA(env, ndx);
				@(return ndx e.key e.value)
			}
		}
		ECL_RPLACA(env, Cnil);
	}
	@(return Cnil)
}

cl_object
si_hash_table_iterator(cl_object ht)
{
	assert_type_hash_table(ht);
	@(return cl_make_cclosure_va((cl_objectfn)si_hash_table_iterate,
				     cl_list(2, MAKE_FIXNUM(-1), ht),
				     @'si::hash-table-iterator'))
}
cl_object
cl_hash_table_rehash_size(cl_object ht)
{
	assert_type_hash_table(ht);
	@(return ht->hash.rehash_size)
}

cl_object
cl_hash_table_rehash_threshold(cl_object ht)
{
	assert_type_hash_table(ht);
	@(return ht->hash.threshold)
}

cl_object
cl_sxhash(cl_object key)
{
	cl_index output = _hash_equal(3, 0, key);
	const cl_index mask = ((cl_index)1 << (FIXNUM_BITS - 3)) - 1;
	@(return MAKE_FIXNUM(output & mask))
}

@(defun si::hash-eql (&rest args)
	cl_index h;
@
	for (h = 0; narg; narg--) {
		cl_object o = cl_va_arg(args);
		h = _hash_eql(h, o);
	}
	@(return MAKE_FIXNUM(h))
@)

@(defun si::hash-equal (&rest args)
	cl_index h;
@
	for (h = 0; narg; narg--) {
		cl_object o = cl_va_arg(args);
		h = _hash_equal(3, h, o);
	}
	@(return MAKE_FIXNUM(h))
@)

@(defun si::hash-equalp (&rest args)
	cl_index h;
@
	for (h = 0; narg; narg--) {
		cl_object o = cl_va_arg(args);
		h = _hash_equalp(3, h, o);
	}
	@(return MAKE_FIXNUM(h))
@)

cl_object
cl_maphash(cl_object fun, cl_object ht)
{
	cl_index i;

	assert_type_hash_table(ht);
	for (i = 0;  i < ht->hash.size;  i++) {
		struct ecl_hashtable_entry e = ht->hash.data[i];
		if(e.key != OBJNULL)
			funcall(3, fun, e.key, e.value);
	}
	@(return Cnil)
}

cl_object
si_copy_hash_table(cl_object orig)
{
	cl_object hash;
	hash = cl__make_hash_table(cl_hash_table_test(orig),
				   cl_hash_table_size(orig),
				   cl_hash_table_rehash_size(orig),
				   cl_hash_table_rehash_threshold(orig),
				   orig->hash.lockable? Ct : Cnil);
	HASH_TABLE_LOCK(hash);
	memcpy(hash->hash.data, orig->hash.data,
	       orig->hash.size * sizeof(*orig->hash.data));
	hash->hash.entries = orig->hash.entries;
	HASH_TABLE_UNLOCK(hash);
	@(return hash)
}
