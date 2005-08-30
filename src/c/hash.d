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

#include "ecl.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "internal.h"

/********************
 * HASHING ROUTINES *
 ********************/

/*
 * SBCL'S old mashing function. Leads to many collisions.
 */

#if FIXNUM_BITS > 32

#define mash(h,n) ((((h) << 5) | ((h) >> (FIXNUM_BITS - 5))) ^ (n))
#define hash_word(h,x) mash(h,(cl_index)x)

static cl_hashkey
hash_string(cl_hashkey h, const unsigned char *buf, cl_index len)
{
	for (; len; len--) {
		h = mash(h, (*buf++));
	}
	return h;
}

#else

/*
 * SBCL's newest algorithm. Leads to few collisions, is fast, but
 * limited to 32 bits.
 */

#define mix(a,b,c) \
{ \
  a -= b; a -= c; a ^= (c>>13); \
  b -= c; b -= a; b ^= (a<<8); \
  c -= a; c -= b; c ^= (b>>13); \
  a -= b; a -= c; a ^= (c>>12);  \
  b -= c; b -= a; b ^= (a<<16); \
  c -= a; c -= b; c ^= (b>>5); \
  a -= b; a -= c; a ^= (c>>3);  \
  b -= c; b -= a; b ^= (a<<10); \
  c -= a; c -= b; c ^= (b>>15); \
}

static uint32_t
hash_string(uint32_t initval, const unsigned char *k, cl_index len)
{
  uint32_t a = 0, b = 0, c = initval;
  for (; len > 12; ) {
    a += (k[0] +((uint32_t)k[1]<<8) +((uint32_t)k[2]<<16) +((uint32_t)k[3]<<24));
    b += (k[4] +((uint32_t)k[5]<<8) +((uint32_t)k[6]<<16) +((uint32_t)k[7]<<24));
    c += (k[8] +((uint32_t)k[9]<<8) +((uint32_t)k[10]<<16)+((uint32_t)k[11]<<24));
    mix(a,b,c);
    k += 12; len -= 12;
  }

  /*------------------------------------- handle the last 11 bytes */
  c += len;
  switch(len) {
    /* all the case statements fall through */
  case 11: c+=((uint32_t)k[10]<<24);
  case 10: c+=((uint32_t)k[9]<<16);
  case 9 : c+=((uint32_t)k[8]<<8);
    /* the first byte of c is reserved for the length */
  case 8 : b+=((uint32_t)k[7]<<24);
  case 7 : b+=((uint32_t)k[6]<<16);
  case 6 : b+=((uint32_t)k[5]<<8);
  case 5 : b+=k[4];
  case 4 : a+=((uint32_t)k[3]<<24);
  case 3 : a+=((uint32_t)k[2]<<16);
  case 2 : a+=((uint32_t)k[1]<<8);
  case 1 : a+=k[0];
    /* case 0: nothing left to add */
  }
  mix(a,b,c);
  /*-------------------------------------------- report the result */
  return c;
}

static uint32_t hash_word(uint32_t c, uint32_t a)
{
	uint32_t b = 0;
	mix(a, b, c);
	return c;
}

#endif

static void corrupted_hash(cl_object hashtable) /*__attribute__((noreturn))*/;

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
		return hash_string(h, (unsigned char*)x->big.big_limbs,
				   labs(x->big.big_size) * sizeof(mp_limb_t));
	case t_ratio:
		h = _hash_eql(h, x->ratio.num);
		return _hash_eql(h, x->ratio.den);
	case t_shortfloat:
		return hash_string(h, (unsigned char*)&sf(x), sizeof(sf(x)));
	case t_longfloat:
		return hash_string(h, (unsigned char*)&lf(x), sizeof(lf(x)));
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
	case t_cons:
		if (depth++ > 3) {
			return 0;
		}
		h = _hash_equal(depth, h, CAR(x));
		return _hash_equal(depth, h, CDR(x));
	case t_symbol:
		x = x->symbol.name;
	case t_string:
		return hash_string(h, x->string.self, x->string.fillp);
	case t_pathname:
		h = _hash_equal(depth, h, x->pathname.host);
		h = _hash_equal(depth, h, x->pathname.device);
		h = _hash_equal(depth, h, x->pathname.directory);
		h = _hash_equal(depth, h, x->pathname.name);
		h = _hash_equal(depth, h, x->pathname.type);
		return _hash_equal(depth, h, x->pathname.name);
	case t_random:
		return hash_word(h, x->random.value);
	case t_bitvector:
		/* Notice that we may round out some bits. We must do this
		 * because the fill pointer may be set in the middle of a byte.
		 * If so, the extra bits _must_ _not_ take part in the hash,
		 * because otherwise we two bit arrays which are EQUAL might
		 * have different hash keys. */
		return hash_string(h, x->vector.self.ch, x->vector.fillp / 8);
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
	case t_cons:
		if (depth++ > 3) {
			return 0;
		}
		h = _hash_equalp(depth, h, CAR(x));
		return _hash_equalp(depth, h, CDR(x));
	case t_string:
	case t_vector:
	case t_bitvector:
		len = x->vector.fillp;
		goto SCAN;
	case t_array:
		len = x->vector.dim;
SCAN:		if (depth++ >= 3) {
			return 0;
		}
		for (i = 0; i < len; i++) {
			h = _hash_equalp(depth, h, aref(x, i));
		}
		return h;
	case t_fixnum:
		return hash_word(h, fix(x));
	case t_shortfloat:
		/* FIXME! We should be more precise here! */
		return hash_word(h, (cl_index)sf(x));
	case t_longfloat:
		/* FIXME! We should be more precise here! */
		return hash_word(h, (cl_index)lf(x));
	case t_bignum:
		/* FIXME! We should be more precise here! */
	case t_ratio:
		h = _hash_equalp(depth, h, x->ratio.num);
		return _hash_equalp(depth, h, x->ratio.den);
	case t_complex:
		h = _hash_equalp(depth, h, x->complex.real);
		return _hash_equalp(depth, h, x->complex.imag);
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
	case htt_equal:	h = _hash_equal(0, 0, key); break;
	case htt_equalp:h = _hash_equalp(0, 0, key); break;
	case htt_pack:	h = _hash_equal(0, 0, key);
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
		case htt_eql:	b = eql(key, hkey); break;
		case htt_equal: b = equal(key, hkey); break;
		case htt_equalp:b = equalp(key, hkey); break;
		case htt_pack:	b = (ho==hkey) && string_eq(key,e->value->symbol.name);
				break;
		}
		if (b)
			return(&hashtable->hash.data[i]);
	}
	return(&hashtable->hash.data[j]);
}

cl_object
gethash(cl_object key, cl_object hashtable)
{
	cl_object output;

	assert_type_hash_table(hashtable);
	HASH_TABLE_LOCK(hashtable);
	output = ecl_search_hash(key, hashtable)->value;
	HASH_TABLE_UNLOCK(hashtable);
	return output;
}

cl_object
gethash_safe(cl_object key, cl_object hashtable, cl_object def)
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
	case htt_equal:	h = _hash_equal(0, 0, key); break;
	case htt_equalp:h = _hash_equalp(0, 0, key); break;
	case htt_pack:	h = _hash_equal(0, 0, key); break;
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
sethash(cl_object key, cl_object hashtable, cl_object value)
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

	assert_type_hash_table(hashtable);
	old_size = hashtable->hash.size;
	if (FIXNUMP(hashtable->hash.rehash_size))
		new_size = old_size + fix(hashtable->hash.rehash_size);
	else if (type_of(hashtable->hash.rehash_size) == t_shortfloat)
		new_size = (cl_index)(old_size * sf(hashtable->hash.rehash_size));
	else if (type_of(hashtable->hash.rehash_size) == t_longfloat)
		new_size = (cl_index)(old_size * lf(hashtable->hash.rehash_size));
	else
		corrupted_hash(hashtable);
	if (new_size <= old_size)
		new_size = old_size + 1;
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
				key = old->hash.data[i].value->symbol.name;
			add_new_to_hash(key, hashtable, old->hash.data[i].value);
		}
}


@(defun make_hash_table (&key (test @'eql')
			      (size MAKE_FIXNUM(1024))
			      (rehash_size make_shortfloat(1.5))
			      (rehash_threshold make_shortfloat(0.7))
			      (lockable Cnil))
@
	@(return cl__make_hash_table(test, size, rehash_size, rehash_threshold,
				     lockable))
@)

cl_object
cl__make_hash_table(cl_object test, cl_object size, cl_object rehash_size,
		    cl_object rehash_threshold, cl_object lockable)
{
	int htt;
	cl_index hsize;
	cl_object h;
	double factor;
	cl_type t;

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
  	if (!FIXNUMP(size) || FIXNUM_MINUSP(size))
		FEerror("~S is an illegal hash-table size.", 1, size);

	/* Do not allow hashtables of size 0 */
	hsize = fixnnint(size);
	if (hsize < 16)
		hsize = 16;

	t = type_of(rehash_size);
	if ((t != t_fixnum && t != t_shortfloat && t != t_longfloat) ||
	    (number_compare(rehash_size, MAKE_FIXNUM(1)) < 0)) {
		FEerror("~S is an illegal hash-table rehash-size.",
			1, rehash_size);
	}
	factor = -1.0;
	t = type_of(rehash_threshold);
	if (t == t_fixnum || t == t_ratio || t == t_shortfloat || t == t_longfloat) {
		factor = number_to_double(rehash_threshold);
	}
	if (factor < 0.0 || factor > 1.0) {
		FEerror("~S is an illegal hash-table rehash-threshold.",
			1, rehash_threshold);
	}
	h = cl_alloc_object(t_hashtable);
	h->hash.test = htt;
	h->hash.size = hsize;
	h->hash.rehash_size = rehash_size;
	h->hash.threshold = rehash_threshold;
	h->hash.factor = factor;
        h->hash.entries = 0;
	h->hash.data = NULL;	/* for GC sake */
	h->hash.data = (struct ecl_hashtable_entry *)
	cl_alloc(hsize * sizeof(struct ecl_hashtable_entry));
	h->hash.lockable = !Null(lockable);
#ifdef ECL_THREADS
	if (h->hash.lockable)
#if defined(_MSC_VER) || defined(mingw32)
		h->hash.lock = CreateMutex(NULL, FALSE, NULL);
#else
		pthread_mutex_init(&h->hash.lock, NULL);
#endif
#endif
	return cl_clrhash(h);
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
	/* INV: sethash() checks the type of hashtable */
	sethash(key, ht, val);
	@(return val)
}

bool
remhash(cl_object key, cl_object hashtable)
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
	@(return (remhash(key, ht)? Ct : Cnil));
}

cl_object
cl_clrhash(cl_object ht)
{
	cl_index i;

	assert_type_hash_table(ht);
	HASH_TABLE_LOCK(ht);
	for(i = 0; i < ht->hash.size; i++) {
		ht->hash.data[i].key = OBJNULL;
		ht->hash.data[i].value = OBJNULL;
	}
	ht->hash.entries = 0;
	HASH_TABLE_UNLOCK(ht);
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
				@(return (CAR(env) = MAKE_FIXNUM(i))
					 e.key
					 e.value)
			}
		}
		CAR(env) = Cnil;
	}
	@(return Cnil)
}

cl_object
si_hash_table_iterator(cl_object ht)
{
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
	cl_index output = _hash_equal(0, 0, key);
	const cl_index mask = ((cl_index)1 << (FIXNUM_BITS - 3)) - 1;
	@(return MAKE_FIXNUM(output & mask))
}

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
