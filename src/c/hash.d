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

/*******************
 * CRC-32 ROUTINES *
 *******************/

/*
 * Lookup table for CRC-32 codes
 */

static const cl_hashkey crc_table[256] = {
  0x00000000L, 0x77073096L, 0xee0e612cL, 0x990951baL, 0x076dc419L,
  0x706af48fL, 0xe963a535L, 0x9e6495a3L, 0x0edb8832L, 0x79dcb8a4L,
  0xe0d5e91eL, 0x97d2d988L, 0x09b64c2bL, 0x7eb17cbdL, 0xe7b82d07L,
  0x90bf1d91L, 0x1db71064L, 0x6ab020f2L, 0xf3b97148L, 0x84be41deL,
  0x1adad47dL, 0x6ddde4ebL, 0xf4d4b551L, 0x83d385c7L, 0x136c9856L,
  0x646ba8c0L, 0xfd62f97aL, 0x8a65c9ecL, 0x14015c4fL, 0x63066cd9L,
  0xfa0f3d63L, 0x8d080df5L, 0x3b6e20c8L, 0x4c69105eL, 0xd56041e4L,
  0xa2677172L, 0x3c03e4d1L, 0x4b04d447L, 0xd20d85fdL, 0xa50ab56bL,
  0x35b5a8faL, 0x42b2986cL, 0xdbbbc9d6L, 0xacbcf940L, 0x32d86ce3L,
  0x45df5c75L, 0xdcd60dcfL, 0xabd13d59L, 0x26d930acL, 0x51de003aL,
  0xc8d75180L, 0xbfd06116L, 0x21b4f4b5L, 0x56b3c423L, 0xcfba9599L,
  0xb8bda50fL, 0x2802b89eL, 0x5f058808L, 0xc60cd9b2L, 0xb10be924L,
  0x2f6f7c87L, 0x58684c11L, 0xc1611dabL, 0xb6662d3dL, 0x76dc4190L,
  0x01db7106L, 0x98d220bcL, 0xefd5102aL, 0x71b18589L, 0x06b6b51fL,
  0x9fbfe4a5L, 0xe8b8d433L, 0x7807c9a2L, 0x0f00f934L, 0x9609a88eL,
  0xe10e9818L, 0x7f6a0dbbL, 0x086d3d2dL, 0x91646c97L, 0xe6635c01L,
  0x6b6b51f4L, 0x1c6c6162L, 0x856530d8L, 0xf262004eL, 0x6c0695edL,
  0x1b01a57bL, 0x8208f4c1L, 0xf50fc457L, 0x65b0d9c6L, 0x12b7e950L,
  0x8bbeb8eaL, 0xfcb9887cL, 0x62dd1ddfL, 0x15da2d49L, 0x8cd37cf3L,
  0xfbd44c65L, 0x4db26158L, 0x3ab551ceL, 0xa3bc0074L, 0xd4bb30e2L,
  0x4adfa541L, 0x3dd895d7L, 0xa4d1c46dL, 0xd3d6f4fbL, 0x4369e96aL,
  0x346ed9fcL, 0xad678846L, 0xda60b8d0L, 0x44042d73L, 0x33031de5L,
  0xaa0a4c5fL, 0xdd0d7cc9L, 0x5005713cL, 0x270241aaL, 0xbe0b1010L,
  0xc90c2086L, 0x5768b525L, 0x206f85b3L, 0xb966d409L, 0xce61e49fL,
  0x5edef90eL, 0x29d9c998L, 0xb0d09822L, 0xc7d7a8b4L, 0x59b33d17L,
  0x2eb40d81L, 0xb7bd5c3bL, 0xc0ba6cadL, 0xedb88320L, 0x9abfb3b6L,
  0x03b6e20cL, 0x74b1d29aL, 0xead54739L, 0x9dd277afL, 0x04db2615L,
  0x73dc1683L, 0xe3630b12L, 0x94643b84L, 0x0d6d6a3eL, 0x7a6a5aa8L,
  0xe40ecf0bL, 0x9309ff9dL, 0x0a00ae27L, 0x7d079eb1L, 0xf00f9344L,
  0x8708a3d2L, 0x1e01f268L, 0x6906c2feL, 0xf762575dL, 0x806567cbL,
  0x196c3671L, 0x6e6b06e7L, 0xfed41b76L, 0x89d32be0L, 0x10da7a5aL,
  0x67dd4accL, 0xf9b9df6fL, 0x8ebeeff9L, 0x17b7be43L, 0x60b08ed5L,
  0xd6d6a3e8L, 0xa1d1937eL, 0x38d8c2c4L, 0x4fdff252L, 0xd1bb67f1L,
  0xa6bc5767L, 0x3fb506ddL, 0x48b2364bL, 0xd80d2bdaL, 0xaf0a1b4cL,
  0x36034af6L, 0x41047a60L, 0xdf60efc3L, 0xa867df55L, 0x316e8eefL,
  0x4669be79L, 0xcb61b38cL, 0xbc66831aL, 0x256fd2a0L, 0x5268e236L,
  0xcc0c7795L, 0xbb0b4703L, 0x220216b9L, 0x5505262fL, 0xc5ba3bbeL,
  0xb2bd0b28L, 0x2bb45a92L, 0x5cb36a04L, 0xc2d7ffa7L, 0xb5d0cf31L,
  0x2cd99e8bL, 0x5bdeae1dL, 0x9b64c2b0L, 0xec63f226L, 0x756aa39cL,
  0x026d930aL, 0x9c0906a9L, 0xeb0e363fL, 0x72076785L, 0x05005713L,
  0x95bf4a82L, 0xe2b87a14L, 0x7bb12baeL, 0x0cb61b38L, 0x92d28e9bL,
  0xe5d5be0dL, 0x7cdcefb7L, 0x0bdbdf21L, 0x86d3d2d4L, 0xf1d4e242L,
  0x68ddb3f8L, 0x1fda836eL, 0x81be16cdL, 0xf6b9265bL, 0x6fb077e1L,
  0x18b74777L, 0x88085ae6L, 0xff0f6a70L, 0x66063bcaL, 0x11010b5cL,
  0x8f659effL, 0xf862ae69L, 0x616bffd3L, 0x166ccf45L, 0xa00ae278L,
  0xd70dd2eeL, 0x4e048354L, 0x3903b3c2L, 0xa7672661L, 0xd06016f7L,
  0x4969474dL, 0x3e6e77dbL, 0xaed16a4aL, 0xd9d65adcL, 0x40df0b66L,
  0x37d83bf0L, 0xa9bcae53L, 0xdebb9ec5L, 0x47b2cf7fL, 0x30b5ffe9L,
  0xbdbdf21cL, 0xcabac28aL, 0x53b39330L, 0x24b4a3a6L, 0xbad03605L,
  0xcdd70693L, 0x54de5729L, 0x23d967bfL, 0xb3667a2eL, 0xc4614ab8L,
  0x5d681b02L, 0x2a6f2b94L, 0xb40bbe37L, 0xc30c8ea1L, 0x5a05df1bL,
  0x2d02ef8dL
};

/*
 * CRC-32 Updater
 */

#define DO1ch(crc,c) crc = crc_table[((int)crc ^ (c)) & 0xff] ^ (crc >> 8)
#define DO1(crc,buf) crc = crc_table[((int)crc ^ (*buf++)) & 0xff] ^ (crc >> 8)
#define DO2(crc,buf) DO1(crc,buf); DO1(crc,buf);
#define DO4(crc,buf) DO2(crc,buf); DO2(crc,buf);
#define DO8(crc,buf) DO4(crc,buf); DO4(crc,buf);

#if 0
static cl_hashkey
update_crc32(cl_hashkey crc, const char *buf, cl_index len)
{
    while (len >= 8) {
      DO8(crc,buf);
      len -= 8;
    }
    if (len) do {
      DO1(crc,buf);
    } while (--len);
    return crc;
}
#endif

static void corrupted_hash(cl_object hashtable) __attribute__((noreturn));

static void
corrupted_hash(cl_object hashtable)
{
  FEerror("internal error, corrupted hashtable ~S", 1, hashtable);
}

cl_hashkey
hash_eql(cl_object x)
{
	register char *buffer;
	register cl_index len;
	register cl_hashkey h = 0;
 BEGIN:
	switch (type_of(x)) {
	case t_bignum:
		buffer = (char*)x->big.big_limbs;
		len = abs(x->big.big_size) * sizeof(mp_limb_t);
		break;
	case t_ratio:
		h = hash_eql(x->ratio.num);
		x = x->ratio.den;
		goto BEGIN;
	case t_shortfloat:
		buffer = (char*)&sf(x);
		len = sizeof(sf(x));
		break;
	case t_longfloat:
		buffer = (char*)&lf(x);
		len = sizeof(lf(x));
		break;
	case t_complex:
		h = hash_eql(x->complex.real);
		x = x->complex.imag;
		goto BEGIN;
	case t_character:
		return CHAR_CODE(x);
	default:
		return (cl_hashkey)x >> 2;
	}
	while (len >= 4) {
		DO4(h, buffer);
		len -= 4;
	}
	while (len--)
		DO1(h, buffer);
	return h;
}

static cl_hashkey
_hash_equal(cl_hashkey h, int depth, cl_object x)
{
	char *buffer;
	cl_index len;

	cs_check(x);
BEGIN:
	if (depth++ > 3) return h;
	switch (type_of(x)) {
	case t_cons:
		h = _hash_equal(h, depth, CAR(x));
		x = CDR(x);
		goto BEGIN;
	case t_symbol:
		x = x->symbol.name;
	case t_string:
		buffer = x->string.self;
		len = x->string.fillp;
		break;
	case t_pathname:
		h = _hash_equal(h, depth, x->pathname.host);
		h = _hash_equal(h, depth, x->pathname.device);
		h = _hash_equal(h, depth, x->pathname.directory);
		h = _hash_equal(h, depth, x->pathname.name);
		h = _hash_equal(h, depth, x->pathname.type);
		x = x->pathname.name;
		goto BEGIN;
#if 0 /* !ANSI */
#ifdef CLOS
	case t_instance:
		h += _hash_equal(CLASS_NAME(x), depth);
		for (i = 0;  i < x->instance.length;  i++)
			h += _hash_equal(x->instance.slots[i], depth);
		return(h);
#else
	case t_structure:
		h += _hash_equal(x->str.name, depth);
		for (i = 0;  i < x->str.length;  i++)
			h += _hash_equal(x->str.self[i], depth);
		return(h);
#endif /* CLOS */
#endif /* !ANSI */
	case t_random:
		return h ^ x->random.value;
        case t_package:		/* These two should actually */
	case t_bitvector:	/* have visible changes under equal */
	default:
		return h ^ hash_eql(x);
	}
	while (len >= 4) {
		DO4(h, buffer);
		len -= 4;
	}
	while (len--)
		DO1(h, buffer);
	return h;
}

cl_hashkey
hash_equal(cl_object key)
{
	return _hash_equal(~(cl_hashkey)0, 0, key);
}

static struct hashtable_entry *
search_hash(cl_object key, cl_object hashtable)
{
	cl_hashkey h;
	cl_index hsize, i, j, k;
	struct hashtable_entry *e;
	cl_object hkey;
	int htest;
	bool b;

	assert_type_hash_table(hashtable);
	htest = hashtable->hash.test;
	hsize = hashtable->hash.size;
	j = hsize;
	switch (htest) {
	case htt_eq:	h = (cl_hashkey)key >> 2; break;
	case htt_eql:	h = hash_eql(key); break;
	case htt_equal:	h = _hash_equal(~(cl_hashkey)0, 0, key); break;
	case htt_pack:	h = _hash_equal(~(cl_hashkey)0, 0, key); break;
	default:	corrupted_hash(hashtable);
	}
	i = h % hsize;
	h = h & 0xFFFF;
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
		case htt_pack:	b = (h==fix(hkey)) && string_eq(key,e->value->symbol.name);
				break;
		default:	corrupted_hash(hashtable);
		}
		if (b)
			return(&hashtable->hash.data[i]);
	}
	return(&hashtable->hash.data[j]);
}

cl_object
gethash(cl_object key, cl_object hashtable)
{
	/* INV: search_hash() checks the type of hashtable */
	return search_hash(key, hashtable)->value;
}

cl_object
gethash_safe(cl_object key, cl_object hashtable, cl_object def)
{
	struct hashtable_entry *e;

	/* INV: search_hash() checks the type of hashtable */
	e = search_hash(key, hashtable);
	if (e->key == OBJNULL)
		return def;
	else
		return e->value;
}

static void
add_new_to_hash(cl_object key, cl_object hashtable, cl_object value)
{
	int htest;
	cl_hashkey h;
	cl_index i, hsize;
	struct hashtable_entry *e;

	/* INV: hashtable has the right type */
	htest = hashtable->hash.test;
	hsize = hashtable->hash.size;
	switch (htest) {
	case htt_eq:	h = (cl_hashkey)key / 4; break;
	case htt_eql:	h = hash_eql(key); break;
	case htt_equal: h = _hash_equal(~(cl_hashkey)0, 0, key); break;
	case htt_pack:	h = _hash_equal(~(cl_hashkey)0, 0, key); break;
	default:	corrupted_hash(hashtable);
	}
	e = hashtable->hash.data;
	for (i = h % hsize; ; i = (i + 1) % hsize)
		if (e[i].key == OBJNULL) {
			hashtable->hash.entries++;
			if (htest == htt_pack)
				e[i].key = MAKE_FIXNUM(h & 0xFFFF);
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
	bool over;
	struct hashtable_entry *e;

	/* INV: search_hash() checks the type of hashtable */
	e = search_hash(key, hashtable);
	if (e->key != OBJNULL) {
		e->value = value;
		return;
	}
	i = hashtable->hash.entries + 1;
	if (i >= hashtable->hash.size)
		over = TRUE;
	else if (FIXNUMP(hashtable->hash.threshold))
		over = i >= (cl_index)fix(hashtable->hash.threshold);
	else if (type_of(hashtable->hash.threshold) == t_shortfloat)
		over = i >= hashtable->hash.size * sf(hashtable->hash.threshold);
	else if (type_of(hashtable->hash.threshold) == t_longfloat)
		over = i >= hashtable->hash.size * lf(hashtable->hash.threshold);
	else
		corrupted_hash(hashtable);
	if (over)
		extend_hashtable(hashtable);
	add_new_to_hash(key, hashtable, value);
}

void
extend_hashtable(cl_object hashtable)
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
	hashtable->hash.size = new_size;
	if (FIXNUMP(hashtable->hash.threshold))
		hashtable->hash.threshold =
		MAKE_FIXNUM(fix(hashtable->hash.threshold) +
			    (new_size - old->hash.size));
	hashtable->hash.data = (struct hashtable_entry *)
	  cl_alloc(new_size * sizeof(struct hashtable_entry));
	for (i = 0;  i < new_size;  i++) {
		hashtable->hash.data[i].key = OBJNULL;
		hashtable->hash.data[i].value = OBJNULL;
	}
	for (i = 0;  i < old_size;  i++)
		if ((key = old->hash.data[i].key) != OBJNULL) {
			if (hashtable->hash.test == htt_pack)
				key = old->hash.data[i].value;
			add_new_to_hash(key, hashtable, old->hash.data[i].value);
		}
}


@(defun make_hash_table (&key (test @'eql')
			      (size MAKE_FIXNUM(1024))
			      (rehash_size make_shortfloat(1.5))
			      (rehash_threshold make_shortfloat(0.7)))
@
	@(return cl__make_hash_table(test, size, rehash_size, rehash_threshold))
@)

cl_object
cl__make_hash_table(cl_object test, cl_object size, cl_object rehash_size,
		    cl_object rehash_threshold)
{
	enum httest htt;
	cl_index hsize;
	cl_object h;

	if (test == @'eq' || test == SYM_FUN(@'eq'))
		htt = htt_eq;
	else if (test == @'eql' || test == SYM_FUN(@'eql'))
		htt = htt_eql;
	else if (test == @'equal' || test == SYM_FUN(@'equal'))
		htt = htt_equal;
	else
		FEerror("~S is an illegal hash-table test function.",
			1, test);
  	if (!FIXNUMP(size) || FIXNUM_MINUSP(size))
		FEerror("~S is an illegal hash-table size.", 1, size);
	hsize = fix(size);
	if ((FIXNUMP(rehash_size) && 0 < fix(rehash_size)) ||
	    (type_of(rehash_size) == t_shortfloat && 1.0 < sf(rehash_size)) ||
	    (type_of(rehash_size) == t_longfloat && 1.0 < lf(rehash_size)))
		;
	else
		FEerror("~S is an illegal hash-table rehash-size.",
			1, rehash_size);
	if ((FIXNUMP(rehash_threshold) &&
	     0 < fix(rehash_threshold) && fix(rehash_threshold) < fix(size)) ||
	    (type_of(rehash_threshold) == t_shortfloat &&
	     0.0 < sf(rehash_threshold) && sf(rehash_threshold) < 1.0) ||
	    (type_of(rehash_threshold) == t_longfloat &&
	     0.0 < lf(rehash_threshold) && lf(rehash_threshold) < 1.0))
		;
	else
		FEerror("~S is an illegal hash-table rehash-threshold.",
			1, rehash_threshold);
	h = cl_alloc_object(t_hashtable);
	h->hash.test = htt;
	h->hash.size = hsize;
	h->hash.rehash_size = rehash_size;
	h->hash.threshold = rehash_threshold;
        h->hash.entries = 0;
	h->hash.data = NULL;	/* for GC sake */
	h->hash.data = (struct hashtable_entry *)
	cl_alloc(hsize * sizeof(struct hashtable_entry));
	return cl_clrhash(h);
}

cl_object
cl_hash_table_p(cl_object ht)
{
	@(return ((type_of(ht) == t_hashtable) ? Ct : Cnil))
}

@(defun gethash (key ht &optional (no_value Cnil))
	struct hashtable_entry *e;
@
	/* INV: search_hash() checks the type of hashtable */
	e = search_hash(key, ht);
	if (e->key != OBJNULL)
		@(return e->value Ct)
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
	struct hashtable_entry *e;

	/* INV: search_hash() checks the type of hashtable */
	e = search_hash(key, hashtable);
	if (e->key != OBJNULL) {
		e->key = OBJNULL;
		e->value = Cnil;
		hashtable->hash.entries--;
		return TRUE;
	}
	return FALSE;
}

cl_object
cl_remhash(cl_object key, cl_object ht)
{
	/* INV: search_hash() checks the type of hashtable */
	@(return (remhash(key, ht)? Ct : Cnil));
}

cl_object
cl_clrhash(cl_object ht)
{
	cl_index i;

	assert_type_hash_table(ht);
	for(i = 0; i < ht->hash.size; i++) {
		ht->hash.data[i].key = OBJNULL;
		ht->hash.data[i].value = OBJNULL;
	}
	ht->hash.entries = 0;
	@(return ht)
}

cl_object
cl_hash_table_count(cl_object ht)
{
	assert_type_hash_table(ht);
	@(return (MAKE_FIXNUM(ht->hash.entries)))
}

static cl_object
si_hash_table_iterate(int narg, cl_object env)
{
	cl_object index = CAR(env);
	cl_object ht = CADR(env);
	cl_fixnum i;
	if (!Null(index)) {
		i = fix(index);
		if (i < 0)
			i = -1;
		for (; ++i < ht->hash.size; )
			if (ht->hash.data[i].key != OBJNULL) {
				@(return (CAR(env) = MAKE_FIXNUM(i))
					 ht->hash.data[i].key
					 ht->hash.data[i].value)
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
	@(return (MAKE_FIXNUM(_hash_equal(~(cl_hashkey)0, 0, key) & 0x7fffffff)))
}

cl_object
cl_maphash(cl_object fun, cl_object ht)
{
	cl_index i;

	assert_type_hash_table(ht);
	for (i = 0;  i < ht->hash.size;  i++) {
		if(ht->hash.data[i].key != OBJNULL)
			funcall(3, fun,
				  ht->hash.data[i].key,
				  ht->hash.data[i].value);
	}
	@(return Cnil)
}
