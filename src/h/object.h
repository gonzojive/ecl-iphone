/*
    object.h  -- Data structure definitions.
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

#ifdef __cplusplus
extern "C" {
#endif

/*
	Integer and boolean types (see config.h)
*/

#define	TRUE		1	/*  boolean true value  */
#define	FALSE		0	/*  boolean false value  */

#define	CHAR_CODE_LIMIT	256	/*  ASCII character code limit  */

#ifndef __cplusplus
typedef int bool;
#endif
typedef unsigned char byte;

/*
	Definition of the type of LISP objects.
*/
typedef union lispunion *cl_object;
typedef cl_object cl_return;
typedef cl_object (*cl_objectfn)(int narg, ...);

/*
	OBJect NULL value.
	It should not coincide with any legal object value.
*/
#define	OBJNULL		((cl_object)NULL)

/*
	Definition of each implementation type.
*/

#define IMMEDIATE(obje)		((cl_fixnum)(obje) & 3)
#define IMMEDIATE_TAG		3

/* Immediate fixnums:		*/
#define FIXNUM_TAG		1
#define MAKE_FIXNUM(n)		((cl_object)(((cl_fixnum)(n) << 2) | FIXNUM_TAG))
#define FIXNUM_MINUSP(n)	((cl_fixnum)(n) < 0)
#define FIXNUM_PLUSP(n)		((cl_fixnum)(n) >= (cl_fixnum)MAKE_FIXNUM(0))
#define	fix(obje)		(((cl_fixnum)(obje)) >> 2)
#define FIXNUMP(obje)		(((cl_fixnum)(obje)) & FIXNUM_TAG)

/* Immediate characters:	*/
#define CHARACTER_TAG		2
#define CHARACTERP(obje)	(((cl_fixnum)(obje)) & 2)
#define	CODE_CHAR(c)		((cl_object)(((cl_fixnum)((unsigned char)c) << 2)|CHARACTER_TAG))
#define	CHAR_CODE(obje)		((((cl_fixnum)(obje)) >> 2) & 0xff)

#define NUMBER_TYPE(t)	(t == t_fixnum || (t >= t_bignum && t <= t_complex))
#define REAL_TYPE(t)	(t == t_fixnum || (t >= t_bignum && t < t_complex))
#define ARRAY_TYPE(t)	(t >= t_array && t <= t_bitvector)
#define ARRAYP(x)	((IMMEDIATE(x) == 0) && (x)->d.t >= t_array && (x)->d.t <= t_bitvector)
#define VECTORP(x)	((IMMEDIATE(x) == 0) && (x)->d.t >= t_vector && (x)->d.t <= t_bitvector)

#define HEADER			int8_t t, m, padding[2]
#define HEADER1(field)		int8_t t, m, field, padding
#define HEADER2(field1,field2)	int8_t t, m, field1, field2
#define HEADER3(field1,flag2,flag3) int8_t t, m, field1; unsigned flag2:4, flag3:4

struct shortfloat_struct {
	HEADER;
	float SFVAL;	/*  shortfloat value  */
};
#define	sf(obje)	(obje)->SF.SFVAL

struct longfloat_struct {
	HEADER;
	double LFVAL;	/*  longfloat value  */
};
#define	lf(obje)	(obje)->LF.LFVAL

struct bignum {
	HEADER;
	mpz_t big_num;
};
#define big_dim		big_num->_mp_alloc
#define big_size	big_num->_mp_size
#define big_limbs	big_num->_mp_d

struct ratio {
	HEADER;
	cl_object den;		/*  denominator, must be an integer  */
	cl_object num;		/*  numerator, must be an integer  */
};

struct complex {
	HEADER;
	cl_object real;		/*  real part, must be a number  */
	cl_object imag;		/*  imaginary part, must be a number  */
};

enum stype {			/*  symbol type  */
	stp_ordinary,		/*  ordinary  */
	stp_constant,		/*  constant  */
        stp_special		/*  special  */
};

#define	Cnil			((cl_object)cl_symbols)
#define	Ct			((cl_object)(cl_symbols+1))

struct symbol {
	HEADER3(stype, mflag, isform);
				/*  symbol type and whether it names a macro */
	cl_object dbind;	/*  dynamic binding  */
	cl_object plist;	/*  property list  */
				/*  This field coincides with cons.car  */
	cl_object name;		/*  print name  */
	cl_object gfdef;	/*  global function definition  */
				/*  For a macro,  */
				/*  its expansion function  */
				/*  is to be stored.  */
	cl_object hpack;	/*  home package  */
				/*  Cnil for uninterned symbols  */
};
#define SYM_VAL(sym)	((sym)->symbol.dbind)
#define SYM_FUN(sym)	((sym)->symbol.gfdef)

struct package {
	HEADER1(locked);
	cl_object name;		/*  package name, a string  */
	cl_object nicknames;	/*  nicknames, list of strings  */
	cl_object shadowings;	/*  shadowing symbol list  */
	cl_object uses;		/*  use-list of packages  */
	cl_object usedby;	/*  used-by-list of packages  */
	cl_object internal;	/*  hashtable for internal symbols  */
	cl_object external;	/*  hashtable for external symbols  */
};

/*
	The values returned by intern and find_symbol.
	File_symbol may return 0.
*/
#define	INTERNAL	1
#define	EXTERNAL	2
#define	INHERITED	3

#define LISTP(x)	(x == Cnil || CONSP(x))
#define CONSP(x)	((IMMEDIATE(x) == 0) && ((x)->d.t == t_cons))
#define ATOM(x)		((IMMEDIATE(x) != 0) || ((x)->d.t != t_cons))
#define SYMBOLP(x)	((IMMEDIATE(x) == 0) && ((x)->d.t == t_symbol))
struct cons {
	HEADER;
	cl_object cdr;		/*  cdr  */
	cl_object car;		/*  car  */
};

enum httest {			/*  hash table key test function  */
	htt_eq,			/*  eq  */
	htt_eql,		/*  eql  */
	htt_equal,		/*  equal  */
	htt_pack		/*  symbol hash  */
};

struct hashtable_entry {	/*  hash table entry  */
	cl_object key;		/*  key  */
	cl_object value;	/*  value  */
};

struct hashtable {		/*  hash table header  */
	HEADER1(test);
	struct hashtable_entry *data; /*  pointer to the hash table  */
	cl_object rehash_size;	/*  rehash size  */
	cl_object threshold;	/*  rehash threshold  */
	cl_index entries;	/*  number of entries  */
	cl_index size;		/*  hash table size  */
};

typedef enum {			/*  array element type  */
	aet_object,		/*  t                */
	aet_ch,			/*  string-char      */
	aet_bit,		/*  bit              */
	aet_fix,		/*  fixnum           */
	aet_sf,			/*  short-float      */
	aet_lf,			/*  long-float       */
	aet_b8,			/*  byte8	     */
	aet_i8,			/*  integer8	     */
#if 0
	aet_short,		/*  signed short     */
	aet_ushort		/*  unsigned short   */
#endif
} cl_elttype;

union array_data {
	cl_object *t;
        unsigned char *ch;
	uint8_t *b8;
	int8_t *i8;
	float *sf;
	double *lf;
	cl_fixnum *fix;
	byte *bit;
};

struct array {			/*  array header  */
				/*  adjustable flag  */
				/*  has-fill-pointer flag  */
	HEADER2(adjustable,rank);
	cl_object displaced;	/*  displaced  */
	cl_index dim;		/*  dimension  */
	cl_index *dims;		/*  table of dimensions  */
	union array_data self;	/*  pointer to the array  */
	byte	elttype;	/*  element type  */
	byte	offset;		/*  bitvector offset  */
};

struct vector {			/*  vector header  */
				/*  adjustable flag  */
				/*  has-fill-pointer flag  */
	HEADER2(adjustable,hasfillp);
	cl_object displaced;	/*  displaced  */
	cl_index dim;		/*  dimension  */
	cl_index fillp;		/*  fill pointer  */
				/*  For simple vectors,  */
				/*  v_fillp is equal to v_dim.  */
	union array_data self;	/*  pointer to the vector  */
	byte	elttype;	/*  element type  */
	byte	offset;
};

struct string {			/*  string header  */
				/*  adjustable flag  */
				/*  has-fill-pointer flag  */
	HEADER2(adjustable,hasfillp);
	cl_object displaced;	/*  displaced  */
	cl_index dim;       	/*  dimension  */
				/*  string length  */
	cl_index fillp;		/*  fill pointer  */
				/*  For simple strings,  */
				/*  st_fillp is equal to st_dim-1.  */
	unsigned char *self;	/*  pointer to the string  */
};

#ifdef CLOS
#define T_STRUCTURE	t_instance
#define STYPE(x)	CLASS_OF(x)
#define SLOTS(x)	(x)->instance.slots
#define SLENGTH(x)	(x)->instance.length
#define SLOT(x,i)	(x)->instance.slots[i]
#define SNAME(x)	CLASS_NAME(CLASS_OF(x))
#else
struct structure {		/*  structure header  */
	HEADER;
	cl_object name;		/*  structure name  */
	cl_object *self;	/*  structure self  */
	cl_fixnum length;	/*  structure length  */
};

#define T_STRUCTURE	t_structure
#define STYPE(x)	x->str.name
#define SLOTS(x)	(x)->str.self
#define SLENGTH(x)	(x)->str.length
#define SLOT(x,i)	(x)->str.self[i]
#define SNAME(x)	x->str.name
#endif

enum smmode {			/*  stream mode  */
	smm_closed,		/*  closed  */
	smm_input,		/*  input  */
	smm_output,		/*  output  */
	smm_io,			/*  input-output  */
	smm_probe,		/*  probe  */
	smm_synonym,		/*  synonym  */
	smm_broadcast,		/*  broadcast  */
	smm_concatenated,	/*  concatenated  */
	smm_two_way,		/*  two way  */
	smm_echo,		/*  echo  */
	smm_string_input,	/*  string input  */
	smm_string_output	/*  string output  */
};

struct stream {
	HEADER1(mode);		/*  stream mode of enum smmode  */
	FILE	*file;		/*  file pointer  */
	cl_object object0;	/*  some object  */
	cl_object object1;	/*  some object */
	cl_fixnum int0;		/*  some int  */
	cl_fixnum int1;		/*  some int  */
#if !defined(GBC_BOEHM)
	char	*buffer;	/*  file buffer  */
#endif
};

struct random {
	HEADER;
	unsigned value;	/*  random state value  */
};

enum chattrib {			/*  character attribute  */
	cat_whitespace,		/*  whitespace  */
	cat_terminating,	/*  terminating macro  */
	cat_non_terminating,	/*  non-terminating macro  */
	cat_single_escape,	/*  single-escape  */
	cat_multiple_escape,	/*  multiple-escape  */
	cat_constituent		/*  constituent  */
};

struct readtable_entry {		/*  read table entry  */
	enum chattrib syntax_type;	/*  character attribute  */
	cl_object macro;		/*  macro function  */
	cl_object *dispatch_table;	/*  pointer to the  */
					/*  dispatch table  */
					/*  NULL for  */
					/*  non-dispatching  */
					/*  macro character, or  */
					/*  non-macro character  */
};

struct readtable {			/*  read table  */
	HEADER;
	struct readtable_entry	*table;	/*  read table itself  */
};

struct pathname {
	HEADER1(logical);	/*  logical pathname?  */
	cl_object host;		/*  host  */
	cl_object device;	/*  device  */
	cl_object directory;	/*  directory  */
	cl_object name;		/*  name  */
	cl_object type;		/*  type  */
	cl_object version;	/*  version  */
};

struct codeblock {
	HEADER;
	void	*handle;		/*  handle returned by dlopen  */
	void	*entry;			/*  entry point  */
 	cl_object *data;		/*  data vector  */
	int	data_size;
	const char *data_text;		/*  string with objects to be defined  */
	int	data_text_size;
	cl_object next;			/*  next codeblock within same library */
#ifdef PDE
	int	source_pathname;
#endif
	cl_object name;
	cl_object links;		/*  list of symbols with linking calls  */
};

struct bytecodes {
	HEADER;
	cl_object name;		/*  function name  */
	cl_object lex;		/*  lexical environment  */
	cl_object specials;	/*  list of special variables  */
	cl_object definition;	/*  function definition in list form  */
	cl_index code_size;	/*  number of bytecodes  */
	cl_index data_size;	/*  number of constants  */
	char *code;		/*  the intermediate language  */
	cl_object *data;	/*  non-inmediate constants used in the code  */
};

struct cfun {			/*  compiled function header  */
	HEADER1(narg);
	cl_object name;		/*  compiled function name  */
	cl_objectfn entry;	/*  entry address  */
	cl_object block;	/*  descriptor of C code block for GC  */
};

struct cclosure {		/*  compiled closure header  */
	HEADER;
	cl_object env;		/*  environment  */
	cl_objectfn entry;	/*  entry address  */
	cl_object block;	/*  descriptor of C code block for GC  */
};

#ifdef ECL_FFI
struct foreign {		/*  user defined datatype  */
	HEADER;
	cl_object tag;		/*  a tag identifying the type  */
	cl_index size;		/*  the amount of memory allocated  */
	char *data;		/*  the data itself  */
};
#endif

/*
	dummy type
*/
struct dummy {
	HEADER;
};

#ifdef THREADS
struct cont {
				/* already resumed */
				/* timed out */
	HEADER(resumed, timed_out);
	cl_object thread;	/* its thread */
};

struct thread {
	HEADER;
	struct pd *self;	/* the thread itself (really a *pd) */
	cl_index size;		/* its size */
	cl_object fun;		/* initial function */
	cl_object cont;		/* its cont */
};
#endif /* THREADS */


#ifdef CLOS
#define CLASS_OF(x)		(x)->instance.clas
#define CLASS_NAME(x)		(x)->instance.slots[0]
#define CLASS_SUPERIORS(x)	(x)->instance.slots[1]
#define CLASS_INFERIORS(x)	(x)->instance.slots[2]
#define CLASS_SLOTS(x)		(x)->instance.slots[3]
#define CLASS_CPL(x)		(x)->instance.slots[4]

struct instance {		/*  instance header  */
	HEADER1(isgf);
	cl_index length;	/*  instance length  */
	cl_object clas;		/*  instance class  */
	cl_object *slots;	/*  instance slots  */
};
#endif /* CLOS */

/*
	Definition of lispunion.
*/
union lispunion {
	struct bignum	big;	/*  bignum  */
	struct ratio	ratio;	/*  ratio  */
	struct shortfloat_struct SF; /*  short floating-point number  */
	struct longfloat_struct LF; /*  long floating-point number  */
	struct complex	complex;/*  complex number  */
	struct symbol	symbol;	/*  symbol  */
	struct package	pack;	/*  package  */
	struct cons	cons;	/*  cons  */
	struct hashtable hash;	/*  hash table  */
	struct array	array;	/*  array  */
	struct vector	vector;	/*  vector  */
	struct string	string;	/*  string  */
	struct stream	stream;	/*  stream  */
	struct random	random;	/*  random-states  */
	struct readtable readtable; /*  read table  */
	struct pathname	pathname; /*  path name  */
	struct bytecodes bytecodes; /*  bytecompiled closure */
	struct cfun	cfun;	/*  compiled function  */
	struct cclosure	cclosure; /*  compiled closure  */

	struct dummy	d;	/*  dummy  */
#ifdef CLOS
	struct instance instance; /*  clos instance */
#else
	struct structure str;	/*  structure  */
#endif /* CLOS */
#ifdef THREADS
	struct cont     cont;	/*  continuation  */
	struct thread   thread;	/*  thread  */
#endif /* THREADS */
	struct codeblock cblock; /*  codeblock  */
#ifdef ECL_FFI
	struct foreign	foreign; /* user defined data type */
#endif
};

/*
	Implementation types.
*/
typedef enum {
	t_cons = 0,
	t_start = 0,
	t_fixnum,		/* 1 immediate fixnum */
	t_character,		/* 2 immediate character */
	t_bignum = 4,		/* 4 */
	t_ratio,		/* 5 */
	t_shortfloat,		/* 6 */
	t_longfloat,		/* 7 */
	t_complex,		/* 8 */
	t_symbol,		/* 9 */
	t_package,		/* a */
	t_hashtable,		/* b */
	t_array,		/* c */
	t_vector,		/* d */
	t_string,		/* e */
	t_bitvector,		/* f */
	t_stream,		/* 10 */
	t_random,		/* 11 */
	t_readtable,		/* 12 */
	t_pathname,		/* 13 */
	t_bytecodes,		/* 14 */
	t_cfun,			/* 15 */
	t_cclosure,		/* 16 */
#ifdef CLOS
	t_instance,		/* 17 */
#else
	t_structure,		/* 17 */
#endif /* CLOS */
#ifdef THREADS
	t_cont,			/* 19 */
	t_thread,		/* 20 */
#endif
	t_codeblock,		/* 21 */
#ifdef ECL_FFI
	t_foreign,		/* 22 */
#endif
	t_end,
	t_other,
	t_contiguous,		/*  contiguous block  */
	FREE = 127		/*  free object  */
} cl_type;


/*
	Type_of.
*/
#define	type_of(obje)	((cl_type)(IMMEDIATE(obje) ? IMMEDIATE(obje) : (((cl_object)(obje)) ->d.t)))

#define	ENDP(x)	(type_of(x) == t_cons ? \
		 FALSE : x == Cnil ? TRUE : \
		 (FEtype_error_list(x), FALSE))

/*
	This is used to retrieve optional arguments
*/
typedef struct {
  va_list args;
  cl_index sp;
  int narg;
} cl_va_list[1];

#ifdef __cplusplus
}
#endif
