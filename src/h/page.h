/*
    page.h  -- Page macros.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

/*****************************
 * BOEHM's GARBAGE COLLECTOR *
 *****************************/

#ifdef GBC_BOEHM
#include "gc.h"

struct typemanager {
	const char *tm_name;
	size_t tm_size;
} tm_table[(int)t_end];

#define	tm_of(t)	(&tm_table[(int)(t)])
#endif GBC_BOEHM


/****************************************
 * ECOLISP's ORIGINAL GARBAGE COLLECTOR *
 ****************************************/

#if !defined(GBC_BOEHM)
/* THREADS: If you make it bigger, the bug is less frequent */
#ifdef SYSV
#define	HOLEPAGE	32
#else
#define	HOLEPAGE	128
#endif SYSV
#define	INIT_HOLEPAGE	150
#define	CBMINSIZE	64	/*  contiguous block minimal size  */

#define page(p)		(((intptr_t)(char *)(p) / LISP_PAGESIZE) - (DATA_START/LISP_PAGESIZE))
#define	pagetochar(x)	((char *)(((x) * LISP_PAGESIZE) + DATA_START))
#define round_to_page(x) (((intptr_t)(x) + LISP_PAGESIZE - 1) / LISP_PAGESIZE)
#define	round_up(n)	(((n) + 03) & ~03)
#define	available_pages() ((size_t)(real_maxpage-page(heap_end)-new_holepage-real_maxpage/32))

extern size_t real_maxpage;
extern size_t new_holepage;

/*
	The struct of free lists.
*/
struct freelist {
	HEADER;
	cl_object	f_link;
};

/*
	Type map.

	enum type type_map[MAXPAGE];
*/
extern char type_map[MAXPAGE];

/*
	Storage manager for each type.
*/
struct typemanager {
	enum type tm_type;	/*  type  */
	size_t	tm_size;	/*  element size in bytes  */
	size_t	tm_nppage;	/*  number per page  */
	cl_object tm_free;	/*  free list  */
				/*  Note that it is of type object.  */
	size_t	tm_nfree;	/*  number of free elements  */
	size_t	tm_nused;	/*  number of elements used  */
	size_t	tm_npage;	/*  number of pages  */
	size_t	tm_maxpage;	/*  maximum number of pages  */
	char	*tm_name;	/*  type name  */
	size_t	tm_gccount;	/*  GC count  */
};

/*
	The table of type managers.
*/
extern struct typemanager tm_table[(int)t_end];

#define	tm_of(t)	(&(tm_table[(int)tm_table[(int)(t)].tm_type]))

/*
	Contiguous block header.
*/
struct contblock {			/*  contiguous block header  */
	size_t cb_size;			/*  size in bytes  */
	struct contblock *cb_link;	/*  contiguous block link  */
};

/*
	The pointer to the contiguous blocks.
*/
extern struct contblock *cb_pointer;	/*  contblock pointer  */

/*
	Variables for memory management.
*/
extern size_t ncb;			/*  number of contblocks  */
extern size_t ncbpage;			/*  number of contblock pages  */
extern size_t maxcbpage;		/*  maximum number of contblock pages  */
extern size_t cbgccount;		/*  contblock gc count  */
extern size_t holepage;			/*  hole pages  */

extern char *heap_start;		/*  heap start  */
extern char *heap_end;			/*  heap end  */
extern char *data_end;			/*  core end  */

#endif /* !GBC_BOEHM */

/*******************************
 * SYMBOLS & KEYWORDS DATABASE *
 *******************************/

struct symbol_info {
  cl_object * const loc;
  const char *name;
  enum {
    CL_ORDINARY,
    CL_SPECIAL,
    SI_SPECIAL,
    SI_ORDINARY
  } type;
};

extern const struct symbol_info all_symbols[];

struct keyword_info {
  cl_object * const loc;
  const char *name;
};

extern const struct keyword_info all_keywords[];

