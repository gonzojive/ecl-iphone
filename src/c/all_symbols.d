#include <ctype.h>
#include "ecl.h"
#include "internal.h"
#include <limits.h>

#define CL_PACKAGE 0
#define SI_PACKAGE 4
#define EXT_PACKAGE SI_PACKAGE
#define KEYWORD_PACKAGE 8
#define MP_PACKAGE 12
#define ORDINARY_SYMBOL 0
#define CONSTANT_SYMBOL 1
#define SPECIAL_SYMBOL 2
#define FORM_SYMBOL 3

#define CL_ORDINARY	CL_PACKAGE | ORDINARY_SYMBOL
#define CL_SPECIAL	CL_PACKAGE | SPECIAL_SYMBOL
#define CL_CONSTANT	CL_PACKAGE | CONSTANT_SYMBOL
#define CL_FORM		CL_PACKAGE | ORDINARY_SYMBOL | FORM_SYMBOL
#define SI_ORDINARY	SI_PACKAGE | ORDINARY_SYMBOL
#define SI_SPECIAL	SI_PACKAGE | SPECIAL_SYMBOL
#define SI_CONSTANT	SI_PACKAGE | CONSTANT_SYMBOL
#define EXT_ORDINARY	EXT_PACKAGE | ORDINARY_SYMBOL
#define EXT_SPECIAL	EXT_PACKAGE | SPECIAL_SYMBOL
#define EXT_CONSTANT	EXT_PACKAGE | CONSTANT_SYMBOL
#define EXT_FORM	EXT_PACKAGE | ORDINARY_SYMBOL | FORM_SYMBOL
#define MP_ORDINARY	MP_PACKAGE | ORDINARY_SYMBOL
#define MP_SPECIAL	MP_PACKAGE | SPECIAL_SYMBOL
#define MP_CONSTANT	MP_PACKAGE | CONSTANT_SYMBOL
#define KEYWORD		KEYWORD_PACKAGE | CONSTANT_SYMBOL

#include "symbols_list.h"

cl_index cl_num_symbols_in_core = 0;

static char *
mangle_name(cl_object output, char *source, int l)
{
	char c;

	while (l--) {
		c = *(source++);
		if (isalpha(c))
			c = tolower(c);
		else if (isdigit(c))
			;
		else if (c == '-' || c == '_') {
			c = '_';
		} else if (c == '&') {
			c = 'A';
		} else if (c == '*') {
			c = 'X';
		} else if (c == '+') {
			c = 'P';
		} else if (c == '<') {
			c = 'L';
		} else if (c == '>') {
			c = 'G';
		} else if (c == '=') {
			c = 'E';
		} else if (c == '/') {
			c = 'N';
		} else if (c == ':') {
			c = 'X';
		} else {
			return NULL;
		}
		output->string.self[output->string.fillp++] = c;
	}
	return &output->string.self[output->string.fillp];
}

@(defun si::mangle-name (symbol &optional as_function)
	cl_index l;
	char c, *source, *dest;
	cl_object output;
	cl_object package;
	cl_object found = Cnil;
	cl_object maxarg = MAKE_FIXNUM(-1);
	bool is_symbol;
@
	assert_type_symbol(symbol);
	is_symbol = Null(as_function);
	if (is_symbol) {
		cl_fixnum p;

		if (symbol == Cnil)
			@(return Ct make_constant_string("Cnil"))
		else if (symbol == Ct)
			@(return Ct make_constant_string("Ct"))
		p  = (cl_symbol_initializer*)symbol - cl_symbols;
		if (p >= 0 && p <= cl_num_symbols_in_core) {
			found = Ct;
			output = cl_format(3, Cnil,
					  make_constant_string("((cl_object)(cl_symbols+~A))"),
					  MAKE_FIXNUM(p));
			@(return found output maxarg)
		}
	} else {
		cl_object fun;
		fun = symbol->symbol.gfdef;
		if (fun != OBJNULL && type_of(fun) == t_cfun &&
		    fun->cfun.block == OBJNULL) {
			for (l = 0; l <= cl_num_symbols_in_core; l++) {
				cl_object s = (cl_object)(cl_symbols + l);
				if (fun == SYM_FUN(s)) {
					symbol = s;
					found = Ct;
					maxarg = MAKE_FIXNUM(fun->cfun.narg);
					break;
				}
			}
		}
	}
	package= symbol->symbol.hpack;
	if (package == cl_core.lisp_package)
		package = make_constant_string("cl");
	else if (package == cl_core.system_package)
		package = make_constant_string("si");
	else if (package == cl_core.keyword_package)
		package = Cnil;
	else
		package = package->pack.name;
	symbol = symbol->symbol.name;
	l      = symbol->string.fillp;
	source = symbol->string.self;
	output = cl_alloc_simple_string(length(package) + l + 1);
	if (is_symbol && source[0] == '*') {
		if (l > 2 && source[l-1] == '*') l--;
		c = 'V';
		l--;
		source++;
	} else if (is_symbol && l > 2 && source[0] == '+' && source[l-1] == '+') {
		c = 'C';
		l-= 2;
		source++;
	} else if (!is_symbol) {
		c = '_';
	} else if (package == cl_core.keyword_package) {
		c = 'K';
	} else {
		c = 'S';
	}
	output->string.fillp = 0;
	if (!Null(package))
		if (!mangle_name(output, package->string.self, package->string.fillp))
			@(return Cnil Cnil maxarg)
	output->string.self[output->string.fillp++] = c;
	if (!(dest = mangle_name(output, source, l)))
		@(return Cnil Cnil maxarg)
	if (dest[-1] == '_')
		dest[-1] = 'M';
	*(dest++) = '\0';
	@(return found output maxarg)
@)

static void
make_this_symbol(int i, cl_object s, int code, const char *name,
		 cl_objectfn fun, int narg, cl_object value)
{
	enum ecl_stype stp;
	cl_object package;
	bool form = 0;

	switch (code & 3) {
	case ORDINARY_SYMBOL: stp = stp_ordinary; break;
	case SPECIAL_SYMBOL: stp = stp_special; break;
	case CONSTANT_SYMBOL: stp = stp_constant; break;
	case FORM_SYMBOL: form = 1; stp = stp_ordinary;
	}
	switch (code & 12) {
	case CL_PACKAGE: package = cl_core.lisp_package; break;
	case SI_PACKAGE: package = cl_core.system_package; break;
	case KEYWORD_PACKAGE: package = cl_core.keyword_package; break;
#ifdef ECL_THREADS
	case MP_PACKAGE: package = cl_core.mp_package; break;
#endif
	}
	s->symbol.t = t_symbol;
	s->symbol.dynamic = 0;
	s->symbol.mflag = FALSE;
	ECL_SET(s, OBJNULL);
	SYM_FUN(s) = OBJNULL;
	s->symbol.plist = Cnil;
	s->symbol.hpack = Cnil;
	s->symbol.stype = stp;
	s->symbol.mflag = FALSE;
	s->symbol.isform = FALSE;
	s->symbol.hpack = package;
	s->symbol.name = make_constant_string(name);
	if (package == cl_core.keyword_package) {
		sethash(s->symbol.name, package->pack.external, s);
		ECL_SET(s, s);
	} else {
		ECL_SET(s, value);
		cl_import2(s, package);
		cl_export2(s, package);
	}
	if (!(s->symbol.isform = form) && fun) {
		cl_object f = cl_make_cfun_va(fun, s, NULL);
		SYM_FUN(s) = f;
		f->cfun.narg = narg;
	}
	cl_num_symbols_in_core = i + 1;
}

void
init_all_symbols(void)
{
	int i, code, narg;
	const char *name;
	cl_object s, value;
	cl_objectfn fun;

	/* We skip NIL and T */
	for (i = 2; cl_symbols[i].init.name != NULL; i++) {
		s = (cl_object)(cl_symbols + i);
		code = cl_symbols[i].init.type;
		name = cl_symbols[i].init.name;
		fun = (cl_objectfn)cl_symbols[i].init.fun;
		narg = cl_symbols[i].init.narg;
		value = cl_symbols[i].init.value;
		make_this_symbol(i, s, code, name, fun, narg, value);
	}
}
