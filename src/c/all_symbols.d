#include <ctype.h>
#include "ecl.h"
#include "internal.h"

#define CL_ORDINARY 0
#define CL_SPECIAL 1
#define CL_CONSTANT 2
#define SI_ORDINARY 4
#define SI_SPECIAL 5
#define KEYWORD 10
#define FORM_ORDINARY 16

#include "symbols_list.h"

cl_index cl_num_symbols_in_core = 0;

@(defun si::mangle-name (symbol &optional as_symbol)
	int l;
	char c, *source, *dest;
	cl_object output;
	cl_object package;
	cl_object found = Cnil;
	cl_object maxarg = MAKE_FIXNUM(-1);
	bool is_symbol;
@
	assert_type_symbol(symbol);
	is_symbol = (as_symbol == Cnil);
	if (is_symbol) {
		cl_fixnum p;

		if (symbol == Cnil)
			@(return Ct make_simple_string("Cnil"))
		else if (symbol == Ct)
			@(return Ct make_simple_string("Ct"))
		p  = (cl_symbol_initializer*)symbol - cl_symbols;
		if (p >= 0 && p <= cl_num_symbols_in_core) {
			found = Ct;
			output = @format(3, Cnil,
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
	symbol = symbol->symbol.name;
	l      = symbol->string.fillp;
	source = symbol->string.self;
	output = cl_alloc_simple_string(l+1); array_allocself(output);
	dest   = output->string.self;
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
	} else if (package == keyword_package) {
		c = 'K';
	} else {
		c = 'S';
	}
	if (package == lisp_package)
		package = make_simple_string("cl");
	else if (package == system_package)
		package = make_simple_string("si");
	else if (package == keyword_package)
		package = Cnil;
	else
		package = lisp_package->pack.name;
	*(dest++) = c;
	output->string.fillp = 1;
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
			@(return Cnil Cnil maxarg)
		}
		*(dest++) = c;
		output->string.fillp++;
	}
	if (dest[-1] == '_')
		dest[-1] = 'M';
	*(dest++) = '\0';
	if (!Null(package))
		output = @si::string-concatenate(2,package,output);
	@(return found output maxarg)
@)

static void
make_this_symbol(int i, cl_object s, int code, const char *name,
		 cl_objectfn fun, int narg)
{
	enum stype stp;
	cl_object package;

	switch (code & 3) {
	case 0: stp = stp_ordinary; break;
	case 1: stp = stp_special; break;
	case 2: stp = stp_constant; break;
	}
	switch (code & 12) {
	case 0: package = lisp_package; break;
	case 4: package = system_package; name = name + 4; break;
	case 8: package = keyword_package; name = name + 1; break;
	}
	s->symbol.t = t_symbol;
	s->symbol.mflag = FALSE;
	SYM_VAL(s) = OBJNULL;
	SYM_FUN(s) = OBJNULL;
	s->symbol.plist = Cnil;
	s->symbol.hpack = Cnil;
	s->symbol.stype = stp;
	s->symbol.mflag = FALSE;
	s->symbol.isform = FALSE;
	s->symbol.hpack = package;
	s->symbol.name = make_constant_string(name);
	if (package == keyword_package) {
		sethash(s->symbol.name, package->pack.external, s);
		SYM_VAL(s) = s;
	} else {
		cl_import2(s, package);
		cl_export2(s, package);
	}
	if (code == FORM_ORDINARY)
		s->symbol.isform = TRUE;
	else if (fun != NULL) {
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
	cl_object s;
	cl_objectfn fun;

	/* We skip NIL and T */
	for (i = 2; cl_symbols[i].init.name != NULL; i++) {
		s = (cl_object)(cl_symbols + i);
		code = cl_symbols[i].init.type;
		name = cl_symbols[i].init.name;
		fun = (cl_objectfn)cl_symbols[i].init.fun;
		narg = cl_symbols[i].init.narg;
		make_this_symbol(i, s, code, name, fun, narg);
	}
}
