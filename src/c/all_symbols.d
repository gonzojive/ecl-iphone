#include <ctype.h>
#include "ecl.h"
#include "internal.h"

#include "symbols_list.h"

struct symbol cl_symbols[393];

@(defun si::mangle-name (symbol &optional as_symbol)
	int l;
	char c, *source, *dest;
	cl_object output;
	cl_object package;
	cl_object found = Cnil;
	bool is_symbol;
@
	assert_type_symbol(symbol);
	is_symbol = (as_symbol == Cnil);
	if (is_symbol) {
		if (symbol == Cnil)
			@(return Ct make_simple_string("Cnil"))
		else if (symbol == Ct)
			@(return Ct make_simple_string("Ct"))
		for (l = 0; all_symbols[l].name != NULL; l++) {
			if (symbol == (cl_object)(cl_symbols + l)) {
				found = Ct;
				output = @format(3, Cnil,
						 make_constant_string("((cl_object)(cl_symbols+~A))"),
						 MAKE_FIXNUM(l));
				@(return found output)
			}
		}
	} else {
		cl_object fun;
		fun = symbol->symbol.gfdef;
		if (fun != OBJNULL && type_of(fun) == t_cfun) {
			for (l = 0; all_functions[l].name != NULL; l++)
				if ((cl_objectfn)fun->cfun.entry ==
				    (cl_objectfn)all_functions[l].f) {
					if (fun->cfun.name != Cnil)
						symbol = fun->cfun.name;
					found = Ct;
					break;
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
		c = 'L';
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
			@(return Cnil Cnil)
		}
		*(dest++) = c;
		output->string.fillp++;
	}
	if (dest[-1] == '_')
		dest[-1] = 'M';
	*(dest++) = '\0';
	if (!Null(package))
		output = @si::string-concatenate(2,package,output);
	@(return found output)
@)

static void
make_this_symbol(int index, const char *name, cl_object package, bool special)
{
	cl_object s = (cl_object)(cl_symbols + index);
	s->symbol.t = t_symbol;
	SYM_VAL(s) = OBJNULL;
	SYM_FUN(s) = OBJNULL;
	s->symbol.plist = Cnil;
	s->symbol.hpack = Cnil;
	s->symbol.stype = special? stp_special : stp_ordinary;
	s->symbol.mflag = FALSE;
	s->symbol.isform = FALSE;
	s->symbol.name = make_constant_string(name);
	s->symbol.hpack = package;
	cl_import(s, package);
	sethash(s->symbol.name, package->pack.external, s);
	if (package == keyword_package) {
		s->symbol.stype = stp_constant;
		SYM_VAL(s) = s;
	}
}

void
init_all_symbols(void)
{
	int i;

	/* We skip NIL and T */
	for (i = 2; all_symbols[i].name != NULL; i++) {
		switch (all_symbols[i].type) {
		case CL_ORDINARY:
			make_this_symbol(i, all_symbols[i].name, lisp_package, FALSE);
			break;
		case CL_SPECIAL:
			make_this_symbol(i, all_symbols[i].name, lisp_package, TRUE);
			break;
		case SI_ORDINARY:
			make_this_symbol(i, all_symbols[i].name+4, system_package, FALSE);
			break;
		case SI_SPECIAL:
			make_this_symbol(i, all_symbols[i].name+4, system_package, TRUE);
			break;
		case KEYWORD:
			make_this_symbol(i, all_symbols[i].name+1, keyword_package, TRUE);
		}
		if (all_symbols[i].loc != NULL)
		  *(all_symbols[i].loc) = (cl_object)(cl_symbols+i);
	}
}
