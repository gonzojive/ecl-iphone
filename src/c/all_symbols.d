#include "ecls.h"
#include "page.h"

const struct symbol_info all_symbols[] = {
/* assignment.c */
{&clSsetf, "SETF", CL_ORDINARY},
{&clSpsetf, "PSETF", CL_ORDINARY},
{&siSsetf_symbol, "SETF-SYMBOL", SI_ORDINARY},
{&siSclear_compiler_properties, "CLEAR-COMPILER-PROPERTIES", SI_ORDINARY},
#ifdef PDE
{&siVrecord_source_pathname_p, "*RECORD-SOURCE-PATHNAME-P*", SI_SPECIAL},
{&siSrecord_source_pathname, "RECORD-SOURCE-PATHNAME", SI_ORDINARY},
#endif

/* backq.c */
{&siScomma, ",", SI_ORDINARY},
{&siScomma_at, ",@@", SI_ORDINARY},
{&siScomma_dot, ",.", SI_ORDINARY},
{&clSlistX, "LIST*", CL_ORDINARY},
{&clSappend, "APPEND", CL_ORDINARY},
{&clSnconc, "NCONC", CL_ORDINARY},

/* bind.c */
{&clSAoptional, "&OPTIONAL", CL_ORDINARY},
{&clSArest, "&REST", CL_ORDINARY},
{&clSAkey, "&KEY", CL_ORDINARY},
{&clSAallow_other_keys, "&ALLOW-OTHER-KEYS", CL_ORDINARY},
{&clSAaux, "&AUX", CL_ORDINARY},

/* block.c */
{&clSblock, "BLOCK", CL_ORDINARY},

/* clos.c */
#ifdef CLOS
{&siVclass_name_hash_table, "*CLASS-NAME-HASH-TABLE*", SI_SPECIAL},
{&clSclass, "CLASS", CL_ORDINARY},
{&clSbuilt_in, "BUILT-IN", CL_ORDINARY},
#endif

/* compiler.c */
{&clSlambda_block, "LAMBDA-BLOCK", CL_ORDINARY},
{&siVkeep_definitions, "*KEEP-DEFINITIONS*", SI_SPECIAL},

/* conditional.c */
{&clSotherwise, "OTHERWISE", CL_ORDINARY},

/* error.c */
{&clSarithmetic_error, "ARITHMETIC-ERROR", CL_ORDINARY},
{&clScell_error, "CELL-ERROR", CL_ORDINARY},
{&clScondition, "CONDITION", CL_ORDINARY},
{&clSdivision_by_zero, "DIVISION-BY-ZERO", CL_ORDINARY},
{&clSend_of_file, "END-OF-FILE", CL_ORDINARY},
{&clSerror, "ERROR", CL_ORDINARY},
{&clSfile_error, "FILE-ERROR", CL_ORDINARY},
{&clSfloating_point_inexact, "FLOATING-POINT-INEXACT", CL_ORDINARY},
{&clSfloating_point_invalid_operation, "FLOATING-POINT-INVALID-OPERATION", CL_ORDINARY},
{&clSfloating_point_overflow, "FLOATING-POINT-OVERFLOW", CL_ORDINARY},
{&clSfloating_point_underflow, "FLOATING-POINT-UNDERFLOW", CL_ORDINARY},
{&clSpackage_error, "PACKAGE-ERROR", CL_ORDINARY},
{&clSparse_error, "PARSE-ERROR", CL_ORDINARY},
{&clSprint_not_readable, "PRINT-NOT-READABLE", CL_ORDINARY},
{&clSprogram_error, "PROGRAM-ERROR", CL_ORDINARY},
{&clSreader_error, "READER-ERROR", CL_ORDINARY},
{&clSserious_condition, "SERIOUS-CONDITION", CL_ORDINARY},
{&clSsimple_condition, "SIMPLE-CONDITION", CL_ORDINARY},
{&clSsimple_error, "SIMPLE-ERROR", CL_ORDINARY},
{&clSsimple_type_error, "SIMPLE-TYPE-ERROR", CL_ORDINARY},
{&clSsimple_warning, "SIMPLE-WARNING", CL_ORDINARY},
{&clSstorage_condition, "STORAGE-CONDITION", CL_ORDINARY},
{&clSstream_error, "STREAM-ERROR", CL_ORDINARY},
{&clSstyle_warning, "STYLE-WARNING", CL_ORDINARY},
{&clStype_error, "TYPE-ERROR", CL_ORDINARY},
{&clSunbound_slot, "UNBOUND-SLOT", CL_ORDINARY},
{&clSunbound_variable, "UNBOUND-VARIABLE", CL_ORDINARY},
{&clSundefined_function, "UNDEFINED-FUNCTION", CL_ORDINARY},
{&clSwarning, "WARNING", CL_ORDINARY},

{&siSsimple_program_error, "SIMPLE-PROGRAM-ERROR", SI_ORDINARY},
{&siSsimple_control_error, "SIMPLE-CONTROL-ERROR", SI_ORDINARY},

{&siSuniversal_error_handler, "UNIVERSAL-ERROR-HANDLER", SI_ORDINARY},
{&siSterminal_interrupt, "TERMINAL-INTERRUPT", SI_ORDINARY},

/* eval.c */
{&clSapply, "APPLY", CL_ORDINARY},
{&clSfuncall, "FUNCALL", CL_ORDINARY},

/* file.c */
{&clVstandard_input, "*STANDARD-INPUT*", CL_SPECIAL},
{&clVstandard_output, "*STANDARD-OUTPUT*", CL_SPECIAL},
{&clVerror_output, "*ERROR-OUTPUT*", CL_SPECIAL},
{&clVquery_io, "*QUERY-IO*", CL_SPECIAL},
{&clVdebug_io, "*DEBUG-IO*", CL_SPECIAL},
{&clVterminal_io, "*TERMINAL-IO*", CL_SPECIAL},
{&clVtrace_output, "*TRACE-OUTPUT*", CL_SPECIAL},
{&siVignore_eof_on_terminal_io, "*IGNORE-EOF-ON-TERMINAL-IO*", SI_SPECIAL},

/* format.c */
{&siVindent_formatted_output, "*INDENT-FORMATTED-OUTPUT*", SI_SPECIAL},

/* gbc.c */
#if 0 && !defined(GBC_BOEHM)
{&siVgc_verbose, "*GC-VERBOSE*", SI_SPECIAL},
{&siVgc_message, "*GC-MESSAGE*", SI_SPECIAL},
#endif /* !GBC_BOEHM */

/* gfun.c */
{&siScompute_applicable_methods, "COMPUTE-APPLICABLE-METHODS", SI_ORDINARY},
{&siScompute_effective_method, "COMPUTE-EFFECTIVE-METHOD", SI_ORDINARY},
{&siSgeneric_function_method_combination, "GENERIC-FUNCTION-METHOD-COMBINATION", SI_ORDINARY},
{&siSgeneric_function_method_combination_args, "GENERIC-FUNCTION-METHOD-COMBINATION-ARGS", SI_ORDINARY},

/* hash.c */
{&clSeq, "EQ", CL_ORDINARY},
{&clSeql, "EQL", CL_ORDINARY},
{&clSequal, "EQUAL", CL_ORDINARY},

/* instance.c */
{&clSprint_object, "PRINT-OBJECT", CL_ORDINARY},

/* lex.c */
{&clSmacro, "MACRO", CL_ORDINARY},
{&siSsymbol_macro, "SYMBOL-MACRO", SI_ORDINARY},
{&clStag, "TAG", CL_ORDINARY},

/* load.c */
{&clVload_verbose, "*LOAD-VERBOSE*", CL_SPECIAL},
{&clVload_print, "*LOAD-PRINT*", CL_SPECIAL},
{&siVload_hooks, "*LOAD-HOOKS*", SI_SPECIAL},
#ifdef PDE
{&siVsource_pathname, "*SOURCE-PATHNAME*", CL_SPECIAL},
#endif

/* lwp.c */
#ifdef THREADS
{&clSrunning, "RUNNING", CL_ORDINARY},
{&clSsuspended, "SUSPENDED", CL_ORDINARY},
{&clSwaiting, "WAITING", CL_ORDINARY},
{&clSstopped, "STOPPED", CL_ORDINARY},
{&clSdead, "DEAD", CL_ORDINARY},
{&siSthread_top_level, "THREAD-TOP-LEVEL", SI_ORDINARY},
#endif

/* macros.c */
{&clVmacroexpand_hook, "*MACROEXPAND-HOOK*", CL_SPECIAL},
{&siSexpand_defmacro, "EXPAND-DEFMACRO", SI_ORDINARY},
{&siVinhibit_macro_special, "*INHIBIT-MACRO-SPECIAL*", SI_SPECIAL},

/* main.c */
{&clVfeatures, "*FEATURES*", CL_SPECIAL},

/* num_rand.c */
{&clVrandom_state, "*RANDOM-STATE*", CL_SPECIAL},

/* package.c */
{&clVpackage, "*PACKAGE*", CL_SPECIAL},

/* pathname.c */
{&clVdefault_pathname_defaults, "*DEFAULT-PATHNAME-DEFAULTS*", CL_SPECIAL},

/* print.c */
{&clVprint_escape, "*PRINT-ESCAPE*", CL_SPECIAL},
{&clVprint_pretty, "*PRINT-PRETTY*", CL_SPECIAL},
{&clVprint_circle, "*PRINT-CIRCLE*", CL_SPECIAL},
{&clVprint_base, "*PRINT-BASE*", CL_SPECIAL},
{&clVprint_radix, "*PRINT-RADIX*", CL_SPECIAL},
{&clVprint_case, "*PRINT-CASE*", CL_SPECIAL},
{&clVprint_gensym, "*PRINT-GENSYM*", CL_SPECIAL},
{&clVprint_level, "*PRINT-LEVEL*", CL_SPECIAL},
{&clVprint_length, "*PRINT-LENGTH*", CL_SPECIAL},
{&clVprint_array, "*PRINT-ARRAY*", CL_SPECIAL},
{&siSpretty_print_format, "PRETTY-PRINT-FORMAT", SI_ORDINARY},
{&siSsharp_exclamation, "#!", SI_ORDINARY},
{&siVprint_package, "*PRINT-PACKAGE*", SI_SPECIAL},
{&siVprint_structure, "*PRINT-STRUCTURE*", SI_SPECIAL},
#ifdef CLOS
{&clSstream_write_char, "STREAM-WRITE-CHAR", CL_ORDINARY},
{&clSstream_write_string, "STREAM-WRITE-STRING", CL_ORDINARY},
{&clSstream_fresh_line, "STREAM-FRESH-LINE", CL_ORDINARY},
{&clSstream_clear_output, "STREAM-CLEAR-OUTPUT", CL_ORDINARY},
{&clSstream_force_output, "STREAM-FORCE-OUTPUT", CL_ORDINARY},
#endif

/* profile.c */
#ifdef PROFILE
{&sSAprofile_arrayA, "*PROFILE-ARRAY*", SI_SPECIAL},
#endif PROFILE

/* read.c */
{&clVreadtable, "*READTABLE*", CL_SPECIAL},
{&clVread_default_float_format, "*READ-DEFAULT-FLOAT-FORMAT*", CL_SPECIAL},
{&clVread_base, "*READ-BASE*", CL_SPECIAL},
{&clVread_suppress, "*READ-SUPPRESS*", CL_SPECIAL},
#ifdef CLOS
{&clSstream_read_line, "STREAM-READ-LINE", CL_ORDINARY},
{&clSstream_read_char, "STREAM-READ-CHAR", CL_ORDINARY},
{&clSstream_unread_char, "STREAM-UNREAD-CHAR", CL_ORDINARY},
{&clSstream_peek_char, "STREAM-PEEK-CHAR", CL_ORDINARY},
{&clSstream_listen, "STREAM-LISTEN", CL_ORDINARY},
{&clSstream_clear_input, "STREAM-CLEAR-INPUT", CL_ORDINARY},
#endif

/* structure.c */
{&siSstructure_print_function, "STRUCTURE-PRINT-FUNCTION", SI_ORDINARY},
{&siSstructure_slot_descriptions, "STRUCTURE-SLOT-DESCRIPTIONS", SI_ORDINARY},
#ifndef CLOS
{&siSstructure_include, "STRUCTURE-INCLUDE", SI_ORDINARY},
#else
{&clSstructure_object, "STRUCTURE-OBJECT", CL_ORDINARY},
#endif

/* symbol.c */
{&clVgensym_counter, "*GENSYM-COUNTER*", CL_SPECIAL},

/* toplevel.c */
{&clSdeclare, "DECLARE", CL_ORDINARY},
{&clScompile, "COMPILE", CL_ORDINARY},
{&clSload, "LOAD", CL_ORDINARY},
{&clSeval, "EVAL", CL_ORDINARY},
{&clSprogn, "PROGN", CL_ORDINARY},
{&clSwarn, "WARN", CL_ORDINARY},
{&clStypep, "TYPEP", CL_ORDINARY},

/* typespec.c */
{&clSquote, "QUOTE", CL_ORDINARY},
{&clSlambda, "LAMBDA", CL_ORDINARY},
{&clSspecial, "SPECIAL", CL_ORDINARY},
{&clScommon, "COMMON", CL_ORDINARY},
{&clSsequence, "SEQUENCE", CL_ORDINARY},
{&clSnull, "NULL", CL_ORDINARY},
{&clScons, "CONS", CL_ORDINARY},
{&clSlist, "LIST", CL_ORDINARY},
{&clSsymbol, "SYMBOL", CL_ORDINARY},
{&clSarray, "ARRAY", CL_ORDINARY},
{&clSvector, "VECTOR", CL_ORDINARY},
{&clSbit_vector, "BIT-VECTOR", CL_ORDINARY},
{&clSstring, "STRING", CL_ORDINARY},
{&clSsimple_array, "SIMPLE-ARRAY", CL_ORDINARY},
{&clSsimple_vector, "SIMPLE-VECTOR", CL_ORDINARY},
{&clSsimple_string, "SIMPLE-STRING", CL_ORDINARY},
{&clSsimple_bit_vector, "SIMPLE-BIT-VECTOR", CL_ORDINARY},
{&clSfunction, "FUNCTION", CL_ORDINARY},
{&clSpathname, "PATHNAME", CL_ORDINARY},
{&clSlogical_pathname, "LOGICAL-PATHNAME", CL_ORDINARY},
{&clScharacter, "CHARACTER", CL_ORDINARY},
{&clSbase_char, "BASE-CHAR", CL_ORDINARY},
{&clSextended_char, "EXTENDED-CHAR", CL_ORDINARY},
{&clScompiled_function, "COMPILED-FUNCTION", CL_ORDINARY},
{&clSnumber, "NUMBER", CL_ORDINARY},
{&clSreal, "REAL", CL_ORDINARY},
{&clSrational, "RATIONAL", CL_ORDINARY},
{&clSfloat, "FLOAT", CL_ORDINARY},
{&clSinteger, "INTEGER", CL_ORDINARY},
{&clSratio, "RATIO", CL_ORDINARY},
{&clSshort_float, "SHORT-FLOAT", CL_ORDINARY},
{&clSstandard_char, "STANDARD-CHAR", CL_ORDINARY},
{&clSfixnum, "FIXNUM", CL_ORDINARY},
{&clScomplex, "COMPLEX", CL_ORDINARY},
{&clSsingle_float, "SINGLE-FLOAT", CL_ORDINARY},
{&clSpackage, "PACKAGE", CL_ORDINARY},
{&clSbignum, "BIGNUM", CL_ORDINARY},
{&clSrandom_state, "RANDOM-STATE", CL_ORDINARY},
{&clSdouble_float, "DOUBLE-FLOAT", CL_ORDINARY},
{&clSstream, "STREAM", CL_ORDINARY},
{&clSbit, "BIT", CL_ORDINARY},
{&clSreadtable, "READTABLE", CL_ORDINARY},
{&clSlong_float, "LONG-FLOAT", CL_ORDINARY},
{&clShash_table, "HASH-TABLE", CL_ORDINARY},
{&clSsigned_char, "SIGNED-CHAR", CL_ORDINARY},
{&clSunsigned_char, "UNSIGNED-CHAR", CL_ORDINARY},
{&clSsigned_short, "SIGNED-SHORT", CL_ORDINARY},
{&clSunsigned_short, "UNSIGNED-SHORT", CL_ORDINARY},
#ifdef CLOS
{&clSinstance, "INSTANCE", CL_ORDINARY},
{&clSdispatch_function, "DISPATCH-FUNCTION", CL_ORDINARY},
{&clSstructure, "STRUCTURE", CL_ORDINARY},
#endif
{&clSsatisfies, "SATISFIES", CL_ORDINARY},
{&clSmember, "MEMBER", CL_ORDINARY},
{&clSnot, "NOT", CL_ORDINARY},
{&clSor, "OR", CL_ORDINARY},
{&clSand, "AND", CL_ORDINARY},
{&clSvalues, "VALUES", CL_ORDINARY},
{&clSmod, "MOD", CL_ORDINARY},
{&clSsigned_byte, "SIGNED-BYTE", CL_ORDINARY},
{&clSunsigned_byte, "UNSIGNED-BYTE", CL_ORDINARY},
{&clV, "*", CL_ORDINARY},
{&clSplusp, "PLUSP", CL_ORDINARY},
{&clSkeyword, "KEYWORD", CL_ORDINARY},
#ifdef THREADS
{&clScont, "CONT", CL_ORDINARY},
{&clSthread, "THREAD", CL_ORDINARY},
#endif
#ifdef LOCATIVE
{&clSlocative, "LOCATIVE", CL_ORDINARY},
#endif
{&clSsubtypep, "SUBTYPEP", CL_ORDINARY},

/* unify.c */
#ifdef LOCATIVE
{&clSsetq, "SETQ", CL_ORDINARY},
{&clSunify_slot, "UNIFY-SLOT", CL_ORDINARY},
#endif

{NULL, (const char*)NULL, CL_ORDINARY}};

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
		for (l = 0; all_symbols[l].loc != NULL; l++) {
			if (symbol == *(all_symbols[l].loc)) {
				found = Ct;
				break;
			}
		}
	} else {
		cl_object fun;
		fun = symbol->symbol.gfdef;
		if (fun != OBJNULL && type_of(fun) == t_cfun) {
			for (l = 0; all_functions[l].name != NULL; l++)
				if (fun->cfun.entry == all_functions[l].f) {
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
	output = alloc_simple_string(l+1); array_allocself(output);
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

void
init_all_symbols(void) {
  const struct symbol_info *s = all_symbols;
  cl_object *loc;

  /* This must keep the garbage collector happy */
  for (s = all_symbols; s->name != NULL; s++)
    *(s->loc) = OBJNULL;

  for (s = all_symbols; s->name != NULL; s++) {
    loc = s->loc;
    switch (s->type) {
    case CL_ORDINARY:
      *loc = make_ordinary(s->name);
      break;
    case CL_SPECIAL:
      *loc = make_special(s->name, Cnil);
      break;
    case SI_ORDINARY:
      *loc = make_si_ordinary(s->name);
      break;
    case SI_SPECIAL:
      *loc = make_si_special(s->name, Cnil);
      break;
    }
    /*    register_root(loc);*/
  }
}
