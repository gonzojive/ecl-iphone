#include "ecls.h"
#include "page.h"

const struct symbol_info all_symbols[] = {
/* assignment.c */
{&Ssetf, "SETF", CL_ORDINARY},
{&Spsetf, "PSETF", CL_ORDINARY},
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
{&SlistX, "LIST*", CL_ORDINARY},
{&Sappend, "APPEND", CL_ORDINARY},
{&Snconc, "NCONC", CL_ORDINARY},

/* bind.c */
{&SAoptional, "&OPTIONAL", CL_ORDINARY},
{&SArest, "&REST", CL_ORDINARY},
{&SAkey, "&KEY", CL_ORDINARY},
{&SAallow_other_keys, "&ALLOW-OTHER-KEYS", CL_ORDINARY},
{&SAaux, "&AUX", CL_ORDINARY},

/* block.c */
{&Sblock, "BLOCK", CL_ORDINARY},

/* clos.c */
#ifdef CLOS
{&siSXclass_name_hash_tableX, "*CLASS-NAME-HASH-TABLE*", SI_SPECIAL},
{&Sclass, "CLASS", CL_ORDINARY},
{&Sbuilt_in, "BUILT-IN", CL_ORDINARY},
#endif

/* compiler.c */
{&siSlambda_block, "LAMBDA-BLOCK", CL_ORDINARY},

/* conditional.c */
{&Sotherwise, "OTHERWISE", CL_ORDINARY},

/* error.c */
{&Sarithmetic_error, "ARITHMETIC-ERROR", CL_ORDINARY},
{&Scell_error, "CELL-ERROR", CL_ORDINARY},
{&Scondition, "CONDITION", CL_ORDINARY},
{&Sdivision_by_zero, "DIVISION-BY-ZERO", CL_ORDINARY},
{&Send_of_file, "END-OF-FILE", CL_ORDINARY},
{&Serror, "ERROR", CL_ORDINARY},
{&Sfile_error, "FILE-ERROR", CL_ORDINARY},
{&Sfloating_point_inexact, "FLOATING-POINT-INEXACT", CL_ORDINARY},
{&Sfloating_point_invalid_operation, "FLOATING-POINT-INVALID-OPERATION", CL_ORDINARY},
{&Sfloating_point_overflow, "FLOATING-POINT-OVERFLOW", CL_ORDINARY},
{&Sfloating_point_underflow, "FLOATING-POINT-UNDERFLOW", CL_ORDINARY},
{&Spackage_error, "PACKAGE-ERROR", CL_ORDINARY},
{&Sparse_error, "PARSE-ERROR", CL_ORDINARY},
{&Sprint_not_readable, "PRINT-NOT-READABLE", CL_ORDINARY},
{&Sprogram_error, "PROGRAM-ERROR", CL_ORDINARY},
{&Sreader_error, "READER-ERROR", CL_ORDINARY},
{&Sserious_condition, "SERIOUS-CONDITION", CL_ORDINARY},
{&Ssimple_condition, "SIMPLE-CONDITION", CL_ORDINARY},
{&Ssimple_error, "SIMPLE-ERROR", CL_ORDINARY},
{&Ssimple_type_error, "SIMPLE-TYPE-ERROR", CL_ORDINARY},
{&Ssimple_warning, "SIMPLE-WARNING", CL_ORDINARY},
{&Sstorage_condition, "STORAGE-CONDITION", CL_ORDINARY},
{&Sstream_error, "STREAM-ERROR", CL_ORDINARY},
{&Sstyle_warning, "STYLE-WARNING", CL_ORDINARY},
{&Stype_error, "TYPE-ERROR", CL_ORDINARY},
{&Sunbound_slot, "UNBOUND-SLOT", CL_ORDINARY},
{&Sunbound_variable, "UNBOUND-VARIABLE", CL_ORDINARY},
{&Sundefined_function, "UNDEFINED-FUNCTION", CL_ORDINARY},
{&Swarning, "WARNING", CL_ORDINARY},

{&siSsimple_program_error, "SIMPLE-PROGRAM-ERROR", SI_ORDINARY},
{&siSsimple_control_error, "SIMPLE-CONTROL-ERROR", SI_ORDINARY},

{&siSuniversal_error_handler, "UNIVERSAL-ERROR-HANDLER", SI_ORDINARY},
{&siSterminal_interrupt, "TERMINAL-INTERRUPT", SI_ORDINARY},

/* eval.c */
{&Sapply, "APPLY", CL_ORDINARY},
{&Sfuncall, "FUNCALL", CL_ORDINARY},
{&Vevalhook, "*EVALHOOK*", CL_SPECIAL},
{&Vapplyhook, "*APPLYHOOK*", CL_SPECIAL},

/* file.c */
{&Vstandard_input, "*STANDARD-INPUT*", CL_SPECIAL},
{&Vstandard_output, "*STANDARD-OUTPUT*", CL_SPECIAL},
{&Verror_output, "*ERROR-OUTPUT*", CL_SPECIAL},
{&Vquery_io, "*QUERY-IO*", CL_SPECIAL},
{&Vdebug_io, "*DEBUG-IO*", CL_SPECIAL},
{&Vterminal_io, "*TERMINAL-IO*", CL_SPECIAL},
{&Vtrace_output, "*TRACE-OUTPUT*", CL_SPECIAL},
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
{&Seq, "EQ", CL_ORDINARY},
{&Seql, "EQL", CL_ORDINARY},
{&Sequal, "EQUAL", CL_ORDINARY},

/* instance.c */
{&Sprint_object, "PRINT-OBJECT", CL_ORDINARY},

/* lex.c */
{&Smacro, "MACRO", CL_ORDINARY},
{&siSsymbol_macro, "SYMBOL-MACRO", SI_ORDINARY},
{&Stag, "TAG", CL_ORDINARY},

/* load.c */
{&Vload_verbose, "*LOAD-VERBOSE*", CL_SPECIAL},
{&Vload_print, "*LOAD-PRINT*", CL_SPECIAL},
{&siVload_hooks, "*LOAD-HOOKS*", SI_SPECIAL},
#ifdef PDE
{&siVsource_pathname, "*SOURCE-PATHNAME*", CL_SPECIAL},
#endif
#ifdef RSYM
{&siVsymbol_table, "*SYMBOL-TABLE*", CL_SPECIAL},
#endif

/* lwp.c */
#ifdef THREADS
{&Srunning, "RUNNING", CL_ORDINARY},
{&Ssuspended, "SUSPENDED", CL_ORDINARY},
{&Swaiting, "WAITING", CL_ORDINARY},
{&Sstopped, "STOPPED", CL_ORDINARY},
{&Sdead, "DEAD", CL_ORDINARY},
{&siSthread_top_level, "THREAD-TOP-LEVEL", SI_ORDINARY},
#endif

/* macros.c */
{&Vmacroexpand_hook, "*MACROEXPAND-HOOK*", CL_SPECIAL},
{&siSexpand_defmacro, "EXPAND-DEFMACRO", SI_ORDINARY},
{&siVinhibit_macro_special, "*INHIBIT-MACRO-SPECIAL*", SI_SPECIAL},

/* main.c */
{&siVsystem_directory, "*SYSTEM-DIRECTORY*", SI_SPECIAL},
{&Vfeatures, "*FEATURES*", CL_SPECIAL},

/* num_rand.c */
{&Vrandom_state, "*RANDOM-STATE*", CL_SPECIAL},

/* package.c */
{&Vpackage, "*PACKAGE*", CL_SPECIAL},

/* pathname.c */
{&Vdefault_pathname_defaults, "*DEFAULT-PATHNAME-DEFAULTS*", CL_SPECIAL},

/* print.c */
{&Vprint_escape, "*PRINT-ESCAPE*", CL_SPECIAL},
{&Vprint_pretty, "*PRINT-PRETTY*", CL_SPECIAL},
{&Vprint_circle, "*PRINT-CIRCLE*", CL_SPECIAL},
{&Vprint_base, "*PRINT-BASE*", CL_SPECIAL},
{&Vprint_radix, "*PRINT-RADIX*", CL_SPECIAL},
{&Vprint_case, "*PRINT-CASE*", CL_SPECIAL},
{&Vprint_gensym, "*PRINT-GENSYM*", CL_SPECIAL},
{&Vprint_level, "*PRINT-LEVEL*", CL_SPECIAL},
{&Vprint_length, "*PRINT-LENGTH*", CL_SPECIAL},
{&Vprint_array, "*PRINT-ARRAY*", CL_SPECIAL},
{&siSpretty_print_format, "PRETTY-PRINT-FORMAT", SI_ORDINARY},
{&siSsharp_exclamation, "#!", SI_ORDINARY},
{&siVprint_package, "*PRINT-PACKAGE*", SI_SPECIAL},
{&siVprint_structure, "*PRINT-STRUCTURE*", SI_SPECIAL},
#ifdef CLOS
{&Sstream_write_char, "STREAM-WRITE-CHAR", CL_ORDINARY},
{&Sstream_write_string, "STREAM-WRITE-STRING", CL_ORDINARY},
{&Sstream_fresh_line, "STREAM-FRESH-LINE", CL_ORDINARY},
{&Sstream_clear_output, "STREAM-CLEAR-OUTPUT", CL_ORDINARY},
{&Sstream_force_output, "STREAM-FORCE-OUTPUT", CL_ORDINARY},
#endif

/* profile.c */
#ifdef PROFILE
{&sSAprofile_arrayA, "*PROFILE-ARRAY*", SI_SPECIAL},
#endif PROFILE

/* read.c */
{&Vreadtable, "*READTABLE*", CL_SPECIAL},
{&Vread_default_float_format, "*READ-DEFAULT-FLOAT-FORMAT*", CL_SPECIAL},
{&Vread_base, "*READ-BASE*", CL_SPECIAL},
{&Vread_suppress, "*READ-SUPPRESS*", CL_SPECIAL},
{&siSsharp_comma, "#,", SI_ORDINARY},
#ifdef CLOS
{&Sstream_read_line, "STREAM-READ-LINE", CL_ORDINARY},
{&Sstream_read_char, "STREAM-READ-CHAR", CL_ORDINARY},
{&Sstream_unread_char, "STREAM-UNREAD-CHAR", CL_ORDINARY},
{&Sstream_peek_char, "STREAM-PEEK-CHAR", CL_ORDINARY},
{&Sstream_listen, "STREAM-LISTEN", CL_ORDINARY},
{&Sstream_clear_input, "STREAM-CLEAR-INPUT", CL_ORDINARY},
#endif

/* structure.c */
{&siSstructure_print_function, "STRUCTURE-PRINT-FUNCTION", SI_ORDINARY},
{&siSstructure_slot_descriptions, "STRUCTURE-SLOT-DESCRIPTIONS", SI_ORDINARY},
#ifndef CLOS
{&siSstructure_include, "STRUCTURE-INCLUDE", SI_ORDINARY},
#else
{&Sstructure_object, "STRUCTURE-OBJECT", CL_ORDINARY},
#endif

/* symbol.c */
{&siSpname, "PNAME", SI_ORDINARY},
{&Vgensym_counter, "*GENSYM-COUNTER*", CL_SPECIAL},

/* toplevel.c */
{&Sdeclare, "DECLARE", CL_ORDINARY},
{&Scompile, "COMPILE", CL_ORDINARY},
{&Sload, "LOAD", CL_ORDINARY},
{&Seval, "EVAL", CL_ORDINARY},
{&Sprogn, "PROGN", CL_ORDINARY},
{&Swarn, "WARN", CL_ORDINARY},
{&Stypep, "TYPEP", CL_ORDINARY},

/* typespec.c */
{&Squote, "QUOTE", CL_ORDINARY},
{&Slambda, "LAMBDA", CL_ORDINARY},
{&Sspecial, "SPECIAL", CL_ORDINARY},
{&St, "T", CL_ORDINARY},
{&Snil, "NIL", CL_ORDINARY},
{&Scommon, "COMMON", CL_ORDINARY},
{&Ssequence, "SEQUENCE", CL_ORDINARY},
{&Snull, "NULL", CL_ORDINARY},
{&Scons, "CONS", CL_ORDINARY},
{&Slist, "LIST", CL_ORDINARY},
{&Ssymbol, "SYMBOL", CL_ORDINARY},
{&Sarray, "ARRAY", CL_ORDINARY},
{&Svector, "VECTOR", CL_ORDINARY},
{&Sbit_vector, "BIT-VECTOR", CL_ORDINARY},
{&Sstring, "STRING", CL_ORDINARY},
{&Ssimple_array, "SIMPLE-ARRAY", CL_ORDINARY},
{&Ssimple_vector, "SIMPLE-VECTOR", CL_ORDINARY},
{&Ssimple_string, "SIMPLE-STRING", CL_ORDINARY},
{&Ssimple_bit_vector, "SIMPLE-BIT-VECTOR", CL_ORDINARY},
{&Sfunction, "FUNCTION", CL_ORDINARY},
{&Spathname, "PATHNAME", CL_ORDINARY},
{&Slogical_pathname, "LOGICAL-PATHNAME", CL_ORDINARY},
{&Scharacter, "CHARACTER", CL_ORDINARY},
{&Sbase_char, "BASE-CHAR", CL_ORDINARY},
{&Sextended_char, "EXTENDED-CHAR", CL_ORDINARY},
{&Scompiled_function, "COMPILED-FUNCTION", CL_ORDINARY},
{&Snumber, "NUMBER", CL_ORDINARY},
{&Sreal, "REAL", CL_ORDINARY},
{&Srational, "RATIONAL", CL_ORDINARY},
{&Sfloat, "FLOAT", CL_ORDINARY},
{&Sinteger, "INTEGER", CL_ORDINARY},
{&Sratio, "RATIO", CL_ORDINARY},
{&Sshort_float, "SHORT-FLOAT", CL_ORDINARY},
{&Sstandard_char, "STANDARD-CHAR", CL_ORDINARY},
{&Sfixnum, "FIXNUM", CL_ORDINARY},
{&Scomplex, "COMPLEX", CL_ORDINARY},
{&Ssingle_float, "SINGLE-FLOAT", CL_ORDINARY},
{&Spackage, "PACKAGE", CL_ORDINARY},
{&Sbignum, "BIGNUM", CL_ORDINARY},
{&Srandom_state, "RANDOM-STATE", CL_ORDINARY},
{&Sdouble_float, "DOUBLE-FLOAT", CL_ORDINARY},
{&Sstream, "STREAM", CL_ORDINARY},
{&Sbit, "BIT", CL_ORDINARY},
{&Sreadtable, "READTABLE", CL_ORDINARY},
{&Slong_float, "LONG-FLOAT", CL_ORDINARY},
{&Shash_table, "HASH-TABLE", CL_ORDINARY},
{&Ssigned_char, "SIGNED-CHAR", CL_ORDINARY},
{&Sunsigned_char, "UNSIGNED-CHAR", CL_ORDINARY},
{&Ssigned_short, "SIGNED-SHORT", CL_ORDINARY},
{&Sunsigned_short, "UNSIGNED-SHORT", CL_ORDINARY},
#ifdef CLOS
{&Sinstance, "INSTANCE", CL_ORDINARY},
{&Sdispatch_function, "DISPATCH-FUNCTION", CL_ORDINARY},
{&Sstructure, "STRUCTURE", CL_ORDINARY},
#endif
{&Ssatisfies, "SATISFIES", CL_ORDINARY},
{&Smember, "MEMBER", CL_ORDINARY},
{&Snot, "NOT", CL_ORDINARY},
{&Sor, "OR", CL_ORDINARY},
{&Sand, "AND", CL_ORDINARY},
{&Svalues, "VALUES", CL_ORDINARY},
{&Smod, "MOD", CL_ORDINARY},
{&Ssigned_byte, "SIGNED-BYTE", CL_ORDINARY},
{&Sunsigned_byte, "UNSIGNED-BYTE", CL_ORDINARY},
{&SX, "*", CL_ORDINARY},
{&Splusp, "PLUSP", CL_ORDINARY},
{&Skeyword, "KEYWORD", CL_ORDINARY},
#ifdef THREADS
{&Scont, "CONT", CL_ORDINARY},
{&Sthread, "THREAD", CL_ORDINARY},
#endif
#ifdef LOCATIVE
{&Slocative, "LOCATIVE", CL_ORDINARY},
#endif
{&Ssubtypep, "SUBTYPEP", CL_ORDINARY},

/* unify.c */
#ifdef LOCATIVE
{&Ssetq, "SETQ", CL_ORDINARY},
{&Sunify_slot, "UNIFY-SLOT", CL_ORDINARY},
#endif

{NULL, (const char*)NULL, CL_ORDINARY}};

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
