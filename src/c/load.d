/*
    load.d -- Binary loader (contains also open_fasl_data).
*/
/*
    Copyright (c) 1990, Giuseppe Attardi and William F. Schelter.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include "ecl.h"
#include "ecl-inl.h"

#ifdef __mips
#include <sys/cachectl.h>
#endif

#ifdef ENABLE_DLOPEN
cl_object
si_load_binary(cl_object filename, cl_object verbose, cl_object print)
{
	cl_object block;
	cl_object basename;
	cl_object prefix;

	/* We need the full pathname */
	filename = coerce_to_filename(cl_truename(filename));

	/* Try to load shared object file */
	block = cl_alloc_object(t_codeblock);
	block->cblock.data = NULL;
	block->cblock.data_size = 0;
	block->cblock.name = filename;
	block->cblock.handle = dlopen(filename->string.self, RTLD_NOW|RTLD_GLOBAL);
	if (block->cblock.handle == NULL)
		@(return make_string_copy(dlerror()))

	/* Fist try to call "init_CODE()" */
	block->cblock.entry = dlsym(block->cblock.handle, "init_CODE");
	if (block->cblock.entry != NULL)
		goto GO_ON;

	/* Next try to call "init_FILE()" where FILE is the file name */
	prefix = symbol_value(@'si::*init-function-prefix*');
	if (Null(prefix))
		prefix = make_simple_string("init_");
	else
		prefix = @si::string-concatenate(3,
						 make_simple_string("init_"),
						 prefix,
						 make_simple_string("_"));
	basename = cl_pathname(filename);
	basename = cl_pathname_name(basename);
	basename = @si::string-concatenate(2, prefix, @string-upcase(1,basename));
	block->cblock.entry = dlsym(block->cblock.handle, basename->string.self);

	if (block->cblock.entry == NULL) {
		dlclose(block->cblock.handle);
		@(return make_string_copy(dlerror()))
	}

	/* Finally, perform initialization */
GO_ON:	
	read_VV(block, block->cblock.entry);
	@(return Cnil)
}
#endif /* ENABLE_DLOPEN */

cl_object
si_load_source(cl_object source, cl_object verbose, cl_object print)
{
	cl_object x, strm;

	/* Source may be either a stream or a filename */
	if (type_of(source) != t_pathname && type_of(source) != t_string) {
		/* INV: if "source" is not a valid stream, file.d will complain */
		strm = source;
	} else {
		strm = open_stream(source, smm_input, Cnil, Cnil);
		if (Null(strm))
			@(return Cnil)
	}
	CL_UNWIND_PROTECT_BEGIN {
		bds_bind(@'*standard-input*', strm);
		for (;;) {
			cl_object bytecodes = Cnil;
			preserving_whitespace_flag = FALSE;
			detect_eos_flag = TRUE;
			x = read_object_non_recursive(strm);
			if (x == OBJNULL)
				break;
			eval(x, &bytecodes, Cnil);
			if (print != Cnil) {
				@write(1, x);
				@terpri(0);
			}
		}
		bds_unwind1;
	} CL_UNWIND_PROTECT_EXIT {
		/* We do not want to come back here if close_stream fails,
		   therefore, first we frs_pop() current jump point, then
		   try to close the stream, and then jump to next catch
		   point */
		if (strm != source)
			close_stream(strm, TRUE);
	} CL_UNWIND_PROTECT_END;
	@(return Cnil)
}

@(defun load (source
	      &key (verbose symbol_value(@'*load-verbose*'))
		   (print symbol_value(@'*load-print*'))
		   (if_does_not_exist @':error')
	      &aux pathname pntype hooks filename function defaults ok)
@
	/* If source is a stream, read conventional lisp code from it */
	if (type_of(source) != t_pathname &&  type_of(source) != t_string) {
		/* INV: if "source" is not a valid stream, file.d will complain */
		filename = source;
		function = Cnil;
		goto NOT_A_FILENAME;
	}
	pathname = coerce_to_physical_pathname(source);
	defaults = symbol_value(@'*default-pathname-defaults*');
	defaults = coerce_to_physical_pathname(defaults);
	pathname = merge_pathnames(pathname, defaults, @':newest');
	pntype   = pathname->pathname.type;

	filename = Cnil;
	hooks = symbol_value(@'si::*load-hooks*');
	if (!Null(pntype) && (pntype != @':wild')) {
		/* If filename already has an extension, make sure
		   that the file exists */
		filename = coerce_to_filename(pathname);
		if (!file_exists(filename)) {
			filename = Cnil;
		} else {
			function = cl_cdr(assoc(pathname->pathname.type, hooks));
		}
	} else loop_for_in(hooks) {
		/* Otherwise try with known extensions until a matching
		   file is found */
		pathname->pathname.type = CAAR(hooks);
		filename = coerce_to_filename(pathname);
		function = CDAR(hooks);
		if (file_exists(filename))
			break;
		else
			filename = Cnil;
	} end_loop_for_in;
	if (Null(filename)) {
		if (Null(if_does_not_exist))
			@(return Cnil)
		else
			FEcannot_open(pathname);
	}
NOT_A_FILENAME:
	if (verbose != Cnil) {
		@fresh-line(0);
		@format(3, Ct, make_simple_string(";;; Loading ~s~%"), filename);
	}
	bds_bind(@'*package*', symbol_value(@'*package*'));
#ifdef PDE
	bds_bind(@'si::*source-pathname*', filename);
#endif
	if (Null(function))
		ok = si_load_source(filename, verbose, print);
	else
		ok = funcall(4, function, filename, verbose, print);
#ifdef PDE
	bds_unwind1;
#endif
	bds_unwind1;
	if (!Null(ok))
		FEerror("LOAD: Could not load file ~S (Error: ~S)",
			2, filename, ok);
	if (print != Cnil) {
		@fresh-line(0);
		@format(3, Ct, make_simple_string(";;; Loading ~s~%"), filename);
	}
	@(return pathname)
@)


/* ---------------------------------------------------------------------- */
void
init_load(void)
{
  SYM_VAL(@'*load-verbose*') = Ct;
  SYM_VAL(@'*load-print*') = Cnil;
#ifdef PDE
  SYM_VAL(@'si::*source-pathname*') = Cnil;
#endif

  SYM_VAL(@'si::*load-hooks*') = cl_list(
#ifdef ENABLE_DLOPEN
				4,CONS(make_simple_string("so"), @'si::load-binary'),
#else
				3,

#endif
				CONS(make_simple_string("lsp"), @'si::load-source'),
				CONS(make_simple_string("lisp"), @'si::load-source'),
				CONS(Cnil, @'si::load-source'));

#ifdef ENABLE_DLOPEN
  if (dlopen(NULL, RTLD_NOW|RTLD_GLOBAL) == NULL)
    printf(";;; Error dlopening self file\n;;; Error: %s\n", dlerror());
#endif
  SYM_VAL(@'si::*init-function-prefix*') = Cnil;
}
