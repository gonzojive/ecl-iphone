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
#include "internal.h"

#ifdef ENABLE_DLOPEN
# ifdef HAVE_DLFCN_H
#  include <dlfcn.h>
#  define INIT_PREFIX "init_"
# endif
# ifdef HAVE_LINK_H
#  include <link.h>
# endif
# ifdef HAVE_MACH_O_DYLD_H
#  include <mach-o/dyld.h>
#  define INIT_PREFIX "_init_"
# endif
# if defined(mingw32) || defined(_MSC_VER)
#  include <windows.h>
#  include <windef.h>
#  include <winbase.h>
#  define INIT_PREFIX "init_"
# endif
#endif

#ifdef ENABLE_DLOPEN
cl_object
ecl_library_open(cl_object filename) {
	cl_object block;
	cl_object libraries = cl_core.libraries;
	block = cl_alloc_object(t_codeblock);
	block->cblock.data = NULL;
	block->cblock.data_size = 0;
	block->cblock.name = filename;
#ifdef HAVE_DLFCN_H
	block->cblock.handle = dlopen(filename->string.self,
				      RTLD_NOW|RTLD_GLOBAL);
#endif
#ifdef HAVE_MACH_O_DYLD_H
	{
	NSObjectFileImage file;
        static NSObjectFileImageReturnCode code;
	code = NSCreateObjectFileImageFromFile(filename->string.self, &file);
	if (code != NSObjectFileImageSuccess) {
		block->cblock.handle = NULL;
	} else {
		NSModule out = NSLinkModule(file, filename->string.self,
					    NSLINKMODULE_OPTION_PRIVATE|
					    NSLINKMODULE_OPTION_BINDNOW|
					    NSLINKMODULE_OPTION_RETURN_ON_ERROR);
		block->cblock.handle = out;
	}}
#endif
#if defined(mingw32) || defined(_MSC_VER)
	block->cblock.handle = LoadLibrary(filename->string.self);
#endif
	/* INV: We can modify "libraries" in a multithread
	   environment because we have already taken the
	   +load-compile-lock+ */
	if (libraries->vector.fillp == libraries->vector.dim) {
		cl_object nvector = cl_alloc_object(t_vector);
		nvector->vector = libraries->vector;
		libraries->vector.dim *= 2;
		libraries->vector.self.t =
			cl_alloc_atomic(libraries->vector.dim *
					sizeof(cl_object));
		memcpy(libraries->vector.self.t,
		       nvector->vector.self.t,
		       nvector->vector.fillp * sizeof(cl_object));
	}
	libraries->vector.self.t[libraries->vector.fillp++]
		= block;
	return block;
}

void *
ecl_library_symbol(cl_object block, const char *symbol) {
#ifdef HAVE_DLFCN_H
	return dlsym(block->cblock.handle, symbol);
#endif
#if defined(mingw32) || defined(_MSC_VER)
	HMODULE h = (HMODULE)(block->cblock.handle);
	return GetProcAddress(h, symbol);
#endif
#ifdef HAVE_MACH_O_DYLD_H
	NSSymbol sym;
	sym = NSLookupSymbolInModule((NSModule)(block->cblock.handle),
				     symbol);
	if (sym == 0) return 0;
	return (void*)NSAddressOfSymbol(sym);
#endif
}

cl_object
ecl_library_error(cl_object block) {
	const char *message;
#ifdef HAVE_DLFCN_H
	message = dlerror();
#endif
#ifdef HAVE_MACH_O_DYLD_H
	NSLinkEditErrors c;
	int number;
	const char *filename;
	NSLinkEditError(&c, &number, &filename, &message);
#endif
#if defined(mingw32) || defined(_MSC_VER)
	cl_object output;
	FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM |
		      FORMAT_MESSAGE_ALLOCATE_BUFFER,
		      0, GetLastError(), 0, (void*)&message, 0, NULL);
	output = make_string_copy(message);
	LocalFree(message);
	return output;
#endif
	return make_string_copy(message);
}

void
ecl_library_close(cl_object block) {
	const char *filename;
	cl_object libraries = cl_core.libraries;
	int i;

	if (block->cblock.handle == NULL)
		return;
	if (block->cblock.name)
		filename = block->cblock.name->string.self;
	else
		filename = "<anonymous>";
	printf("\n;;; Freeing library %s\n", filename);
#ifdef HAVE_DLFCN_H
	dlclose(block->cblock.handle);
#endif
#ifdef HAVE_MACH_O_DYLD_H
	NSUnLinkModule(block->cblock.handle, NSUNLINKMODULE_OPTION_NONE);
#endif
#if defined(mingw32) || defined(_MSC_VER)
	FreeLibrary(block->cblock.handle);
#endif
	if (block->cblock.self_destruct)
		unlink(filename);
	for (i = 0; i <= libraries->vector.fillp; i++) {
		if (libraries->vector.self.t[i] == block) {
			memcpy(libraries->vector.self.t+i,
			       libraries->vector.self.t+i+1,
			       (libraries->vector.fillp-i) *
			       sizeof(cl_object));
			libraries->vector.fillp--;
			break;
		}
	}
}

void
ecl_library_close_all(void)
{
	int i;
	while ((i = cl_core.libraries->vector.fillp)) {
		ecl_library_close(cl_core.libraries->vector.self.t[--i]);
	}
}

cl_object
si_load_binary(cl_object filename, cl_object verbose, cl_object print)
{
	cl_object block;
	cl_object basename;
	cl_object prefix;
	cl_object output;

	/* A full garbage collection enables us to detect unused code
	   and leave space for the library to be loaded. */
	si_gc(Ct);

	/* We need the full pathname */
	filename = cl_namestring(cl_truename(filename));

#ifdef ECL_THREADS
	/* Loading binary code is not thread safe. When another thread tries
	   to load the same file, we may end up initializing twice the same
	   module. */
	mp_get_lock(1, symbol_value(@'mp::+load-compile-lock+'));
	CL_UNWIND_PROTECT_BEGIN {
#endif
	/* Try to load shared object file */
	block = ecl_library_open(filename);
	if (block->cblock.handle == NULL) {
		output = ecl_library_error(block);
		goto OUTPUT;
	}

	/* Fist try to call "init_CODE()" */
	block->cblock.entry = ecl_library_symbol(block, INIT_PREFIX "CODE");
	if (block->cblock.entry != NULL)
		goto GO_ON;

	/* Next try to call "init_FILE()" where FILE is the file name */
	prefix = symbol_value(@'si::*init-function-prefix*');
	if (Null(prefix))
		prefix = make_constant_string(INIT_PREFIX);
	else
		prefix = @si::string-concatenate(3,
						 make_constant_string(INIT_PREFIX),
						 prefix,
						 make_constant_string("_"));
	basename = cl_pathname_name(1,filename);
	basename = @si::string-concatenate(2, prefix, @string-upcase(1,basename));
	block->cblock.entry = ecl_library_symbol(block, basename->string.self);

	if (block->cblock.entry == NULL) {
		output = ecl_library_error(block);
		ecl_library_close(block);
		goto OUTPUT;
	}

	/* Finally, perform initialization */
GO_ON:	
	read_VV(block, (void (*)(cl_object))(block->cblock.entry));
	output = Cnil;
OUTPUT:
#ifdef ECL_THREADS
	(void)0; /* MSVC complains about missing ';' before '}' */
	} CL_UNWIND_PROTECT_EXIT {
	mp_giveup_lock(symbol_value(@'mp::+load-compile-lock+'));
	} CL_UNWIND_PROTECT_END;
#endif
	@(return output)
}
#endif /* !ENABLE_DLOPEN */

cl_object
si_load_source(cl_object source, cl_object verbose, cl_object print)
{
	cl_object x, strm;

	/* Source may be either a stream or a filename */
	if (type_of(source) != t_pathname && type_of(source) != t_string) {
		/* INV: if "source" is not a valid stream, file.d will complain */
		strm = source;
	} else {
		strm = open_stream(source, smm_input, Cnil, Cnil, 8, 1, 1);
		if (Null(strm))
			@(return Cnil)
	}
	CL_UNWIND_PROTECT_BEGIN {
		for (;;) {
			x = cl_read(3, strm, Cnil, OBJNULL);
			if (x == OBJNULL)
				break;
			si_eval_with_env(1, x);
			if (print != Cnil) {
				@write(1, x);
				@terpri(0);
			}
		}
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
	           (search_list symbol_value(@'si::*load-search-list*'))
	      &aux pathname pntype hooks filename function ok)
@
	/* If source is a stream, read conventional lisp code from it */
	if (type_of(source) != t_pathname &&  type_of(source) != t_string) {
		/* INV: if "source" is not a valid stream, file.d will complain */
		filename = source;
		function = Cnil;
		goto NOT_A_FILENAME;
	}
	/* INV: coerce_to_file_pathname() creates a fresh new pathname object */
	pathname = coerce_to_file_pathname(source);
	pntype   = pathname->pathname.type;

	filename = Cnil;
	hooks = symbol_value(@'si::*load-hooks*');
	if (Null(pathname->pathname.directory) &&
	    Null(pathname->pathname.host) &&
	    Null(pathname->pathname.device) &&
	    !Null(search_list))
	{
		loop_for_in(search_list) {
			cl_object d = CAR(search_list);
			cl_object f = cl_merge_pathnames(2, pathname, d);
			cl_object ok = cl_load(9, f, @':verbose', verbose,
					       @':print', print,
					       @':if-does-not-exist', Cnil,
					       @':search-list', Cnil);
			if (!Null(ok)) {
				@(return ok);
			}
		} end_loop_for_in;
	}
	if (!Null(pntype) && (pntype != @':wild')) {
		/* If filename already has an extension, make sure
		   that the file exists */
		filename = si_coerce_to_filename(pathname);
		if (si_file_kind(filename, Ct) != @':file') {
			filename = Cnil;
		} else {
			function = cl_cdr(assoc(pathname->pathname.type, hooks));
		}
	} else loop_for_in(hooks) {
		/* Otherwise try with known extensions until a matching
		   file is found */
		filename = pathname;
		filename->pathname.type = CAAR(hooks);
		function = CDAR(hooks);
		if (si_file_kind(filename, Ct) == @':file')
			break;
		else
			filename = Cnil;
	} end_loop_for_in;
	if (Null(filename)) {
		if (Null(if_does_not_exist))
			@(return Cnil)
		else
			FEcannot_open(source);
	}
NOT_A_FILENAME:
	if (verbose != Cnil) {
		cl_format(3, Ct, make_constant_string("~&;;; Loading ~s~%"),
			  filename);
	}
	bds_bind(@'*package*', symbol_value(@'*package*'));
	bds_bind(@'*readtable*', symbol_value(@'*readtable*'));
	bds_bind(@'*load-pathname*', cl_pathname(filename));
	bds_bind(@'*load-truename*', cl_truename(filename));
	if (Null(function))
		ok = si_load_source(filename, verbose, print);
	else
		ok = funcall(4, function, filename, verbose, print);
	bds_unwind_n(4);
	if (!Null(ok))
		FEerror("LOAD: Could not load file ~S (Error: ~S)",
			2, filename, ok);
	if (print != Cnil) {
		cl_format(3, Ct, make_constant_string("~&;;; Loading ~s~%"),
			  filename);
	}
	@(return filename)
@)
