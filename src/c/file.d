/*
    file.d -- File interface.
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

/*
	IMPLEMENTATION-DEPENDENT

	The file contains code to reclaim the I/O buffer
	by accessing the FILE structure of C.
*/

#include <ecl.h>
#include "ecl-inl.h"
#include "internal.h"
#include <string.h>

#ifdef HAVE_SELECT
#include <sys/select.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#elif defined(mingw32)
#include <winsock.h>
#define HAVE_SELECT
#elif defined(HAVE_SYS_IOCTL_H) && !defined(MSDOS) && !defined(cygwin)
#include <sys/ioctl.h>
#endif

static int flisten(FILE *fp);

/*----------------------------------------------------------------------
 *	Input_stream_p(strm) answers
 *	if stream strm is an input stream or not.
 *	It does not check if it really is possible to read
 *	from the stream,
 *	but only checks the mode of the stream (sm_mode).
 *----------------------------------------------------------------------
 */
bool
input_stream_p(cl_object strm)
{
BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) == t_instance)
		return !Null(funcall(2, @'ext::stream-input-p', strm));
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_closed:
		FEclosed_stream(strm);
		break;

	case smm_io:
	case smm_input:
	case smm_concatenated:
	case smm_two_way:
	case smm_echo:
	case smm_string_input:
		return(TRUE);

	case smm_output:
	case smm_string_output:
	case smm_broadcast:
		return(FALSE);

	case smm_synonym:
		strm = symbol_value(strm->stream.object0);
		goto BEGIN;

	default:
		error("illegal stream mode");
	}
}

/*----------------------------------------------------------------------
 *	Output_stream_p(strm) answers
 *	if stream strm is an output stream.
 *	It does not check if it really is possible to write
 *	to the stream,
 *	but only checks the mode of the stream (sm_mode).
 *----------------------------------------------------------------------
 */
bool
output_stream_p(cl_object strm)
{
BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) == t_instance)
		return !Null(funcall(2, @'ext::stream-output-p', strm));
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_closed:
		FEclosed_stream(strm);
		return(FALSE);

	case smm_input:
	case smm_concatenated:
	case smm_string_input:
		return(FALSE);

	case smm_output:
	case smm_io:
	case smm_two_way:
	case smm_echo:
	case smm_broadcast:
	case smm_string_output:
		return(TRUE);

	case smm_synonym:
		strm = symbol_value(strm->stream.object0);
		goto BEGIN;

	default:
		error("illegal stream mode");
	}
}

/*
 * In ECL, all streams have element type (UNSIGNED-BYTE 8), (SIGNED-BYTE 8)
 * or BASE-CHAR. Nevertheless, READ-CHAR and WRITE-CHAR are allowed in them,
 * and they perform more or less as if
 *	(READ-CHAR) = (CODE-CHAR (READ-BYTE))
 *	(WRITE-CHAR c) = (WRITE-BYTE (CHAR-CODE c))
 */
cl_object
cl_stream_element_type(cl_object strm)
{
	cl_object x;
	cl_object output = @'base-char';

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) == t_instance)
		@(return @'base-char');
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_closed:
		FEclosed_stream(strm);

	case smm_input:
	case smm_output:
	case smm_io:
		output = ecl_elttype_to_symbol(strm->stream.elttype);
		break;

	case smm_synonym:
		strm = symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_broadcast:
		x = strm->stream.object0;
		if (endp(x)) {
			output = @'t';
			break;
		}
		strm = CAR(x);
		goto BEGIN;

	case smm_concatenated:
		x = strm->stream.object0;
		if (endp(x))
			break;
		strm = CAR(x);
		goto BEGIN;

	case smm_two_way:
	case smm_echo:
		strm = strm->stream.object0;
		goto BEGIN;

	case smm_string_input:
	case smm_string_output:
		break;

	default:
		error("illegal stream mode");
	}
	@(return output)
}

cl_object
cl_stream_external_format(cl_object strm)
{
	cl_object output;
	cl_type t = type_of(strm);
#ifdef ECL_CLOS_STREAMS
	if (t == t_instance)
		output = @':default';
	else
#endif
	if (t == t_stream)
		output = @':default';
	else
		FEwrong_type_argument(@'stream', strm);
	@(return output)
}

/*----------------------------------------------------------------------
 *	Error messages
 *----------------------------------------------------------------------
 */

static void not_an_input_stream(cl_object fn) __attribute__((noreturn));
static void not_an_output_stream(cl_object fn) __attribute__((noreturn));
static void wrong_file_handler(cl_object strm) __attribute__((noreturn));

static void
not_an_input_stream(cl_object strm)
{
	FEerror("Cannot read the stream ~S.", 1, strm);
}

static void
not_an_output_stream(cl_object strm)
{
	FEerror("Cannot write to the stream ~S.", 1, strm);
}

static void
io_error(cl_object strm)
{
	FElibc_error("Read or write operation to stream ~S signaled an error.",
		     1, strm);
}

static void
wrong_file_handler(cl_object strm)
{
	FEerror("Internal error: closed stream ~S without smm_mode flag.", 1, strm);
}

/*----------------------------------------------------------------------
 *	Open_stream(fn, smm, if_exists, if_does_not_exist)
 *	opens file fn with mode smm.
 *	Fn is a pathname designator.
 *----------------------------------------------------------------------
 */
cl_object
open_stream(cl_object fn, enum ecl_smmode smm, cl_object if_exists,
	    cl_object if_does_not_exist, cl_elttype elttype)
{
	cl_object x;
	FILE *fp;
	cl_object filename = si_coerce_to_filename(fn);
	char *fname = filename->string.self;

	if (smm == smm_input || smm == smm_probe) {
		fp = fopen(fname, OPEN_R);
		if (fp == NULL) {
			if (if_does_not_exist == @':error')
				FEcannot_open(fn);
			else if (if_does_not_exist == @':create') {
				fp = fopen(fname, OPEN_W);
				if (fp == NULL)
					FEcannot_open(fn);
				fclose(fp);
				fp = fopen(fname, OPEN_R);
				if (fp == NULL)
					FEcannot_open(fn);
			} else if (Null(if_does_not_exist)) {
				return(Cnil);
			} else {
				FEerror("~S is an illegal IF-DOES-NOT-EXIST option.",
					1, if_does_not_exist);
			}
		}
	} else if (smm == smm_output || smm == smm_io) {
		if (if_exists == @':new_version' && if_does_not_exist == @':create')
			goto CREATE;
		fp = fopen(fname, OPEN_R);
		if (fp != NULL) {
			fclose(fp);
			if (if_exists == @':error')
				FEcannot_open(fn);
			else if (if_exists == @':rename') {
				fp = backup_fopen(fname, (smm == smm_output)
						  ? OPEN_W
						  : OPEN_RW);
				if (fp == NULL)
					FEcannot_open(fn);
			} else if (if_exists == @':rename_and_delete' ||
				   if_exists == @':new_version' ||
				   if_exists == @':supersede') {
				fp = fopen(fname, (smm == smm_output)
					   ? OPEN_W
					   : OPEN_RW);
				if (fp == NULL)
					FEcannot_open(fn);
			} else if (if_exists == @':overwrite') {
				fp = fopen(fname, OPEN_RW);
				if (fp == NULL)
					FEcannot_open(fn);
			} else if (if_exists == @':append') {
				fp = fopen(fname, (smm == smm_output)
					   ? OPEN_A
					   : OPEN_RA);
				if (fp == NULL)
					FEcannot_open(fn);
			} else if (Null(if_exists)) {
				return(Cnil);
			} else {
				FEerror("~S is an illegal IF-EXISTS option.",
					1, if_exists);
			}
		} else {
			if (if_does_not_exist == @':error')
				FEcannot_open(fn);
			else if (if_does_not_exist == @':create') {
			CREATE:
				fp = fopen(fname, (smm == smm_output)
					   ? OPEN_W
					   : OPEN_RW);
				if (fp == NULL)
					FEcannot_open(fn);
			} else if (Null(if_does_not_exist)) {
				return(Cnil);
			} else {
				FEerror("~S is an illegal IF-DOES-NOT-EXIST option.",
					1, if_does_not_exist);
			}
		}
	} else {
		FEerror("Illegal stream mode ~S", 1, MAKE_FIXNUM(smm));
	}
	if (elttype == aet_bit) {
		elttype = aet_b8;
	} else if (elttype != aet_b8 &&
		   elttype != aet_i8 &&
		   elttype != aet_ch) {
		FEerror("~S is not a valid stream element type",
			1, ecl_elttype_to_symbol(elttype));
	}
	x = cl_alloc_object(t_stream);
	x->stream.mode = (short)smm;
	x->stream.file = fp;
	x->stream.elttype = elttype;
	x->stream.object1 = fn;
	x->stream.int0 = x->stream.int1 = 0;
#if !defined(GBC_BOEHM)
	setbuf(fp, x->stream.buffer = cl_alloc(BUFSIZ));
#endif
	if (smm == smm_probe)
		close_stream(x, 0);
	return(x);
}


/*----------------------------------------------------------------------
 *	Close_stream(strm, abort_flag) closes stream strm.
 *	The abort_flag is not used now.
 *----------------------------------------------------------------------
 */
void
close_stream(cl_object strm, bool abort_flag)        /*  Not used now!  */
{
	FILE *fp;

#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) == t_instance) {
		funcall(2, @'ext::stream-close', strm);
		return;
	}
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	fp = strm->stream.file;
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_closed:
		/* It is permissible to close a closed stream, although the output
		   is unspecified in those cases. */
		break;

	case smm_output:
		if (fp == stdout)
			FEerror("Cannot close the standard output.", 0);
		goto DO_CLOSE;
	case smm_input:
		if (fp == stdin)
			FEerror("Cannot close the standard input.", 0);
	DO_CLOSE:
	case smm_io:
	case smm_probe:
		if (fp == NULL)
			wrong_file_handler(strm);
		if (fclose(fp) != 0)
			FElibc_error("Cannot close stream ~S.", 1, strm);
#if !defined(GBC_BOEHM)
		cl_dealloc(strm->stream.buffer, BUFSIZ);
		strm->stream.file = NULL;
#endif
		break;

	case smm_synonym:
	case smm_broadcast:
	case smm_concatenated:
	case smm_two_way:
	case smm_echo:
	case smm_string_input:
	case smm_string_output:
	  /* The elements of a composite stream are not closed. For
	     composite streams we zero object1. For files we do not,
	     as it might contain an useful pathname */
		strm->stream.object1 = OBJNULL;
		break;

	default:
		error("illegal stream mode");
	}
	strm->stream.mode = smm_closed;
	strm->stream.file = NULL;
	strm->stream.object0 = OBJNULL;
}

cl_object
make_two_way_stream(cl_object istrm, cl_object ostrm)
{
	cl_object strm;

	strm = cl_alloc_object(t_stream);
	strm->stream.mode = (short)smm_two_way;
	strm->stream.file = NULL;
	strm->stream.object0 = istrm;
	strm->stream.object1 = ostrm;
	strm->stream.int0 = strm->stream.int1 = 0;
	return(strm);
}

cl_object
make_string_input_stream(cl_object strng, cl_index istart, cl_index iend)
{
	cl_object strm;

	strm = cl_alloc_object(t_stream);
	strm->stream.mode = (short)smm_string_input;
	strm->stream.file = NULL;
	strm->stream.object0 = strng;
	strm->stream.object1 = OBJNULL;
	strm->stream.int0 = istart;
	strm->stream.int1 = iend;
	return(strm);
}

cl_object
make_string_output_stream(cl_index line_length)
{
	cl_object s = cl_alloc_adjustable_string(line_length);
	return make_string_output_stream_from_string(s);
}

cl_object
make_string_output_stream_from_string(cl_object s)
{
	cl_object strm;

	if (type_of(s) != t_string || !s->string.hasfillp)
		FEerror("~S is not a string with a fill-pointer.", 1, s);
	strm = cl_alloc_object(t_stream);
	strm->stream.mode = (short)smm_string_output;
	strm->stream.file = NULL;
	strm->stream.object0 = s;
	strm->stream.object1 = OBJNULL;
	strm->stream.int0 = s->string.fillp;
	strm->stream.int1 = 0;
	return strm;
}

cl_object
get_output_stream_string(cl_object strm)
{
	cl_object strng;

	strng = copy_simple_string(strm->stream.object0);
	strm->stream.object0->string.fillp = 0;
	return(strng);
}



/*
 * ecl_getc(s) tries to read a character from the stream S. It outputs
 * either the code of the character read, or EOF. Whe compiled with
 * CLOS-STREAMS and S is an instance object, STREAM-READ-CHAR is invoked
 * to retrieve the character. Then STREAM-READ-CHAR should either
 * output the character, or NIL, indicating EOF.
 *
 * INV: ecl_getc(strm) checks the type of STRM.
 */
int
ecl_getc(cl_object strm)
{
	int c;
	FILE *fp;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) == t_instance) {
		cl_object c = funcall(2, @'ext::stream-read-char', strm);
		return CHARACTERP(c)? CHAR_CODE(c) : EOF;
	}
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	fp = strm->stream.file;
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_closed:
		FEclosed_stream(strm);
		break;

	case smm_input:
	case smm_io:
		if (fp == NULL)
			wrong_file_handler(strm);
		c = getc(fp);
		if (c == EOF && ferror(fp))
			io_error(strm);
		if (strm->stream.elttype == aet_i8) {
			c = (signed char)c;
		}
		break;

	case smm_synonym:
		strm = symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_concatenated: {
		cl_object strmi = strm->stream.object0;
		c = EOF;
		while (!endp(strmi)) {
			c = ecl_getc(CAR(strmi));
			if (c != EOF)
				break;
			strm->stream.object0 = strmi = CDR(strmi);
		}
		break;
	}
	case smm_two_way:
		if (strm == cl_core.terminal_io)
			flush_stream(cl_core.terminal_io->stream.object1);
		strm->stream.int1 = 0;
		strm = strm->stream.object0;
		goto BEGIN;

	case smm_echo:
		c = ecl_getc(strm->stream.object0);
		if (c != EOF) {
			if (strm->stream.int0 == 0)
				writec_stream(c, strm->stream.object1);
			else		/* don't echo twice if it was unread */
				--(strm->stream.int0);
		}
		break;

	case smm_string_input:
		if (strm->stream.int0 >= strm->stream.int1)
			c = EOF;
		else
			c = strm->stream.object0->string.self[strm->stream.int0++];
		break;

	case smm_output:
	case smm_broadcast:
	case smm_string_output:
		not_an_input_stream(strm);

	default:
		error("illegal stream mode");
	}
	return c;
}

/*
 * ecl_getc(s) tries to read a character from the stream S. It outputs
 * either the code of the character read, or EOF. Whe compiled with
 * CLOS-STREAMS and S is an instance object, STREAM-READ-CHAR is invoked
 * to retrieve the character. Then STREAM-READ-CHAR should either
 * output the character, or NIL, indicating EOF.
 *
 * INV: ecl_getc(strm) checks the type of STRM.
 */
int
ecl_peek_char(cl_object strm)
{
	int c;
	FILE *fp;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) == t_instance) {
		cl_object c = funcall(2, @'ext::stream-read-char', strm);
		if (CHARACTERP(c)) {
			funcall(3, @'ext::stream-unread-char', strm, c);
			return CHAR_CODE(c);
		} else {
			return EOF;
		}
	}
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	fp = strm->stream.file;
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_closed:
		FEclosed_stream(strm);
		break;

	case smm_input:
	case smm_io:
		if (fp == NULL)
			wrong_file_handler(strm);
		c = getc(fp);
		if (c == EOF && ferror(fp))
			io_error(strm);
		ungetc(c, fp);
		if (strm->stream.elttype == aet_i8) {
			c = (signed char)c;
		}
		break;

	case smm_synonym:
		strm = symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_concatenated: {
		cl_object strmi = strm->stream.object0;
		c = EOF;
		while (!endp(strmi)) {
			c = ecl_getc(CAR(strmi));
			if (c != EOF)
				break;
			strm->stream.object0 = strmi = CDR(strmi);
		}
		break;
	}
	case smm_two_way:
		if (strm == cl_core.terminal_io)
			flush_stream(cl_core.terminal_io->stream.object1);
		strm->stream.int1 = 0;
		strm = strm->stream.object0;
		goto BEGIN;

	case smm_echo:
		c = ecl_peek_char(strm->stream.object0);
		break;

	case smm_string_input:
		if (strm->stream.int0 >= strm->stream.int1)
			c = EOF;
		else
			c = strm->stream.object0->string.self[strm->stream.int0];
		break;

	case smm_output:
	case smm_broadcast:
	case smm_string_output:
		not_an_input_stream(strm);

	default:
		error("illegal stream mode");
	}
	return c;
}

int
ecl_getc_noeof(cl_object strm)
{
	int c = ecl_getc(strm);
	if (c == EOF)
		FEend_of_file(strm);
	return c;
}

void
ecl_ungetc(int c, cl_object strm)
{
	FILE *fp;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) == t_instance) {
		funcall(3, @'ext::stream-unread-char', strm, CODE_CHAR(c));
		return;
	}
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	fp = strm->stream.file;
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_closed:
		FEclosed_stream(strm);
		break;

	case smm_input:
	case smm_io:
		if (fp == NULL)
			wrong_file_handler(strm);
		ungetc(c, fp);
		if (c == EOF)
			io_error(strm);
/*		--strm->stream.int0; useless in smm_io, Beppe */
		break;

	case smm_synonym:
		strm = symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_concatenated:
		if (endp(strm->stream.object0))
			goto UNREAD_ERROR;
		strm = CAR(strm->stream.object0);
		goto BEGIN;

	case smm_two_way:
		strm = strm->stream.object0;
		goto BEGIN;

	case smm_echo:
		ecl_ungetc(c, strm->stream.object0);
		(strm->stream.int0)++;
		break;

	case smm_string_input:
		if (strm->stream.int0 <= 0)
			goto UNREAD_ERROR;
		--strm->stream.int0;
		break;

	case smm_output:
	case smm_broadcast:
	case smm_string_output:
		not_an_input_stream(strm);

	default:
		error("illegal stream mode");
	}
	return;

UNREAD_ERROR:
	FEerror("Cannot unread the stream ~S.", 1, strm);
}

int
writec_stream(int c, cl_object strm)
{
	cl_object x;
	FILE *fp;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) == t_instance) {
		funcall(3, @'ext::stream-write-char', strm, CODE_CHAR(c));
		return c;
	}
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	fp = strm->stream.file;
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_closed:
		FEclosed_stream(strm);
		break;

	case smm_output:
	case smm_io:
		if (c == '\n')
			strm->stream.int1 = 0;
		else if (c == '\t')
			strm->stream.int1 = (strm->stream.int1&~07) + 8;
		else
			strm->stream.int1++;
		if (fp == NULL)
			wrong_file_handler(strm);
		if (putc(c, fp) == EOF)
			io_error(strm);
		break;

	case smm_synonym:
		strm = symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_broadcast:
		for (x = strm->stream.object0; !endp(x); x = CDR(x))
			writec_stream(c, CAR(x));
		break;

	case smm_two_way:
		strm->stream.int0++;
		if (c == '\n')
			strm->stream.int1 = 0;
		else if (c == '\t')
			strm->stream.int1 = (strm->stream.int1&~07) + 8;
		else
			strm->stream.int1++;
		strm = strm->stream.object1;
		goto BEGIN;

	case smm_echo:
		strm = strm->stream.object1;
		goto BEGIN;

	case smm_string_output:
		strm->stream.int0++;
		if (c == '\n')
			strm->stream.int1 = 0;
		else if (c == '\t')
			strm->stream.int1 = (strm->stream.int1&~07) + 8;
		else
			strm->stream.int1++;
		ecl_string_push_extend(strm->stream.object0, c);
		break;

	case smm_input:
	case smm_concatenated:
	case smm_string_input:
		not_an_output_stream(strm);

	default:
		error("illegal stream mode");
	}
	return(c);
}

void
writestr_stream(const char *s, cl_object strm)
{
	while (*s != '\0')
		writec_stream(*s++, strm);
}

cl_object
si_do_write_sequence(cl_object seq, cl_object stream, cl_object s, cl_object e)
{
	cl_fixnum start = fixnnint(s);
	cl_fixnum limit = length(seq);
	cl_fixnum end = (e == Cnil)? limit : fixnnint(e);
	cl_type t = type_of(seq);

	/* Since we have called length(), we know that SEQ is a valid
	   sequence. Therefore, we only need to check the type of the
	   object, and seq == Cnil i.f.f. t = t_symbol */
	if (start > limit) {
		FEtype_error_index(seq, MAKE_FIXNUM(start));
	} else if (end > limit) {
		FEtype_error_index(seq, MAKE_FIXNUM(end));
	} else if (end <= start) {
		goto OUTPUT;
	}
	if (t == t_cons || t == t_symbol) {
		bool ischar = cl_stream_element_type(stream) == @'base-char';
		cl_object s = nthcdr(start, seq);
		loop_for_in(s) {
			if (start < end) {
				cl_object elt = CAR(s);
				cl_write_byte(ischar? cl_char_code(elt) : elt,
					      stream);
				start++;
			} else {
				goto OUTPUT;
			}
		} end_loop_for_in;
		goto OUTPUT;
	}
	if (t != t_string &&
	    !(t == t_array &&
	      (seq->vector.elttype == aet_b8 || seq->vector.elttype == aet_i8)))
	{
		bool ischar = cl_stream_element_type(stream) == @'base-char';
		while (start < end) {
			cl_object elt = aref(seq, start++);
			cl_write_byte(ischar? cl_char_code(elt) : elt, stream);
		}
		goto OUTPUT;
	}
 AGAIN:
	if ((t = type_of(stream)) == t_stream &&
	    (stream->stream.mode == smm_io ||
	     stream->stream.mode == smm_output))
	{
		size_t towrite = end - start;
		if (fwrite(seq->vector.self.ch + start, sizeof(char),
			   towrite, stream->stream.file) < towrite) {
			io_error(stream);
		}
	} else if (t == t_stream && stream->stream.mode == smm_two_way) {
		stream = stream->stream.object1;
		goto AGAIN;
	} else {
		unsigned char *p;
		for (p= seq->vector.self.ch; start < end; start++) {
			writec_stream(p[start], stream);
		}
	}
 OUTPUT:
	@(return seq);
}

cl_object
si_do_read_sequence(cl_object seq, cl_object stream, cl_object s, cl_object e)
{
	cl_fixnum start = fixnnint(s);
	cl_fixnum limit = length(seq);
	cl_fixnum end = (e == Cnil)? limit : fixnnint(e);
	cl_type t = type_of(seq);

	/* Since we have called length(), we know that SEQ is a valid
	   sequence. Therefore, we only need to check the type of the
	   object, and seq == Cnil i.f.f. t = t_symbol */
	if (start > limit) {
		FEtype_error_index(seq, MAKE_FIXNUM(start));
	} else if (end > limit) {
		FEtype_error_index(seq, MAKE_FIXNUM(end));
	} else if (end <= start) {
		goto OUTPUT;
	}
	if (t == t_cons || t == t_symbol) {
		bool ischar = cl_stream_element_type(stream) == @'base-char';
		seq = nthcdr(start, seq);
		loop_for_in(seq) {
			if (start >= end) {
				goto OUTPUT;
			} else {
				int c = ecl_getc(stream);
				if (c == EOF)
					goto OUTPUT;
				CAR(seq) = ischar? CODE_CHAR(c) : MAKE_FIXNUM(c);
				start++;
			}
		} end_loop_for_in;
		goto OUTPUT;
	}
	if (t != t_string &&
	    !(t == t_array &&
	      (seq->vector.elttype == aet_b8 || seq->vector.elttype == aet_i8)))
	{
		bool ischar = cl_stream_element_type(stream) == @'base-char';
		while (start < end) {
			int c = ecl_getc(stream);
			if (c == EOF)
				goto OUTPUT;
			aset(seq, start++, ischar? CODE_CHAR(c) : MAKE_FIXNUM(c));
		}
		goto OUTPUT;
	}
 AGAIN:
	if ((t = type_of(stream)) == t_stream &&
	    (stream->stream.mode == smm_io ||
	     stream->stream.mode == smm_output))
	{
		size_t toread = end - start;
		size_t n = fread(seq->vector.self.ch + start, sizeof(char),
				 toread, stream->stream.file);
		if (n < toread && ferror(stream->stream.file))
			io_error(stream);
		start += n;
	} else if (t == t_stream && stream->stream.mode == smm_two_way) {
		stream = stream->stream.object0;
		goto AGAIN;
	} else {
		unsigned char *p;
		for (p = seq->vector.self.ch; start < end; start++) {
			int c = ecl_getc(stream);
			if (c == EOF)
				break;
			p[start] = c;
		}
	}
 OUTPUT:
	@(return MAKE_FIXNUM(start))
}

void
flush_stream(cl_object strm)
{
	cl_object x;
	FILE *fp;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) == t_instance) {
		funcall(2, @'ext::stream-force-output', strm);
		return;
	}
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	fp = strm->stream.file;
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_closed:
		FEclosed_stream(strm);
		break;

	case smm_output:
	case smm_io:
		if (fp == NULL)
			wrong_file_handler(strm);
		if (fflush(fp) == EOF)
			io_error(strm);
		break;

	case smm_synonym:
		strm = symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_broadcast:
		for (x = strm->stream.object0; !endp(x); x = CDR(x))
			flush_stream(CAR(x));
		break;

	case smm_two_way:
	case smm_echo:
		strm = strm->stream.object1;
		goto BEGIN;

	case smm_string_output: {
	  	cl_object strng = strm->stream.object0;
		strng->string.self[strng->string.fillp] = '\0';
		break;
	      }
	case smm_input:
	case smm_concatenated:
	case smm_string_input:
		FEerror("Cannot flush the stream ~S.", 1, strm);

	default:
		error("illegal stream mode");
	}
}

void
clear_input_stream(cl_object strm)
{
	cl_object x;
	FILE *fp;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) == t_instance) {
		funcall(2, @'ext::stream-clear-input', strm);
		return;
	}
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	fp = strm->stream.file;
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_closed:
		FEclosed_stream(strm);
		break;

	case smm_input:
		if (fp == NULL)
			wrong_file_handler(strm);
		while (flisten(fp) == ECL_LISTEN_AVAILABLE) {
			getc(fp);
		}
		break;

	case smm_synonym:
		strm = symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_broadcast:
		for (x = strm->stream.object0; !endp(x); x = CDR(x))
			flush_stream(CAR(x));
		break;

	case smm_two_way:
	case smm_echo:
		strm = strm->stream.object0;
		goto BEGIN;

	case smm_string_output:
	case smm_io:
	case smm_output:
	case smm_concatenated:
	case smm_string_input:
		break;

	default:
		error("illegal stream mode");
	}
}

void
clear_output_stream(cl_object strm)
{
	cl_object x;
	FILE *fp;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) == t_instance) {
		funcall(2, @'ext::stream-clear-output',strm);
		return;
	}
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	fp = strm->stream.file;
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_closed:
		FEclosed_stream(strm);
		break;

	case smm_output:
#if 0
		if (fp == NULL)
			wrong_file_handler(strm);
		if (fseek(fp, 0L, 2) != 0)
			io_error(strm);
#endif
		break;

	case smm_synonym:
		strm = symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_broadcast:
		for (x = strm->stream.object0; !endp(x); x = CDR(x))
			flush_stream(CAR(x));
		break;

	case smm_two_way:
	case smm_echo:
		strm = strm->stream.object1;
		goto BEGIN;

	case smm_string_output:
	case smm_io:
	case smm_input:
	case smm_concatenated:
	case smm_string_input:
		break;

	default:
		error("illegal stream mode");
	}
}

static int
flisten(FILE *fp)
{
#ifdef HAVE_SELECT
	fd_set fds;
	int retv, fd;
	struct timeval tv = { 0, 0 };
#endif
	if (feof(fp))
		return ECL_LISTEN_EOF;
#ifdef FILE_CNT
	if (FILE_CNT(fp) > 0)
		return ECL_LISTEN_AVAILABLE;
#endif
#if !defined(mingw32)
#if defined(HAVE_SELECT)
	fd = fileno(fp);
	FD_ZERO(&fds);
	FD_SET(fd, &fds);
	retv = select(fd + 1, &fds, NULL, NULL, &tv);
	if (retv < 0)
		FElibc_error("select() returned an error value", 0);
	return (retv > 0)? ECL_LISTEN_AVAILABLE : ECL_LISTEN_NO_CHAR;
#elif defined(FIONREAD)
	{ long c = 0;
	ioctl(fileno(fp), FIONREAD, &c);
	return (c > 0)? ECL_LISTEN_AVAILABLE : ECL_LISTEN_NO_CHAR;
	}
#endif /* FIONREAD */
#endif
	return ECL_LISTEN_AVAILABLE;
}

int
ecl_listen_stream(cl_object strm)
{
	FILE *fp;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) == t_instance) {
		cl_object flag = funcall(2, @'ext::stream-listen', strm);
		return !(strm == Cnil);
	}
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_closed:
		FEclosed_stream(strm);
		return ECL_LISTEN_EOF;

	case smm_input:
	case smm_io:
		fp = strm->stream.file;
		if (fp == NULL)
			wrong_file_handler(strm);
		return flisten(fp);

	case smm_synonym:
		strm = symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_concatenated: {
		cl_object l = strm->stream.object0;
		while (!endp(l)) {
			int f = ecl_listen_stream(CAR(l));
			l = CDR(l);
			if (f == ECL_LISTEN_EOF) {
				strm->stream.object0 = l;
			} else {
				return f;
			}
		}
		return ECL_LISTEN_EOF;
	}
	case smm_two_way:
	case smm_echo:
		strm = strm->stream.object0;
		goto BEGIN;

	case smm_string_input:
		if (strm->stream.int0 < strm->stream.int1)
			return ECL_LISTEN_AVAILABLE;
		else
			return ECL_LISTEN_EOF;

	case smm_output:
	case smm_broadcast:
	case smm_string_output:
		not_an_input_stream(strm);

	default:
		error("illegal stream mode");
	}
}

long
file_position(cl_object strm)
{
	FILE *fp;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) == t_instance)
		FEerror("file-position not implemented for CLOS streams", 0);
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_closed:
		FEclosed_stream(strm);
		return(-1);

	case smm_input:
	case smm_output:
	case smm_io:
		fp = strm->stream.file;
		if (fp == NULL)
			wrong_file_handler(strm);
		return(ftell(fp));

	case smm_string_output:
		return(strm->stream.object0->string.fillp);

	case smm_synonym:
		strm = symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_broadcast:
		strm = strm->stream.object0;
		if (endp(strm))
			return 0;
		strm = CAR(strm);
		goto BEGIN;

	case smm_concatenated:
	case smm_two_way:
	case smm_echo:
	case smm_string_input:
		return(-1);

	default:
		error("illegal stream mode");
	}
}

long
file_position_set(cl_object strm, long disp)
{
	FILE *fp;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) == t_instance)
		FEerror("file-position not implemented for CLOS streams", 0);
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_closed:
		FEclosed_stream(strm);
		return(-1);

	case smm_input:
	case smm_output:
	case smm_io:
		fp = strm->stream.file;
		if (fp == NULL)
			wrong_file_handler(strm);
		if (fseek(fp, disp, 0) != 0)
			return(-1);
		return(0);

	case smm_string_output:
		if (disp < strm->stream.object0->string.fillp) {
			strm->stream.object0->string.fillp = disp;
			strm->stream.int0 = disp;
		} else {
			disp -= strm->stream.object0->string.fillp;
			while (disp-- > 0)
				writec_stream(' ', strm);
		}
		return(0);

	case smm_synonym:
		strm = symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_broadcast:
		strm = strm->stream.object0;
		if (endp(strm))
			return 0;
		strm = CAR(strm);
		goto BEGIN;

	case smm_concatenated:
	case smm_two_way:
	case smm_echo:
	case smm_string_input:
		return(-1);

	default:
		error("illegal stream mode");
	}
}

long
file_length(cl_object strm)
{
	FILE *fp;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) == t_instance)
		goto ERROR;
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_closed:
		FEclosed_stream(strm);
		return(-1);

	case smm_input:
	case smm_output:
	case smm_io:
		fp = strm->stream.file;
		if (fp == NULL)
			wrong_file_handler(strm);
		return(file_len(fp));

	case smm_synonym:
		strm = symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_broadcast:
		strm = strm->stream.object0;
		if (endp(strm)) {
			return 0;
		}
		strm = CAR(strm);
		goto BEGIN;

	/* FIXME! Should signal an error of type-error */
	case smm_concatenated:
	case smm_two_way:
	case smm_echo:
	case smm_string_input:
	case smm_string_output:
	ERROR:
		FEwrong_type_argument(c_string_to_object("(OR BROADCAST-STREAM SYNONYM-STREAM FILE-STREAM)"),
				      strm);

	default:
		error("illegal stream mode");
	}
}

cl_object si_file_column(cl_object strm)
{
	@(return MAKE_FIXNUM(file_column(strm)))
}

int
file_column(cl_object strm)
{

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (type_of(strm) == t_instance)
		return 0;
#endif
	if (type_of(strm) != t_stream)
		FEtype_error_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_closed:
		FEclosed_stream(strm);
		return 0;

	case smm_output:
	case smm_io:
	case smm_two_way:
	case smm_string_output:
		return(strm->stream.int1);

	case smm_synonym:
		strm = symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_echo:
		strm = strm->stream.object1;
		goto BEGIN;

	case smm_input:
	case smm_string_input:
		return 0;

	case smm_concatenated:
	case smm_broadcast:
		strm = strm->stream.object0;
		if (endp(strm))
			return 0;
		strm = CAR(strm);
		goto BEGIN;
	default:
		error("illegal stream mode");
	}
}

cl_object
cl_make_synonym_stream(cl_object sym)
{
	cl_object x;

	assert_type_symbol(sym);
	x = cl_alloc_object(t_stream);
	x->stream.mode = (short)smm_synonym;
	x->stream.file = NULL;
	x->stream.object0 = sym;
	x->stream.object1 = OBJNULL;
	x->stream.int0 = x->stream.int1 = 0;
	@(return x)
}

cl_object
cl_synonym_stream_symbol(cl_object strm)
{
	if (type_of(strm) != t_stream || strm->stream.mode != smm_synonym)
		FEwrong_type_argument(@'synonym-stream', strm);
	@(return strm->stream.object0)
}

@(defun make_broadcast_stream (&rest ap)
	cl_object x, streams;
	int i;
@
	streams = Cnil;
	for (i = 0; i < narg; i++) {
		x = cl_va_arg(ap);
		if (!output_stream_p(x))
			not_an_output_stream(x);
		streams = CONS(x, streams);
	}
	x = cl_alloc_object(t_stream);
	x->stream.mode = (short)smm_broadcast;
	x->stream.file = NULL;
	x->stream.object0 = cl_nreverse(streams);
	x->stream.object1 = OBJNULL;
	x->stream.int0 = x->stream.int1 = 0;
	@(return x)
@)

cl_object
cl_broadcast_stream_streams(cl_object strm)
{
	if (type_of(strm) != t_stream || strm->stream.mode != smm_broadcast)
		FEwrong_type_argument(@'broadcast-stream', strm);
	return cl_copy_list(strm->stream.object0);
}

@(defun make_concatenated_stream (&rest ap)
	cl_object x, streams;
	int i;
@
	streams = Cnil;
	for (i = 0; i < narg; i++) {
		x = cl_va_arg(ap);
		if (!input_stream_p(x))
			not_an_input_stream(x);
		streams = CONS(x, streams);
	}
	x = cl_alloc_object(t_stream);
	x->stream.mode = (short)smm_concatenated;
	x->stream.file = NULL;
	x->stream.object0 = cl_nreverse(streams);
	x->stream.object1 = OBJNULL;
	x->stream.int0 = x->stream.int1 = 0;
	@(return x)
@)

cl_object
cl_concatenated_stream_streams(cl_object strm)
{
	if (type_of(strm) != t_stream || strm->stream.mode != smm_concatenated)
		FEwrong_type_argument(@'concatenated-stream', strm);
	return cl_copy_list(strm->stream.object0);
}

cl_object
cl_make_two_way_stream(cl_object strm1, cl_object strm2)
{
	if (!input_stream_p(strm1))
		not_an_input_stream(strm1);
	if (!output_stream_p(strm2))
		not_an_output_stream(strm2);
	@(return make_two_way_stream(strm1, strm2))
}

cl_object
cl_two_way_stream_input_stream(cl_object strm)
{
	if (type_of(strm) != t_stream || strm->stream.mode != smm_two_way)
		FEwrong_type_argument(@'two-way-stream', strm);
	@(return strm->stream.object0)
}

cl_object
cl_two_way_stream_output_stream(cl_object strm)
{
	if (type_of(strm) != t_stream || strm->stream.mode != smm_two_way)
		FEwrong_type_argument(@'two-way-stream', strm);
	@(return strm->stream.object1)
}

cl_object
cl_make_echo_stream(cl_object strm1, cl_object strm2)
{
	cl_object output;
	if (!input_stream_p(strm1))
		not_an_input_stream(strm1);
	if (!output_stream_p(strm2))
		not_an_output_stream(strm2);
	output = make_two_way_stream(strm1, strm2);
	output->stream.mode = smm_echo;
	@(return output)
}

cl_object
cl_echo_stream_input_stream(cl_object strm)
{
	if (type_of(strm) != t_stream || strm->stream.mode != smm_echo)
		FEwrong_type_argument(@'echo-stream', strm);
	@(return strm->stream.object0)
}

cl_object
cl_echo_stream_output_stream(cl_object strm)
{
	if (type_of(strm) != t_stream || strm->stream.mode != smm_echo)
		FEwrong_type_argument(@'echo-stream', strm);
	@(return strm->stream.object1)
}

@(defun make_string_input_stream (strng &o istart iend)
	cl_index s, e;
@
	assert_type_string(strng);
	if (Null(istart))
		s = 0;
	else if (!FIXNUMP(istart) || FIXNUM_MINUSP(istart))
		goto E;
	else
		s = (cl_index)fix(istart);
	if (Null(iend))
		e = strng->string.fillp;
	else if (!FIXNUMP(iend) || FIXNUM_MINUSP(iend))
		goto E;
	else
		e = (cl_index)fix(iend);
	if (e > strng->string.fillp || s > e)
		goto E;
	@(return (make_string_input_stream(strng, s, e)))

E:
	FEerror("~S and ~S are illegal as :START and :END~%\
for the string ~S.",
		3, istart, iend, strng);
@)

@(defun make-string-output-stream (&key (element_type @'base-char'))
@
	@(return make_string_output_stream(128))
@)

cl_object
cl_get_output_stream_string(cl_object strm)
{
	if (type_of(strm) != t_stream ||
	    (enum ecl_smmode)strm->stream.mode != smm_string_output)
		FEerror("~S is not a string-output stream.", 1, strm);
	@(return get_output_stream_string(strm))
}

/*----------------------------------------------------------------------
 *	(SI:OUTPUT-STREAM-STRING string-output-stream)
 *
 *		extracts the string associated with the given
 *		string-output-stream.
 *----------------------------------------------------------------------
 */
cl_object
si_output_stream_string(cl_object strm)
{
	if (type_of(strm) != t_stream ||
	    (enum ecl_smmode)strm->stream.mode != smm_string_output)
		FEerror("~S is not a string-output stream.", 1, strm);
	@(return strm->stream.object0)
}

cl_object
cl_streamp(cl_object strm)
{
	@(return ((type_of(strm) == t_stream) ? Ct : Cnil))
}

cl_object
cl_input_stream_p(cl_object strm)
{
	@(return (input_stream_p(strm) ? Ct : Cnil))
}

cl_object
cl_output_stream_p(cl_object strm)
{
	@(return (output_stream_p(strm) ? Ct : Cnil))
}

@(defun close (strm &key abort)
@
	close_stream(strm, abort != Cnil);
	@(return Ct)
@)

@(defun open (filename
	      &key (direction @':input')
		   (element_type @'base-char')
		   (if_exists Cnil iesp)
		   (if_does_not_exist Cnil idnesp)
	           (external_format @':default')
	      &aux strm)
	enum ecl_smmode smm;
	cl_elttype elttype;
@
	if (external_format != @':default')
		FEerror("~S is not a valid stream external format.", 1,
			external_format);
	/* INV: open_stream() checks types */
	if (direction == @':input') {
		smm = smm_input;
		if (!idnesp)
			if_does_not_exist = @':error';
	} else if (direction == @':output') {
		smm = smm_output;
		if (!iesp)
			if_exists = @':new_version';
		if (!idnesp) {
			if (if_exists == @':overwrite' ||
			    if_exists == @':append')
				if_does_not_exist = @':error';
			else
				if_does_not_exist = @':create';
		}
	} else if (direction == @':io') {
		smm = smm_io;
		if (!iesp)
			if_exists = @':new_version';
		if (!idnesp) {
			if (if_exists == @':overwrite' ||
			    if_exists == @':append')
				if_does_not_exist = @':error';
			else
				if_does_not_exist = @':create';
		}
	} else if (direction == @':probe') {
		smm = smm_probe;
		if (!idnesp)
			if_does_not_exist = Cnil;
	} else {
		FEerror("~S is an illegal DIRECTION for OPEN.",
			1, direction);
 	}
	if (element_type == @':default') {
		elttype = aet_ch;
	} else if (element_type == @'signed-byte') {
		elttype = aet_i8;
	} else if (element_type == @'unsigned-byte') {
		elttype = aet_b8;
	} else {
		elttype = ecl_symbol_to_elttype(element_type);
		if (elttype == aet_object) {
			FEerror("~S is not a valid stream element type",
				1, elttype);
		}
	}
	strm = open_stream(filename, smm, if_exists, if_does_not_exist, elttype);
	@(return strm)
@)

@(defun file-position (file_stream &o position)
	long i;
	cl_object output;
@
	if (Null(position)) {
		i = file_position(file_stream);
		output = (i < 0)? Cnil : MAKE_FIXNUM(i);
	} else {
		if (position == @':start') {
			i = 0;
		} else if (position == @':end') {
			i = file_length(file_stream);
		} else if (!FIXNUMP(position) || (i = fix((position))) < 0) {
			FEerror("~S is an illegal file position~%\
for the file-stream ~S.",
				2, position, file_stream);
		}
		output = (file_position_set(file_stream, i) < 0)? Cnil : Ct;
	}
	@(return output)
@)

cl_object
cl_file_string_length(cl_object stream, cl_object string)
{
	cl_fixnum l;
	/* This is a stupid requirement from the spec. Why returning 1???
	 * Why not simply leaving the value unspecified, as with other
	 * streams one cannot write to???
	 */
	if (type_of(stream) == t_stream &&
	    stream->stream.mode == smm_broadcast) {
		stream = stream->stream.object0;
		if (endp(stream))
			@(return MAKE_FIXNUM(1))
	}
	switch (type_of(string)) {
	case t_string:
		l = string->string.fillp;
		break;
	case t_character:
		l = 1;
		break;
	default:
		FEwrong_type_argument(@'string', string);
	}
	@(return MAKE_FIXNUM(l))
}


cl_object
cl_file_length(cl_object strm)
{
	cl_fixnum i = file_length(strm);
	@(return ((i < 0) ? Cnil : MAKE_FIXNUM(i)))
}

cl_object
cl_open_stream_p(cl_object strm)
{
	/* ANSI and Cltl2 specify that open-stream-p should work
	   on closed streams, and that a stream is only closed
	   when #'close has been applied on it */
	if (type_of(strm) != t_stream)
		FEwrong_type_argument(@'stream', strm);
	@(return (strm->stream.mode != smm_closed ? Ct : Cnil))
}

cl_object
si_get_string_input_stream_index(cl_object strm)
{
	if ((enum ecl_smmode)strm->stream.mode != smm_string_input)
		FEerror("~S is not a string-input stream.", 1, strm);
	@(return MAKE_FIXNUM(strm->stream.int0))
}

cl_object
si_make_string_output_stream_from_string(cl_object s)
{
	@(return make_string_output_stream_from_string(s))
}

cl_object
si_copy_stream(cl_object in, cl_object out)
{
	int c;
	for (c = ecl_getc(in); c != EOF; c = ecl_getc(in)) {
		writec_stream(c, out);
	}
	flush_stream(out);
	@(return Ct)
}

cl_object
cl_interactive_stream_p(cl_object strm)
{
	cl_object output = Cnil;
	cl_type t;
 BEGIN:
	t = type_of(strm);
#ifdef ECL_CLOS_STREAMS
	if (t == t_instance)
		return funcall(2, @'ext::stream-interactive-p', strm);
#endif
	if (t != t_stream)
		FEtype_error_stream(strm);
	switch(strm->stream.mode) {
	case smm_synonym:
		strm = symbol_value(strm->stream.object0);
		goto BEGIN;
	case smm_input:
#ifdef HAVE_ISATTY
		/* Here we should check for the type of file descriptor,
		 * and whether it is connected to a tty. */
		output = Cnil;
#endif
		break;
	default:;
	}
	@(return output)
}

void
init_file(void)
{
	cl_object standard_input;
	cl_object standard_output;
	cl_object standard;
	cl_object x;

	standard_input = cl_alloc_object(t_stream);
	standard_input->stream.elttype = aet_ch;
	standard_input->stream.mode = (short)smm_input;
	standard_input->stream.file = stdin;
	standard_input->stream.object0 = @'base-char';
	standard_input->stream.object1 = make_constant_string("stdin");
	standard_input->stream.int0 = 0;
	standard_input->stream.int1 = 0;

	standard_output = cl_alloc_object(t_stream);
	standard_output->stream.elttype = aet_ch;
	standard_output->stream.mode = (short)smm_output;
	standard_output->stream.file = stdout;
	standard_output->stream.object0 = @'base-char';
	standard_output->stream.object1= make_constant_string("stdout");
	standard_output->stream.int0 = 0;
	standard_output->stream.int1 = 0;

	cl_core.terminal_io = standard
	= make_two_way_stream(standard_input, standard_output);

	ECL_SET(@'*terminal-io*', standard);

	x = cl_alloc_object(t_stream);
	x->stream.mode = (short)smm_synonym;
	x->stream.file = NULL;
	x->stream.object0 = @'*terminal-io*';
	x->stream.object1 = OBJNULL;
	x->stream.int0 = x->stream.int1 = 0;
	standard = x;

	ECL_SET(@'*standard-input*', standard);
	ECL_SET(@'*standard-output*', standard);
	ECL_SET(@'*error-output*', standard);

	ECL_SET(@'*query-io*', standard);
	ECL_SET(@'*debug-io*', standard);
	ECL_SET(@'*trace-output*', standard);
}
