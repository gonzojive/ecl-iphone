/* -*- mode: c; c-basic-offset: 8 -*- */
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

#include <ecl/ecl.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>

#ifdef HAVE_SELECT
# ifdef HAVE_SYS_SELECT_H
#  include <sys/select.h>
# endif
# include <sys/time.h>
# include <sys/types.h>
# include <unistd.h>
#elif defined(mingw32) || defined(_MSC_VER)
# include <winsock.h>
# define HAVE_SELECT
#elif defined(HAVE_SYS_IOCTL_H) && !defined(MSDOS) && !defined(cygwin)
# include <sys/ioctl.h>
#endif

#ifndef HAVE_FSEEKO
#define ecl_off_t int
#define ecl_fseeko fseek
#define ecl_ftello ftell
#else
#define ecl_off_t off_t
#define ecl_fseeko fseeko
#define ecl_ftello ftello
#endif

static cl_index ecl_read_byte8(cl_object stream, char *c, cl_index n);
static cl_index ecl_write_byte8(cl_object stream, char *c, cl_index n);

struct ecl_file_ops *duplicate_dispatch_table(const struct ecl_file_ops *ops);
const struct ecl_file_ops *stream_dispatch_table(cl_object strm);

static int flisten(FILE *);
static void io_stream_begin_write(cl_object strm);
static void io_stream_begin_read(cl_object strm);
static cl_object ecl_off_t_to_integer(ecl_off_t offset);
static ecl_off_t ecl_integer_to_off_t(cl_object offset);

static cl_object alloc_stream();

static cl_object not_a_file_stream(cl_object fn);
static void not_an_input_stream(cl_object fn);
static void not_an_output_stream(cl_object fn);
static void not_a_character_stream(cl_object s);
static void not_a_binary_stream(cl_object s);
static int restartable_io_error(cl_object strm);
static void unread_error(cl_object strm);
static void unread_twice(cl_object strm);
static void io_error(cl_object strm);
static void wrong_file_handler(cl_object strm);

/**********************************************************************
 * NOT IMPLEMENTED or NOT APPLICABLE OPERATIONS
 */

static cl_index
not_output_write_byte8(cl_object strm, char *c, cl_index n)
{
	not_an_output_stream(strm);
	return 0;
}

static cl_index
not_input_read_byte8(cl_object strm, char *c, cl_index n)
{
	not_an_input_stream(strm);
	return 0;
}

static cl_index
not_binary_write_byte8(cl_object strm, char *c, cl_index n)
{
	not_a_binary_stream(strm);
	return 0;
}

static cl_index
not_binary_read_byte8(cl_object strm, char *c, cl_index n)
{
	not_a_binary_stream(strm);
	return 0;
}

static int
not_input_read_char(cl_object strm)
{
	not_an_input_stream(strm);
	return -1;
}

static int
not_output_write_char(cl_object strm, int c)
{
	not_an_output_stream(strm);
	return c;
}

static void
not_input_unread_char(cl_object strm, int c)
{
	not_an_input_stream(strm);
}

static int
not_input_listen(cl_object strm)
{
	not_an_input_stream(strm);
	return -1;
}

static int
not_character_read_char(cl_object strm)
{
	not_a_character_stream(strm);
	return -1;
}

static int
not_character_write_char(cl_object strm, int c)
{
	not_a_character_stream(strm);
	return c;
}

static void
not_character_unread_char(cl_object strm, int c)
{
	not_a_character_stream(strm);
}

static int
not_character_listen(cl_object strm)
{
	not_a_character_stream(strm);
	return -1;
}

static void
not_input_clear_input(cl_object strm)
{
	not_an_input_stream(strm);
	return;
}

static void
not_output_clear_output(cl_object strm)
{
	not_an_output_stream(strm);
	return;
}

static void
not_output_force_output(cl_object strm)
{
	not_an_output_stream(strm);
	return;
}

static void
not_output_finish_output(cl_object strm)
{
	not_an_output_stream(strm);
	return;
}

static cl_object
not_implemented_get_position(cl_object strm)
{
	FEerror("file-position not implemented for stream ~S", 1, strm);
	return Cnil;
}

static cl_object
not_implemented_set_position(cl_object strm, cl_object pos)
{
	FEerror("file-position not implemented for stream ~S", 1, strm);
	return Cnil;
}

/**********************************************************************
 * CLOSED STREAM OPS
 */

static cl_index
closed_stream_read_byte8(cl_object strm, char *c, cl_index n)
{
	FEclosed_stream(strm);
}

static cl_index
closed_stream_write_byte8(cl_object strm, char *c, cl_index n)
{
	FEclosed_stream(strm);
}

static int
closed_stream_read_char(cl_object strm)
{
	FEclosed_stream(strm);
}

static int
closed_stream_write_char(cl_object strm, int c)
{
	FEclosed_stream(strm);
	return c;
}

static void
closed_stream_unread_char(cl_object strm, int c)
{
	FEclosed_stream(strm);
}

static int
closed_stream_listen(cl_object strm)
{
	FEclosed_stream(strm);
}

static void
closed_stream_clear_input(cl_object strm)
{
	FEclosed_stream(strm);
}

#define closed_stream_clear_output closed_stream_clear_input
#define closed_stream_force_output closed_stream_clear_input
#define closed_stream_finish_output closed_stream_clear_input

static cl_object
closed_stream_length(cl_object strm)
{
	FEclosed_stream(strm);
}

#define closed_stream_get_position closed_stream_length

static cl_object
closed_stream_set_position(cl_object strm, cl_object position)
{
	FEclosed_stream(strm);
}

/**********************************************************************
 * GENERIC OPERATIONS
 *
 * Versions of the methods which are defined in terms of others
 */
/*
 * Byte operations for devices that are character based. We assume that
 * the character size matches that of the byte.
 */
static cl_index
generic_write_byte8(cl_object strm, char *c, cl_index n)
{
	const struct ecl_file_ops *ops = stream_dispatch_table(strm);
	cl_index i;
	for (i = 0; i < n; i++) {
		ops->write_char(strm, c[i]);
	}
	return n;
}

static cl_index
generic_read_byte8(cl_object strm, char *c, cl_index n)
{
	const struct ecl_file_ops *ops = stream_dispatch_table(strm);
	cl_index i;
	for (i = 0; i < n; i++) {
		c[i] = ops->read_char(strm);
	}
	return n;
}

/*
 * Character operations for devices which are byte based. We assume that
 * the character size matches that of the byte.
 */
static int
generic_read_char(cl_object strm)
{
	int c = strm->stream.unread;
	if (c == EOF) {
		const struct ecl_file_ops *ops = stream_dispatch_table(strm);
		char aux;
		if (ops->read_byte8(strm, &aux, 1) < 1)
			c = EOF;
		else
			c = aux;
	} else {
		strm->stream.unread = EOF;
	}
	return c;
}

static int
generic_peek_char(cl_object strm)
{
	int out = ecl_read_char(strm);
	if (out != EOF) ecl_unread_char(out, strm);
	return out;
}

static int
generic_write_char(cl_object strm, int c)
{
	const struct ecl_file_ops *ops = stream_dispatch_table(strm);
	char aux = c;
	ops->write_byte8(strm, &aux, 1);
	return c;
}

static void
generic_unread_char(cl_object strm, int c)
{
	if (strm->stream.unread != EOF) {
		unread_twice(strm);
	}
	strm->stream.unread = c;
}

static void
generic_void(cl_object strm)
{
}

static int
generic_always_true(cl_object strm)
{
	return 1;
}

static int
generic_always_false(cl_object strm)
{
	return 0;
}

static cl_object
generic_always_nil(cl_object strm)
{
	return Cnil;
}

static int
generic_column(cl_object strm)
{
	return 0;
}

static cl_object
generic_set_position(cl_object strm, cl_object pos)
{
	return Cnil;
}

static cl_object
generic_close(cl_object strm)
{
	struct ecl_file_ops *ops = strm->stream.ops;
	if (ecl_input_stream_p(strm)) {
		ops->read_byte8 = closed_stream_read_byte8;
		ops->read_char = closed_stream_read_char;
		ops->unread_char = closed_stream_unread_char;
		ops->listen = closed_stream_listen;
		ops->clear_input = closed_stream_clear_input;
	}
	if (ecl_output_stream_p(strm)) {
		ops->write_byte8 = closed_stream_write_byte8;
		ops->write_char = closed_stream_write_char;
		ops->clear_output = closed_stream_clear_output;
		ops->force_output = closed_stream_force_output;
		ops->finish_output = closed_stream_finish_output;
	}
	ops->get_position = closed_stream_get_position;
	ops->set_position = closed_stream_set_position;
	ops->length = closed_stream_length;
	ops->close = generic_close;
	strm->stream.closed = 1;
	return Ct;
}

static cl_index
generic_write_vector(cl_object strm, cl_object data, cl_index start, cl_index end)
{
	if (start >= end)
		return start;
	if (data->vector.elttype == aet_bc ||
#ifdef ECL_UNICODE
	    data->vector.elttype == aet_ch ||
#endif
	    (data->vector.elttype == aet_object && CHARACTERP(ecl_elt(data, 0)))) {
		for (; start < end; start++) {
			ecl_write_char(ecl_char_code(ecl_elt(data, start)), strm);
		}
	} else {
		for (; start < end; start++) {
			ecl_write_byte(ecl_elt(data, start), strm);
		}
	}
	return start;
}

static cl_index
generic_read_vector(cl_object strm, cl_object data, cl_index start, cl_index end)
{
	cl_object expected_type;
	if (start >= end)
		return start;
	expected_type = ecl_stream_element_type(strm);
	if (expected_type == @'base-char' || expected_type == @'character') {
		for (; start < end; start++) {
			cl_fixnum c = ecl_read_char(strm);
			if (c == EOF) break;
			ecl_elt_set(data, start, CODE_CHAR(c));
		}
	} else {
		for (; start < end; start++) {
			cl_object x = ecl_read_byte(strm);
			if (Null(x)) break;
			ecl_elt_set(data, start, x);
		}
	}
	return start;
}

/********************************************************************************
 * CLOS STREAMS
 */

static cl_index
clos_stream_read_byte8(cl_object strm, char *c, cl_index n)
{
	cl_index i;
	for (i = 0; i < n; i++) {
		cl_object byte = funcall(3, @'gray::stream-read-byte', strm);
		if (!FIXNUMP(byte))
			break;
		c[i] = fix(byte);
	}
	return i;
}

static cl_index
clos_stream_write_byte8(cl_object strm, char *c, cl_index n)
{
	cl_index i;
	for (i = 0; i < n; i++) {
		cl_object byte = funcall(3, @'gray::stream-write-byte', strm,
					 MAKE_FIXNUM(c[i]));
		if (!FIXNUMP(byte))
			break;
	}
	return i;
}

static int
clos_stream_read_char(cl_object strm)
{
	cl_object output = funcall(3, @'gray::stream-read-char', strm);
	return CHAR_CODE(output);
}

static int
clos_stream_write_char(cl_object strm, int c)
{
	funcall(3, @'gray::stream-write-char', strm, CODE_CHAR(c));
	return c;
}

static void
clos_stream_unread_char(cl_object strm, int c)
{
	funcall(3, @'gray::stream-unread-char', strm, CODE_CHAR(c));
}

#define clos_stream_peek_char generic_peek_char

static int
clos_stream_listen(cl_object strm)
{
	return !Null(funcall(2, @'gray::stream-listen', strm));
}

static void
clos_stream_clear_input(cl_object strm)
{
	funcall(2, @'gray::stream-clear-input', strm);
	return;
}

static void
clos_stream_clear_output(cl_object strm)
{
	funcall(2, @'gray::stream-clear-output', strm);
	return;
}

static void
clos_stream_force_output(cl_object strm)
{
	funcall(2, @'gray::stream-force-output', strm);
	return;
}

static void
clos_stream_finish_output(cl_object strm)
{
	funcall(2, @'gray::stream-finish-output', strm);
	return;
}

static int
clos_stream_input_p(cl_object strm)
{
	return !Null(funcall(2, @'gray::input-stream-p', strm));
}

static int
clos_stream_output_p(cl_object strm)
{
	return !Null(funcall(2, @'gray::output-stream-p', strm));
}

static int
clos_stream_interactive_p(cl_object strm)
{
	return !Null(funcall(2, @'gray::stream-interactive-p', strm));

}

static cl_object
clos_stream_element_type(cl_object strm)
{
	return funcall(2, @'gray::stream-element-type', strm);
}

#define clos_stream_length not_a_file_stream
#define clos_stream_get_position not_implemented_get_position
#define clos_stream_set_position not_implemented_set_position

static int
clos_stream_column(cl_object strm)
{
	cl_object col = funcall(2, @'gray::stream-line-column', strm);
	/* FIXME! The Gray streams specifies NIL is a valid
	 * value but means "unknown". Should we make it
	 * zero? */
	return Null(col)? 0 : fixnnint(col);
}

static cl_object
clos_stream_close(cl_object strm)
{
	return funcall(2, @'gray::close', strm);
}

const struct ecl_file_ops clos_stream_ops = {
	clos_stream_write_byte8,
	clos_stream_read_byte8,

	clos_stream_read_char,
	clos_stream_write_char,
	clos_stream_unread_char,
	clos_stream_peek_char,

	generic_read_vector,
	generic_write_vector,

	clos_stream_listen,
	clos_stream_clear_input,
	clos_stream_clear_output,
	clos_stream_finish_output,
	clos_stream_force_output,

	clos_stream_input_p,
	clos_stream_output_p,
	clos_stream_interactive_p,
	clos_stream_element_type,

	clos_stream_length,
	clos_stream_get_position,
	clos_stream_set_position,
	clos_stream_column,
	clos_stream_close
};

/**********************************************************************
 * STRING OUTPUT STREAMS
 */

#define str_out_read_byte8 not_input_read_byte8
#define str_out_write_byte8 not_binary_write_byte8
#define str_out_read_char not_input_read_char
#define str_out_unread_char not_input_unread_char
#define str_out_peek_char generic_peek_char
#define str_out_listen not_input_listen

static int
str_out_write_char(cl_object strm, int c)
{
	int column = STRING_OUTPUT_COLUMN(strm);
	if (c == '\n')
		STRING_OUTPUT_COLUMN(strm) = 0;
	else if (c == '\t')
		STRING_OUTPUT_COLUMN(strm) = (column&~07) + 8;
	else
		STRING_OUTPUT_COLUMN(strm) = column+1;
	ecl_string_push_extend(STRING_OUTPUT_STRING(strm), c);
	return c;
}

#define str_out_clear_input not_input_clear_input
#define str_out_clear_output generic_void
#define str_out_force_output generic_void
#define str_out_finish_output generic_void
#define str_out_input_p generic_always_false
#define str_out_output_p generic_always_true

static cl_object
str_out_element_type(cl_object strm)
{
	cl_object string = STRING_OUTPUT_STRING(strm);
	if (type_of(string) == t_base_string)
		return @'base-char';
	return @'character';
}

#define str_out_length not_a_file_stream

static cl_object
str_out_get_position(cl_object strm)
{
	return ecl_make_unsigned_integer(STRING_OUTPUT_STRING(strm)->base_string.fillp);
}

static cl_object
str_out_set_position(cl_object strm, cl_object pos)
{
	cl_object string = STRING_OUTPUT_STRING(strm);
	cl_fixnum disp = fixnnint(pos);
	if (disp < string->base_string.fillp) {
		string->base_string.fillp = disp;
	} else {
		disp -= string->base_string.fillp;
		while (disp-- > 0)
			ecl_write_char(' ', strm);
	}
	return Ct;
}

static int
str_out_column(cl_object strm)
{
	return STRING_OUTPUT_COLUMN(strm);
}

#define str_out_close generic_close

const struct ecl_file_ops str_out_ops = {
	str_out_write_byte8,
	str_out_read_byte8,

	str_out_read_char,
	str_out_write_char,
	str_out_unread_char,
	str_out_peek_char,

	generic_read_vector,
	generic_write_vector,

	str_out_listen,
	str_out_clear_input,
	str_out_clear_output,
	str_out_finish_output,
	str_out_force_output,

	str_out_input_p,
	str_out_output_p,
	generic_always_false,
	str_out_element_type,

	str_out_length,
	str_out_get_position,
	str_out_set_position,
	str_out_column,
	str_out_close
};


cl_object
si_make_string_output_stream_from_string(cl_object s)
{
	cl_object strm = alloc_stream();

	if (type_of(s) != t_base_string || !s->base_string.hasfillp)
		FEerror("~S is not a base-string with a fill-pointer.", 1, s);
	strm->stream.ops = duplicate_dispatch_table(&str_out_ops);
	strm->stream.mode = (short)smm_string_output;
	STRING_OUTPUT_STRING(strm) = s;
	STRING_OUTPUT_COLUMN(strm) = 0;
	strm->stream.char_stream_p = 1;
	strm->stream.byte_size = 8;
	strm->stream.signed_bytes = 0;
	@(return strm)
}

cl_object
ecl_make_string_output_stream(cl_index line_length)
{
	cl_object s = cl_alloc_adjustable_base_string(line_length);
	return si_make_string_output_stream_from_string(s);
}

@(defun make-string-output-stream (&key (element_type @'base-char'))
@
	if (Null(funcall(3, @'subtypep', element_type, @'character'))) {
		FEerror("In MAKE-STRING-OUTPUT-STREAM, the argument :ELEMENT-TYPE (~A) must be a subtype of character",
			1, element_type);
	}
	@(return ecl_make_string_output_stream(128))
@)

cl_object
cl_get_output_stream_string(cl_object strm)
{
	cl_object strng;
	if (type_of(strm) != t_stream ||
	    (enum ecl_smmode)strm->stream.mode != smm_string_output)
		FEerror("~S is not a string-output stream.", 1, strm);
	strng = si_copy_to_simple_base_string(STRING_OUTPUT_STRING(strm));
	STRING_OUTPUT_STRING(strm)->base_string.fillp = 0;
	@(return strng)
}

/**********************************************************************
 * STRING INPUT STREAMS
 */

#define str_in_read_byte8 not_binary_read_byte8
#define str_in_write_byte8 not_output_write_byte8

static int
str_in_read_char(cl_object strm)
{
	int c = strm->stream.unread;
	if (c != EOF) {
		strm->stream.unread = EOF;
	} else {
		cl_fixnum curr_pos = STRING_INPUT_POSITION(strm);
		if (curr_pos >= STRING_INPUT_LIMIT(strm)) {
			c = EOF;
		} else {
			c = STRING_INPUT_STRING(strm)->base_string.self[curr_pos];
			STRING_INPUT_POSITION(strm) = curr_pos+1;
		}
	}
	return c;
}

#define str_in_write_char not_output_write_char

static void
str_in_unread_char(cl_object strm, int c)
{
	cl_fixnum curr_pos = STRING_INPUT_POSITION(strm);
	if (c <= 0) {
		unread_error(strm);
	}
	generic_unread_char(strm, c);
}

static int
str_in_peek_char(cl_object strm)
{
	cl_index pos = STRING_INPUT_POSITION(strm);
	if (pos >= STRING_INPUT_LIMIT(strm)) {
		return EOF;
	} else {
		return STRING_INPUT_STRING(strm)->base_string.self[pos];
	}
}
#define str_in_peek_char generic_peek_char

static int
str_in_listen(cl_object strm)
{
	if (strm->stream.unread != EOF)
		return ECL_LISTEN_AVAILABLE;
	if (STRING_INPUT_POSITION(strm) < STRING_INPUT_LIMIT(strm))
		return ECL_LISTEN_AVAILABLE;
	else
		return ECL_LISTEN_EOF;
}

#define str_in_clear_input generic_void
#define str_in_clear_output not_output_clear_output
#define str_in_force_output not_output_force_output
#define str_in_finish_output not_output_finish_output
#define str_in_input_p generic_always_true
#define str_in_output_p generic_always_false

static cl_object
str_in_element_type(cl_object strm)
{
	cl_object string = STRING_INPUT_STRING(strm);
	if (type_of(string) == t_base_string)
		return @'base-char';
	return @'character';
}

#define str_in_length not_a_file_stream

static cl_object
str_in_get_position(cl_object strm)
{
	return ecl_make_unsigned_integer(STRING_INPUT_POSITION(strm));
}

static cl_object
str_in_set_position(cl_object strm, cl_object pos)
{
	cl_fixnum disp = fixnnint(pos);
	if (disp >= STRING_INPUT_LIMIT(strm)) {
		disp = STRING_INPUT_LIMIT(strm);
	}
	STRING_INPUT_POSITION(strm) = disp;
	return Ct;
}

#define str_in_column generic_column
#define str_in_close generic_close

const struct ecl_file_ops str_in_ops = {
	str_in_write_byte8,
	str_in_read_byte8,

	str_in_read_char,
	str_in_write_char,
	str_in_unread_char,
	str_in_peek_char,

	generic_read_vector,
	generic_write_vector,

	str_in_listen,
	str_in_clear_input,
	str_in_clear_output,
	str_in_finish_output,
	str_in_force_output,

	str_in_input_p,
	str_in_output_p,
	generic_always_false,
	str_in_element_type,

	str_in_length,
	str_in_get_position,
	str_in_set_position,
	str_in_column,
	str_in_close
};

cl_object
ecl_make_string_input_stream(cl_object strng, cl_index istart, cl_index iend)
{
	cl_object strm;

	strm = alloc_stream();
	strm->stream.ops = duplicate_dispatch_table(&str_in_ops);
	strm->stream.mode = (short)smm_string_input;
	STRING_INPUT_STRING(strm) = strng;
	STRING_INPUT_POSITION(strm) = istart;
	STRING_INPUT_LIMIT(strm) = iend;
	strm->stream.char_stream_p = 1;
	strm->stream.byte_size = 8;
	strm->stream.signed_bytes = 0;
	return strm;
}

@(defun make_string_input_stream (strng &o istart iend)
	cl_index s, e;
@
	strng = si_coerce_to_base_string(strng);
#ifdef ECL_UNICODE
	if (type_of(strng) == t_string) {
		FEerror("Reading from extended strings is not supported: ~A",
			1, strng);
	}
#endif
	if (Null(istart))
		s = 0;
	else if (!FIXNUMP(istart) || FIXNUM_MINUSP(istart))
		goto E;
	else
		s = (cl_index)fix(istart);
	if (Null(iend))
		e = strng->base_string.fillp;
	else if (!FIXNUMP(iend) || FIXNUM_MINUSP(iend))
		goto E;
	else
		e = (cl_index)fix(iend);
	if (e > strng->base_string.fillp || s > e)
		goto E;
	@(return (ecl_make_string_input_stream(strng, s, e)))

E:
	FEerror("~S and ~S are illegal as :START and :END~%\
for the string ~S.",
		3, istart, iend, strng);
@)

/**********************************************************************
 * TWO WAY STREAM
 */

static cl_index
two_way_read_byte8(cl_object strm, char *c, cl_index n)
{
	if (strm == cl_core.terminal_io)
		ecl_force_output(TWO_WAY_STREAM_OUTPUT(cl_core.terminal_io));
	return ecl_read_byte8(TWO_WAY_STREAM_INPUT(strm), c, n);
}

static cl_index
two_way_write_byte8(cl_object strm, char *c, cl_index n)
{
	return ecl_write_byte8(TWO_WAY_STREAM_OUTPUT(strm), c, n);
}

static int
two_way_read_char(cl_object strm)
{
	return ecl_read_char(TWO_WAY_STREAM_INPUT(strm));
}

static int
two_way_write_char(cl_object strm, int c)
{
	return ecl_write_char(c, TWO_WAY_STREAM_OUTPUT(strm));
}

static void
two_way_unread_char(cl_object strm, int c)
{
	return ecl_unread_char(c, TWO_WAY_STREAM_INPUT(strm));
}

static int
two_way_peek_char(cl_object strm)
{
	return ecl_peek_char(TWO_WAY_STREAM_INPUT(strm));
}

static cl_index
two_way_read_vector(cl_object strm, cl_object data, cl_index start, cl_index n)
{
	strm = TWO_WAY_STREAM_INPUT(strm);
	return stream_dispatch_table(strm)->read_vector(strm, data, start, n);
}

static cl_index
two_way_write_vector(cl_object strm, cl_object data, cl_index start, cl_index n)
{
	strm = TWO_WAY_STREAM_OUTPUT(strm);
	return stream_dispatch_table(strm)->write_vector(strm, data, start, n);
}

static int
two_way_listen(cl_object strm)
{
	return ecl_listen_stream(TWO_WAY_STREAM_INPUT(strm));
}

static void
two_way_clear_input(cl_object strm)
{
	return ecl_clear_input(TWO_WAY_STREAM_INPUT(strm));
}

static void
two_way_clear_output(cl_object strm)
{
	return ecl_clear_output(TWO_WAY_STREAM_OUTPUT(strm));
}

static void
two_way_force_output(cl_object strm)
{
	return ecl_force_output(TWO_WAY_STREAM_OUTPUT(strm));
}

static void
two_way_finish_output(cl_object strm)
{
	return ecl_finish_output(TWO_WAY_STREAM_OUTPUT(strm));
}

#define two_way_input_p generic_always_true
#define two_way_output_p generic_always_true

static int
two_way_interactive_p(cl_object strm)
{
	return ecl_interactive_stream_p(TWO_WAY_STREAM_INPUT(strm));
}

static cl_object
two_way_element_type(cl_object strm)
{
	return ecl_stream_element_type(TWO_WAY_STREAM_INPUT(strm));
}

#define two_way_length not_a_file_stream
#define two_way_get_position generic_always_nil
#define two_way_set_position generic_set_position

static int
two_way_column(cl_object strm)
{
	return ecl_file_column(TWO_WAY_STREAM_OUTPUT(strm));
}

#define two_way_close generic_close

const struct ecl_file_ops two_way_ops = {
	two_way_write_byte8,
	two_way_read_byte8,

	two_way_read_char,
	two_way_write_char,
	two_way_unread_char,
	two_way_peek_char,

	two_way_read_vector,
	two_way_write_vector,

	two_way_listen,
	two_way_clear_input,
	two_way_clear_output,
	two_way_finish_output,
	two_way_force_output,

	two_way_input_p,
	two_way_output_p,
	two_way_interactive_p,
	two_way_element_type,

	two_way_length,
	two_way_get_position,
	two_way_set_position,
	two_way_column,
	two_way_close
};


cl_object
cl_make_two_way_stream(cl_object istrm, cl_object ostrm)
{
	cl_object strm;
	if (!ecl_input_stream_p(istrm))
		not_an_input_stream(istrm);
	if (!ecl_output_stream_p(ostrm))
		not_an_output_stream(ostrm);
	strm = alloc_stream();
	strm->stream.mode = (short)smm_two_way;
	strm->stream.ops = duplicate_dispatch_table(&two_way_ops);
	TWO_WAY_STREAM_INPUT(strm) = istrm;
	TWO_WAY_STREAM_OUTPUT(strm) = ostrm;
	@(return strm)
}

cl_object
cl_two_way_stream_input_stream(cl_object strm)
{
	if (type_of(strm) != t_stream || strm->stream.mode != smm_two_way)
		FEwrong_type_argument(@'two-way-stream', strm);
	@(return TWO_WAY_STREAM_INPUT(strm))
}

cl_object
cl_two_way_stream_output_stream(cl_object strm)
{
	if (type_of(strm) != t_stream || strm->stream.mode != smm_two_way)
		FEwrong_type_argument(@'two-way-stream', strm);
	@(return TWO_WAY_STREAM_OUTPUT(strm))
}

/**********************************************************************
 * BROADCAST STREAM
 */

#define broadcast_read_byte8 not_input_read_byte8

static cl_index
broadcast_write_byte8(cl_object strm, char *c, cl_index n)
{
	cl_object l;
	cl_index out = n;
	for (l = BROADCAST_STREAM_LIST(strm); !ecl_endp(l); l = ECL_CONS_CDR(l)) {
		out = ecl_write_byte8(ECL_CONS_CAR(l), c, n);
	}
	return out;
}

#define broadcast_read_char not_input_read_char

static int
broadcast_write_char(cl_object strm, int c)
{
	cl_object l;
	for (l = BROADCAST_STREAM_LIST(strm); !ecl_endp(l); l = ECL_CONS_CDR(l)) {
		ecl_write_char(c, ECL_CONS_CAR(l));
	}
	return c;
}

#define broadcast_unread_char not_input_unread_char
#define broadcast_peek_char not_input_read_char
#define broadcast_listen not_input_listen

/* FIXME! This is legacy behaviour */
#define broadcast_clear_input broadcast_force_output

static void
broadcast_clear_output(cl_object strm)
{
	cl_object l;
	for (l = BROADCAST_STREAM_LIST(strm); !ecl_endp(l); l = ECL_CONS_CDR(l)) {
		ecl_clear_output(ECL_CONS_CAR(l));
	}
}

static void
broadcast_force_output(cl_object strm)
{
	cl_object l;
	for (l = BROADCAST_STREAM_LIST(strm); !ecl_endp(l); l = ECL_CONS_CDR(l)) {
		ecl_force_output(ECL_CONS_CAR(l));
	}
}

static void
broadcast_finish_output(cl_object strm)
{
	cl_object l;
	for (l = BROADCAST_STREAM_LIST(strm); !ecl_endp(l); l = ECL_CONS_CDR(l)) {
		ecl_finish_output(ECL_CONS_CAR(l));
	}
}

#define broadcast_input_p generic_always_false
#define broadcast_output_p generic_always_true

static cl_object
broadcast_element_type(cl_object strm)
{
	cl_object l = BROADCAST_STREAM_LIST(strm);
	if (Null(l))
		return Ct;
	return ecl_stream_element_type(ECL_CONS_CAR(l));
}

static cl_object
broadcast_length(cl_object strm)
{
	cl_object l = BROADCAST_STREAM_LIST(strm);
	if (Null(l))
		return MAKE_FIXNUM(0);
	return ecl_file_length(ECL_CONS_CAR(l));
}

static cl_object
broadcast_get_position(cl_object strm)
{
	cl_object l = BROADCAST_STREAM_LIST(strm);
	if (Null(l))
		return MAKE_FIXNUM(0);
	return ecl_file_position(ECL_CONS_CAR(l));
}

static cl_object
broadcast_set_position(cl_object strm, cl_object pos)
{
	cl_object l = BROADCAST_STREAM_LIST(strm);
	if (Null(l))
		return Cnil;
	return ecl_file_position_set(ECL_CONS_CAR(l), pos);
}

static int
broadcast_column(cl_object strm)
{
	cl_object l = BROADCAST_STREAM_LIST(strm);
	if (Null(l))
		return 0;
	return ecl_file_column(ECL_CONS_CAR(l));
}

#define broadcast_close generic_close

const struct ecl_file_ops broadcast_ops = {
	broadcast_write_byte8,
	broadcast_read_byte8,

	broadcast_read_char,
	broadcast_write_char,
	broadcast_unread_char,
	broadcast_peek_char,

	generic_read_vector,
	generic_write_vector,

	broadcast_listen,
	broadcast_clear_input,
	broadcast_clear_output,
	broadcast_finish_output,
	broadcast_force_output,

	broadcast_input_p,
	broadcast_output_p,
	generic_always_false,
	broadcast_element_type,

	broadcast_length,
	broadcast_get_position,
	broadcast_set_position,
	broadcast_column,
	broadcast_close
};

@(defun make_broadcast_stream (&rest ap)
	cl_object x, streams;
	int i;
@
	streams = Cnil;
	for (i = 0; i < narg; i++) {
		x = cl_va_arg(ap);
		if (!ecl_output_stream_p(x))
			not_an_output_stream(x);
		streams = CONS(x, streams);
	}
	x = alloc_stream();
	x->stream.ops = duplicate_dispatch_table(&broadcast_ops);
	x->stream.mode = (short)smm_broadcast;
	BROADCAST_STREAM_LIST(x) = cl_nreverse(streams);
	@(return x)
@)

cl_object
cl_broadcast_stream_streams(cl_object strm)
{
	if (type_of(strm) != t_stream || strm->stream.mode != smm_broadcast)
		FEwrong_type_argument(@'broadcast-stream', strm);
	return cl_copy_list(BROADCAST_STREAM_LIST(strm));
}

/**********************************************************************
 * ECHO STREAM
 */

static cl_index
echo_read_byte8(cl_object strm, char *c, cl_index n)
{
	cl_index out = ecl_read_byte8(ECHO_STREAM_INPUT(strm), c, n);
	return ecl_write_byte8(ECHO_STREAM_OUTPUT(strm), c, out);
}

static cl_index
echo_write_byte8(cl_object strm, char *c, cl_index n)
{
	return ecl_write_byte8(ECHO_STREAM_OUTPUT(strm), c, n);
}

static int
echo_read_char(cl_object strm)
{
	int c = strm->stream.unread;
	if (c == EOF) {
		c = ecl_read_char(ECHO_STREAM_INPUT(strm));
		if (c != EOF)
			ecl_write_char(c, ECHO_STREAM_OUTPUT(strm));
	} else {
		strm->stream.unread = EOF;
	}
	return c;
}

static int
echo_write_char(cl_object strm, int c)
{
	return ecl_write_char(c, ECHO_STREAM_OUTPUT(strm));
}

#define echo_unread_char generic_unread_char

static int
echo_peek_char(cl_object strm)
{
	int c = strm->stream.unread;
	if (c == EOF) {
		c = ecl_peek_char(ECHO_STREAM_INPUT(strm));
	}
	return c;
}

static int
echo_listen(cl_object strm)
{
	return ecl_listen_stream(ECHO_STREAM_INPUT(strm));
}

static void
echo_clear_input(cl_object strm)
{
	return ecl_clear_input(ECHO_STREAM_INPUT(strm));
}

static void
echo_clear_output(cl_object strm)
{
	return ecl_clear_output(ECHO_STREAM_OUTPUT(strm));
}

static void
echo_force_output(cl_object strm)
{
	return ecl_force_output(ECHO_STREAM_OUTPUT(strm));
}

static void
echo_finish_output(cl_object strm)
{
	return ecl_finish_output(ECHO_STREAM_OUTPUT(strm));
}

#define echo_input_p generic_always_true
#define echo_output_p generic_always_true

static cl_object
echo_element_type(cl_object strm)
{
	return ecl_stream_element_type(ECHO_STREAM_INPUT(strm));
}

#define echo_length not_a_file_stream
#define echo_get_position generic_always_nil
#define echo_set_position generic_set_position

static int
echo_column(cl_object strm)
{
	return ecl_file_column(ECHO_STREAM_OUTPUT(strm));
}

#define echo_close generic_close

const struct ecl_file_ops echo_ops = {
	echo_write_byte8,
	echo_read_byte8,

	echo_read_char,
	echo_write_char,
	echo_unread_char,
	echo_peek_char,

	generic_read_vector,
	generic_write_vector,

	echo_listen,
	echo_clear_input,
	echo_clear_output,
	echo_finish_output,
	echo_force_output,

	echo_input_p,
	echo_output_p,
	generic_always_false,
	echo_element_type,

	echo_length,
	echo_get_position,
	echo_set_position,
	echo_column,
	echo_close
};

cl_object
cl_make_echo_stream(cl_object strm1, cl_object strm2)
{
	cl_object strm;
	if (!ecl_input_stream_p(strm1))
		not_an_input_stream(strm1);
	if (!ecl_output_stream_p(strm2))
		not_an_output_stream(strm2);
	strm = alloc_stream();
	strm->stream.mode = (short)smm_echo;
	strm->stream.ops = duplicate_dispatch_table(&echo_ops);
	ECHO_STREAM_INPUT(strm) = strm1;
	ECHO_STREAM_OUTPUT(strm) = strm2;
	@(return strm)
}

cl_object
cl_echo_stream_input_stream(cl_object strm)
{
	if (type_of(strm) != t_stream || strm->stream.mode != smm_echo)
		FEwrong_type_argument(@'echo-stream', strm);
	@(return ECHO_STREAM_INPUT(strm))
}

cl_object
cl_echo_stream_output_stream(cl_object strm)
{
	if (type_of(strm) != t_stream || strm->stream.mode != smm_echo)
		FEwrong_type_argument(@'echo-stream', strm);
	@(return ECHO_STREAM_OUTPUT(strm))
}

/**********************************************************************
 * CONCATENATED STREAM
 */

static cl_index
concatenated_read_byte8(cl_object strm, char *c, cl_index n)
{
	cl_object l = CONCATENATED_STREAM_LIST(strm);
	cl_index out = 0;
	while (out < n && !ecl_endp(l)) {
		cl_index left = n - out;
		cl_index delta = ecl_read_byte8(ECL_CONS_CAR(l), c + out, n - out);
		out += delta;
		if (out == n) break;
		CONCATENATED_STREAM_LIST(strm) = l = ECL_CONS_CDR(l);
	}
	return out;
}

#define concatenated_write_byte8 not_output_write_byte8

static int
concatenated_read_char(cl_object strm)
{
	cl_object l = CONCATENATED_STREAM_LIST(strm);
	int c = EOF;
	while (!ecl_endp(l)) {
		c = ecl_read_char(ECL_CONS_CAR(l));
		if (c != EOF) break;
		CONCATENATED_STREAM_LIST(strm) = l = ECL_CONS_CDR(l);
	}
	return c;
}

#define concatenated_write_char not_output_write_char

static void
concatenated_unread_char(cl_object strm, int c)
{
	cl_object l = CONCATENATED_STREAM_LIST(strm);
	if (Null(l))
		unread_error(strm);
	return ecl_unread_char(c, ECL_CONS_CAR(l));
}

#define concatenated_peek_char generic_peek_char

static int
concatenated_listen(cl_object strm)
{
	cl_object l = CONCATENATED_STREAM_LIST(strm);
	while (!ecl_endp(l)) {
		int f = ecl_listen_stream(ECL_CONS_CAR(l));
		l = ECL_CONS_CDR(l);
		if (f == ECL_LISTEN_EOF) {
			CONCATENATED_STREAM_LIST(strm) = l;
		} else {
			return f;
		}
	}
	return ECL_LISTEN_EOF;
}

#define concatenated_clear_input generic_void
#define concatenated_clear_output not_output_clear_output
#define concatenated_force_output not_output_force_output
#define concatenated_finish_output not_output_finish_output

#define concatenated_input_p generic_always_true
#define concatenated_output_p generic_always_false
#define concatenated_element_type broadcast_element_type

#define concatenated_length not_a_file_stream
#define concatenated_get_position generic_always_nil
#define concatenated_set_position generic_set_position
#define concatenated_column generic_column

#define concatenated_close generic_close

const struct ecl_file_ops concatenated_ops = {
	concatenated_write_byte8,
	concatenated_read_byte8,

	concatenated_read_char,
	concatenated_write_char,
	concatenated_unread_char,
	concatenated_peek_char,

	generic_read_vector,
	generic_write_vector,

	concatenated_listen,
	concatenated_clear_input,
	concatenated_clear_output,
	concatenated_finish_output,
	concatenated_force_output,

	concatenated_input_p,
	concatenated_output_p,
	generic_always_false,
	concatenated_element_type,

	concatenated_length,
	concatenated_get_position,
	concatenated_set_position,
	concatenated_column,
	concatenated_close
};

@(defun make_concatenated_stream (&rest ap)
	cl_object x, streams;
	int i;
@
	streams = Cnil;
	for (i = 0; i < narg; i++) {
		x = cl_va_arg(ap);
		if (!ecl_input_stream_p(x))
			not_an_input_stream(x);
		streams = CONS(x, streams);
	}
	x = alloc_stream();
	x->stream.mode = (short)smm_concatenated;
	x->stream.ops = duplicate_dispatch_table(&concatenated_ops);
	CONCATENATED_STREAM_LIST(x) = cl_nreverse(streams);
	@(return x)
@)

cl_object
cl_concatenated_stream_streams(cl_object strm)
{
	if (type_of(strm) != t_stream || strm->stream.mode != smm_concatenated)
		FEwrong_type_argument(@'concatenated-stream', strm);
	return cl_copy_list(CONCATENATED_STREAM_LIST(strm));
}

/**********************************************************************
 * SYNONYM STREAM
 */

static cl_index
synonym_read_byte8(cl_object strm, char *c, cl_index n)
{
	return ecl_read_byte8(SYNONYM_STREAM_STREAM(strm), c, n);
}

static cl_index
synonym_write_byte8(cl_object strm, char *c, cl_index n)
{
	return ecl_write_byte8(SYNONYM_STREAM_STREAM(strm), c, n);
}

static int
synonym_read_char(cl_object strm)
{
	return ecl_read_char(SYNONYM_STREAM_STREAM(strm));
}

static int
synonym_write_char(cl_object strm, int c)
{
	return ecl_write_char(c, SYNONYM_STREAM_STREAM(strm));
}

static void
synonym_unread_char(cl_object strm, int c)
{
	return ecl_unread_char(c, SYNONYM_STREAM_STREAM(strm));
}

static int
synonym_peek_char(cl_object strm)
{
	return ecl_peek_char(SYNONYM_STREAM_STREAM(strm));
}

static cl_index
synonym_read_vector(cl_object strm, cl_object data, cl_index start, cl_index n)
{
	strm = SYNONYM_STREAM_STREAM(strm);
	return stream_dispatch_table(strm)->read_vector(strm, data, start, n);
}

static cl_index
synonym_write_vector(cl_object strm, cl_object data, cl_index start, cl_index n)
{
	strm = SYNONYM_STREAM_STREAM(strm);
	return stream_dispatch_table(strm)->write_vector(strm, data, start, n);
}

static int
synonym_listen(cl_object strm)
{
	return ecl_listen_stream(SYNONYM_STREAM_STREAM(strm));
}

static void
synonym_clear_input(cl_object strm)
{
	return ecl_clear_input(SYNONYM_STREAM_STREAM(strm));
}

static void
synonym_clear_output(cl_object strm)
{
	return ecl_clear_output(SYNONYM_STREAM_STREAM(strm));
}

static void
synonym_force_output(cl_object strm)
{
	return ecl_force_output(SYNONYM_STREAM_STREAM(strm));
}

static void
synonym_finish_output(cl_object strm)
{
	return ecl_finish_output(SYNONYM_STREAM_STREAM(strm));
}

static int
synonym_input_p(cl_object strm)
{
	return ecl_input_stream_p(SYNONYM_STREAM_STREAM(strm));
}

static int
synonym_output_p(cl_object strm)
{
	return ecl_output_stream_p(SYNONYM_STREAM_STREAM(strm));
}

static int
synonym_interactive_p(cl_object strm)
{
	return ecl_interactive_stream_p(SYNONYM_STREAM_STREAM(strm));
}

static cl_object
synonym_element_type(cl_object strm)
{
	return ecl_stream_element_type(SYNONYM_STREAM_STREAM(strm));
}

static cl_object
synonym_length(cl_object strm)
{
	return ecl_file_length(SYNONYM_STREAM_STREAM(strm));
}

static cl_object
synonym_get_position(cl_object strm)
{
	return ecl_file_position(SYNONYM_STREAM_STREAM(strm));
}

static cl_object
synonym_set_position(cl_object strm, cl_object pos)
{
	return ecl_file_position_set(SYNONYM_STREAM_STREAM(strm), pos);
}

static int
synonym_column(cl_object strm)
{
	return ecl_file_column(SYNONYM_STREAM_STREAM(strm));
}

#define synonym_close generic_close

const struct ecl_file_ops synonym_ops = {
	synonym_write_byte8,
	synonym_read_byte8,

	synonym_read_char,
	synonym_write_char,
	synonym_unread_char,
	synonym_peek_char,

	synonym_read_vector,
	synonym_write_vector,

	synonym_listen,
	synonym_clear_input,
	synonym_clear_output,
	synonym_finish_output,
	synonym_force_output,

	synonym_input_p,
	synonym_output_p,
	synonym_interactive_p,
	synonym_element_type,

	synonym_length,
	synonym_get_position,
	synonym_set_position,
	synonym_column,
	synonym_close
};

cl_object
cl_make_synonym_stream(cl_object sym)
{
	cl_object x;

	sym = ecl_check_cl_type(@'make-synonym-stream',sym,t_symbol);
	x = alloc_stream();
	x->stream.ops = duplicate_dispatch_table(&synonym_ops);
	x->stream.mode = (short)smm_synonym;
	SYNONYM_STREAM_SYMBOL(x) = sym;
	@(return x)
}

cl_object
cl_synonym_stream_symbol(cl_object strm)
{
	if (type_of(strm) != t_stream || strm->stream.mode != smm_synonym)
		FEwrong_type_argument(@'synonym-stream', strm);
	@(return SYNONYM_STREAM_SYMBOL(strm))
}

/**********************************************************************
 * TWO WAY STREAM
 */

static cl_index
io_stream_read_byte8(cl_object strm, char *c, cl_index n)
{
	FILE *f = IO_STREAM_FILE(strm);
	cl_index out;
	if (strm->stream.mode == smm_io)
		io_stream_begin_write(strm);
	ecl_disable_interrupts();
	do {
		out = fread(c, sizeof(char), n, IO_STREAM_FILE(strm));
	} while (out < n && ferror(f) && restartable_io_error(strm));
	ecl_enable_interrupts();
	return out;
}

static cl_index
io_stream_write_byte8(cl_object strm, char *c, cl_index n)
{
	FILE *f = IO_STREAM_FILE(strm);
	cl_index out;
	if (strm->stream.mode == smm_io)
		io_stream_begin_write(strm);
	ecl_disable_interrupts();
	do {
		out = fwrite(c, sizeof(char), n, IO_STREAM_FILE(strm));
	} while (out < n && restartable_io_error(strm));
	ecl_enable_interrupts();
	return out;
}

static int
io_stream_read_char(cl_object strm)
{
	int c = strm->stream.unread;
	if (c != EOF) {
		strm->stream.unread = EOF;
	} else {
		FILE *f = IO_STREAM_FILE(strm);
		char aux;
		ecl_disable_interrupts();
		do {
			c = getc(f);
		} while ((c == EOF) && ferror(f) && restartable_io_error(strm));
		ecl_enable_interrupts();
	}
	return c;
}

static int
io_stream_write_char(cl_object strm, int c)
{
	FILE *f = IO_STREAM_FILE(strm);
	char aux = c;
	int outcome;
	strm->stream.unread = EOF;
	if (c == '\n')
		IO_STREAM_COLUMN(strm) = 0;
	else if (c == '\t')
		IO_STREAM_COLUMN(strm) = (IO_STREAM_COLUMN(strm)&~07) + 8;
	else
		IO_STREAM_COLUMN(strm)++;
	ecl_disable_interrupts();
	do {
		outcome = putc(c, f);
	} while (outcome == EOF && restartable_io_error(strm));
	ecl_enable_interrupts();
	return c;
}

static void
io_stream_unread_char(cl_object strm, int c)
{
	if (strm->stream.unread != EOF) {
		unread_twice(strm);
	}
	strm->stream.unread = c;
}

#define io_stream_peek_char generic_peek_char

static int
io_stream_listen(cl_object strm)
{
	FILE *f = IO_STREAM_FILE(strm);
	return flisten(f);
}

static void
io_stream_clear_input(cl_object strm)
{
	FILE *f = IO_STREAM_FILE(strm);
#if defined(mingw32) || defined(_MSC_VER)
	if (isatty(fileno(fp))) {
		/* Flushes Win32 console */
		if (!FlushConsoleInputBuffer((HANDLE)_get_osfhandle(fileno(fp))))
			FEwin32_error("FlushConsoleInputBuffer() failed", 0);
		/* Do not stop here: the FILE structure needs also to be flushed */
	}
#endif
	while (flisten(f) == ECL_LISTEN_AVAILABLE) {
		ecl_disable_interrupts();
		getc(f);
		ecl_enable_interrupts();
	}
}

#define io_stream_clear_output generic_void

static void
io_stream_force_output(cl_object strm)
{
	FILE *f = IO_STREAM_FILE(strm);
	ecl_disable_interrupts();
	while ((fflush(f) == EOF) && restartable_io_error(strm))
		(void)0;
	ecl_enable_interrupts();
}

#define io_stream_finish_output generic_void
#define io_stream_input_p generic_always_true
#define io_stream_output_p generic_always_true

static int
io_stream_interactive_p(cl_object strm)
{
	FILE *f = IO_STREAM_FILE(strm);
	return isatty(fileno(f));
}

static cl_object
io_stream_element_type(cl_object strm)
{
	cl_object output;
	if (strm->stream.char_stream_p) {
		output = @'base-char';
	} else {
		cl_fixnum bs = strm->stream.byte_size;
		output = strm->stream.signed_bytes?
			@'signed-byte' : @'unsigned-byte';
		if (bs != 8)
			output = cl_list(2, output, MAKE_FIXNUM(bs));
	}
	return output;
}

static cl_object
io_stream_length(cl_object strm)
{
	FILE *f = IO_STREAM_FILE(strm);
	cl_object output = ecl_file_len(f);
	if (strm->stream.byte_size != 8) {
		cl_index bs = strm->stream.byte_size;
		output = ecl_floor2(output, MAKE_FIXNUM(bs/8));
		if (VALUES(1) != MAKE_FIXNUM(0)) {
			FEerror("File length is not on byte boundary", 0);
		}
	}
	return output;
}

static cl_object
io_stream_get_position(cl_object strm)
{
	FILE *f = IO_STREAM_FILE(strm);
	cl_object output;
	ecl_off_t offset;

	ecl_disable_interrupts();
	offset = ecl_ftello(f);
	ecl_enable_interrupts();
	if (offset < 0)
		io_error(strm);
	if (sizeof(ecl_off_t) == sizeof(long)) {
		output = ecl_make_integer(offset);
	} else {
		output = ecl_off_t_to_integer(offset);
	}
	if (strm->stream.byte_size != 8) {
		output = ecl_floor2(output, MAKE_FIXNUM(strm->stream.byte_size / 8));
	}
	return output;
}

static cl_object
io_stream_set_position(cl_object strm, cl_object large_disp)
{
	FILE *f = IO_STREAM_FILE(strm);
	ecl_off_t disp;
	cl_object output;
	if (strm->stream.byte_size != 8) {
		large_disp = ecl_times(large_disp,
				       MAKE_FIXNUM(strm->stream.byte_size / 8));
	}
	disp = ecl_integer_to_off_t(large_disp);
	ecl_disable_interrupts();
	if (ecl_fseeko(f, disp, 0) != 0)
		output = Cnil;
	else
		output = Ct;
	ecl_enable_interrupts();
	return output;
}

static int
io_stream_column(cl_object strm)
{
	return IO_STREAM_COLUMN(strm);
}

static cl_object
io_stream_close(cl_object strm)
{
	FILE *f = IO_STREAM_FILE(strm);
	int failed;
	if (f == stdout)
		FEerror("Cannot close the standard output", 0);
	if (f == stdin)
		FEerror("Cannot close the standard input", 0);
	if (f == NULL)
		wrong_file_handler(strm);
	if (ecl_output_stream_p(strm)) {
		ecl_force_output(strm);
	}
	ecl_disable_interrupts();
	failed = fclose(f);
	ecl_enable_interrupts();
	if (failed)
		FElibc_error("Cannot close stream ~S.", 1, strm);
#if !defined(GBC_BOEHM)
	ecl_dealloc(strm->stream.buffer);
	strm->stream.file = NULL;
#endif
	return generic_close(strm);
}

/*
 * Specialized sequence operations
 */

static cl_index
io_stream_read_vector(cl_object strm, cl_object data, cl_index start, cl_index end)
{
	cl_elttype t = data->vector.elttype;
	if (start >= end)
		return start;
	if (t == aet_b8 || t == aet_i8 || t == aet_bc) {
		if (strm->stream.byte_size == 8) {
			void *aux = data->vector.self.ch + start;
			return io_stream_read_byte8(strm, aux, end-start);
		}
	}
	if (t == aet_fix || t == aet_index) {
		if (strm->stream.byte_size == sizeof(cl_fixnum)*8) {
			void *aux = data->vector.self.fix + start;
			cl_index bytes = (end - start) * sizeof(cl_fixnum);
			bytes = io_stream_read_byte8(strm, aux, bytes);
			return start + bytes / sizeof(cl_fixnum);
		}
	}
	return generic_read_vector(strm, data, start, end);
}

static cl_index
io_stream_write_vector(cl_object strm, cl_object data, cl_index start, cl_index end)
{
	cl_elttype t = data->vector.elttype;
	if (start >= end)
		return start;
	if (t == aet_b8 || t == aet_i8 || t == aet_bc) {
		if (strm->stream.byte_size == 8) {
			void *aux = data->vector.self.fix + start;
			return io_stream_write_byte8(strm, aux, end-start);
		}
	}
	if (t == aet_fix || t == aet_index) {
		if (strm->stream.byte_size == sizeof(cl_fixnum)*8) {
			void *aux = data->vector.self.fix + start;
			cl_index bytes = (end - start) * sizeof(cl_fixnum);
			bytes = io_stream_write_byte8(strm, aux, bytes);
			return start + bytes / sizeof(cl_fixnum);
		}
	}
	return generic_write_vector(strm, data, start, end);
}

const struct ecl_file_ops io_stream_ops = {
	io_stream_write_byte8,
	io_stream_read_byte8,

	io_stream_read_char,
	io_stream_write_char,
	io_stream_unread_char,
	io_stream_peek_char,

	io_stream_read_vector,
	io_stream_write_vector,

	io_stream_listen,
	io_stream_clear_input,
	io_stream_clear_output,
	io_stream_finish_output,
	io_stream_force_output,

	io_stream_input_p,
	io_stream_output_p,
	io_stream_interactive_p,
	io_stream_element_type,

	io_stream_length,
	io_stream_get_position,
	io_stream_set_position,
	io_stream_column,
	io_stream_close
};

const struct ecl_file_ops output_stream_ops = {
	io_stream_write_byte8,
	not_input_read_byte8,

	not_input_read_char,
	io_stream_write_char,
	not_input_unread_char,
	not_input_read_char,

	generic_read_vector,
	io_stream_write_vector,

	not_input_listen,
	generic_void,
	io_stream_clear_output,
	io_stream_finish_output,
	io_stream_force_output,

	generic_always_false,
	io_stream_output_p,
	generic_always_false,
	io_stream_element_type,

	io_stream_length,
	io_stream_get_position,
	io_stream_set_position,
	io_stream_column,
	io_stream_close
};

const struct ecl_file_ops input_stream_ops = {
	not_output_write_byte8,
	io_stream_read_byte8,

	io_stream_read_char,
	not_output_write_char,
	io_stream_unread_char,
	io_stream_peek_char,

	io_stream_read_vector,
	generic_write_vector,

	io_stream_listen,
	io_stream_clear_input,
	generic_void,
	generic_void,
	generic_void,

	io_stream_input_p,
	generic_always_false,
	io_stream_interactive_p,
	io_stream_element_type,

	io_stream_length,
	io_stream_get_position,
	io_stream_set_position,
	generic_column,
	io_stream_close
};

cl_object
si_set_buffering_mode(cl_object stream, cl_object buffer_mode_symbol)
{
	enum ecl_smmode mode = stream->stream.mode;
	int buffer_mode;

	if (type_of(stream) != t_stream) {
		FEerror("Cannot set buffer of ~A", 1, stream);
	}
	if (buffer_mode_symbol == Cnil) {
		buffer_mode = _IONBF;
	} else if (buffer_mode_symbol == Ct || buffer_mode_symbol == @':fully-buffered') {
		buffer_mode = _IOFBF;
	} else if (buffer_mode_symbol == @':line-buffered') {
		buffer_mode = _IOLBF;
	} else {
		FEerror("Not a valid buffering mode: ~A", 1, buffer_mode_symbol);
	}
	if (mode == smm_output || mode == smm_io || mode == smm_input) {
		FILE *fp = (FILE*)stream->stream.file;
		char *new_buffer = 0;
		setvbuf(fp, 0, _IONBF, 0);
		if (buffer_mode != _IONBF) {
			cl_index buffer_size = BUFSIZ;
			char *new_buffer = ecl_alloc_atomic(buffer_size);
			stream->stream.buffer = new_buffer;
			setvbuf(fp, new_buffer, buffer_mode, buffer_size);
		}
	}
	@(return stream)
}

cl_object
ecl_make_stream_from_FILE(cl_object fname, void *f, enum ecl_smmode smm,
			  cl_fixnum byte_size, int char_stream_p)
{
	cl_object stream;
	stream = alloc_stream();
	stream->stream.mode = (short)smm;
	stream->stream.closed = 0;
#if defined (ECL_WSOCK)
	if (smm == smm_input_wsock || smm == smm_io_wsock)
		character_p = 1;
#endif
	switch (smm) {
	case smm_io:
		stream->stream.ops = duplicate_dispatch_table(&io_stream_ops);
		break;
	case smm_probe:
	case smm_input:
		stream->stream.ops = duplicate_dispatch_table(&input_stream_ops);
		break;
	case smm_output:
		stream->stream.ops = duplicate_dispatch_table(&output_stream_ops);
		break;
	default:
		FEerror("Not a valid mode ~D for ecl_make_stream_from_FILE", 1, MAKE_FIXNUM(smm));
	}
	if (char_stream_p) {
		if (byte_size != 8) {
			FEerror("Cannot create a character stream when byte size is not 8.", 0);
		}
		IO_STREAM_ELT_TYPE(stream) = @'base-char';
 	} else {
		IO_STREAM_ELT_TYPE(stream) = Cnil;
	}
	stream->stream.char_stream_p = char_stream_p;
	if (byte_size > 0) {
		stream->stream.signed_bytes = 0;
	} else {
		byte_size = -byte_size;
		stream->stream.signed_bytes = 1;
	}
	stream->stream.byte_size = (byte_size+7)&(~(cl_fixnum)7);
	IO_STREAM_FILENAME(stream) = fname; /* not really used */
	IO_STREAM_COLUMN(stream) = 0;
	stream->stream.file = f;
	stream->stream.last_op = 0;
	si_set_finalizer(stream, Ct);
	return stream;
}

cl_object
ecl_make_stream_from_fd(cl_object fname, int fd, enum ecl_smmode smm,
			cl_fixnum byte_size, int char_stream_p)
{
	char *mode;			/* file open mode */
	FILE *fp;			/* file pointer */
	switch(smm) {
	case smm_input:
		mode = "r";
		break;
	case smm_output:
		mode = "w";
		break;
	case smm_io:
		mode = "w+";
		break;
#if defined(ECL_WSOCK)
	case smm_input_wsock:
	case smm_output_wsock:
	case smm_io_wsock:
		break;
#endif
	default:
		FEerror("make_stream: wrong mode", 0);
	}
	ecl_disable_interrupts();
#if defined(ECL_WSOCK)
	if (smm == smm_input_wsock || smm == smm_output_wsock || smm == smm_io_wsock)
		fp = (FILE*)fd;
	else
		fp = fdopen(fd, mode);
#else
	fp = fdopen(fd, mode);
#endif
	ecl_enable_interrupts();
	return ecl_make_stream_from_FILE(fname, fp, smm, byte_size, char_stream_p);
}

int
ecl_stream_to_handle(cl_object s, bool output)
{
	FILE *f;
 BEGIN:
	if (type_of(s) != t_stream)
		return -1;
	switch ((enum ecl_smmode)s->stream.mode) {
	case smm_input:
		if (output) return -1;
		f = (FILE*)s->stream.file;
		break;
	case smm_output:
		if (!output) return -1;
		f = (FILE*)s->stream.file;
		break;
	case smm_io:
		f = (FILE*)s->stream.file;
		break;
	case smm_synonym:
		s = SYNONYM_STREAM_STREAM(s);
		goto BEGIN;
	case smm_two_way:
		s = output? TWO_WAY_STREAM_OUTPUT(s) : TWO_WAY_STREAM_INPUT(s);
		goto BEGIN;
	default:
		ecl_internal_error("illegal stream mode");
	}
	return fileno(f);
}

/**********************************************************************
 * MEDIUM LEVEL INTERFACE
 */

struct ecl_file_ops *
duplicate_dispatch_table(const struct ecl_file_ops *ops)
{
	struct ecl_file_ops *new_ops = ecl_alloc_atomic(sizeof(*ops));
	*new_ops = *ops;
	return new_ops;
}

const struct ecl_file_ops *
stream_dispatch_table(cl_object strm)
{
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm)) {
		return &clos_stream_ops;
	}
#endif
	if (type_of(strm) != t_stream)
		FEtype_error_stream(strm);
	return (const struct ecl_file_ops *)strm->stream.ops;
}

static cl_index
ecl_read_byte8(cl_object strm, char *c, cl_index n)
{
	return stream_dispatch_table(strm)->read_byte8(strm, c, n);
}

static cl_index
ecl_write_byte8(cl_object strm, char *c, cl_index n)
{
	return stream_dispatch_table(strm)->write_byte8(strm, c, n);
}

int
ecl_read_char(cl_object strm)
{
	return stream_dispatch_table(strm)->read_char(strm);
}

int
ecl_read_char_noeof(cl_object strm)
{
	int c = ecl_read_char(strm);
	if (c == EOF)
		FEend_of_file(strm);
	return c;
}

cl_object
ecl_read_byte(cl_object strm)
{
	cl_index (*read_byte8)(cl_object, char *, cl_index);
	cl_index bs;
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm)) {
		return funcall(2, @'gray::stream-read-byte', strm);
	}
#endif
	read_byte8 = stream_dispatch_table(strm)->read_byte8;
	bs = strm->stream.byte_size;
	if (bs == 8) {
		unsigned char c;
		if (read_byte8(strm, (char*)&c, 1) < 1)
			return Cnil;
		if (strm->stream.signed_bytes) {
			return MAKE_FIXNUM((signed char)c);
		} else {
			return MAKE_FIXNUM((unsigned char)c);
		}
	} else {
		unsigned char c;
		cl_index nb;
		cl_object output = MAKE_FIXNUM(0);
		for (nb = 0; bs >= 8; bs -= 8, nb += 8) {
			cl_object aux;
			if (read_byte8(strm, (char*)&c, 1) < 1)
				return Cnil;
			if (bs <= 8 && strm->stream.signed_bytes)
				aux = MAKE_FIXNUM((signed char)c);
			else
				aux = MAKE_FIXNUM((unsigned char)c);
			output = cl_logior(2, output, cl_ash(aux, MAKE_FIXNUM(nb)));
		}
	}
}

void
ecl_write_byte(cl_object c, cl_object strm)
{
	cl_index (*write_byte8)(cl_object strm, char *c, cl_index n);
	cl_index bs;
	/*
	 * The first part is only for composite or complex streams.
	 */
BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm)) {
		funcall(3, @'gray::stream-write-byte', strm, c);
		return;
	}
#endif
	write_byte8 = stream_dispatch_table(strm)->write_byte8;
	bs = strm->stream.byte_size;
	if (bs == 8) {
		cl_fixnum i = (strm->stream.signed_bytes)? fixint(c) : fixnnint(c);
		char c = (char)i;
		write_byte8(strm, &c, 1);
	} else do {
		cl_object b = cl_logand(2, c, MAKE_FIXNUM(0xFF));
		char aux = (char)fix(b);
		if (write_byte8(strm, &aux, 1) < 1)
			break;
		c = cl_ash(c, MAKE_FIXNUM(-8));
		bs -= 8;
	} while (bs);
}

int
ecl_write_char(int c, cl_object strm)
{
	return stream_dispatch_table(strm)->write_char(strm, c);
}

void
ecl_unread_char(int c, cl_object strm)
{
	return stream_dispatch_table(strm)->unread_char(strm, c);
}

int
ecl_listen_stream(cl_object strm)
{
	return stream_dispatch_table(strm)->listen(strm);
}

void
ecl_clear_input(cl_object strm)
{
	return stream_dispatch_table(strm)->clear_input(strm);
}

void
ecl_clear_output(cl_object strm)
{
	return stream_dispatch_table(strm)->clear_output(strm);
}

void
ecl_force_output(cl_object strm)
{
	return stream_dispatch_table(strm)->force_output(strm);
}

void
ecl_finish_output(cl_object strm)
{
	return stream_dispatch_table(strm)->finish_output(strm);
}

int
ecl_file_column(cl_object strm)
{
	return stream_dispatch_table(strm)->column(strm);
}

cl_object
ecl_file_length(cl_object strm)
{
	return stream_dispatch_table(strm)->length(strm);
}

cl_object
ecl_file_position(cl_object strm)
{
	return stream_dispatch_table(strm)->get_position(strm);
}

cl_object
ecl_file_position_set(cl_object strm, cl_object pos)
{
	return stream_dispatch_table(strm)->set_position(strm, pos);
}

bool
ecl_input_stream_p(cl_object strm)
{
	return stream_dispatch_table(strm)->input_p(strm);
}

bool
ecl_output_stream_p(cl_object strm)
{
	return stream_dispatch_table(strm)->output_p(strm);
}

cl_object
ecl_stream_element_type(cl_object strm)
{
	return stream_dispatch_table(strm)->element_type(strm);
}

int
ecl_interactive_stream_p(cl_object strm)
{
	return stream_dispatch_table(strm)->interactive_p(strm);
}

/*
 * ecl_read_char(s) tries to read a character from the stream S. It outputs
 * either the code of the character read, or EOF. Whe compiled with
 * CLOS-STREAMS and S is an instance object, STREAM-READ-CHAR is invoked
 * to retrieve the character. Then STREAM-READ-CHAR should either
 * output the character, or NIL, indicating EOF.
 *
 * INV: ecl_read_char(strm) checks the type of STRM.
 */
int
ecl_peek_char(cl_object strm)
{
	return stream_dispatch_table(strm)->peek_char(strm);
}

/*******************************tl***************************************
 * SEQUENCES I/O
 */

void
writestr_stream(const char *s, cl_object strm)
{
	while (*s != '\0')
		ecl_write_char(*s++, strm);
}

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
		stream = BROADCAST_STREAM_LIST(stream);
		if (ecl_endp(stream))
			@(return MAKE_FIXNUM(1))
	}
	switch (type_of(string)) {
	case t_base_string:
		l = string->base_string.fillp;
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
si_do_write_sequence(cl_object seq, cl_object stream, cl_object s, cl_object e)
{
	cl_fixnum start,limit,end;
	cl_type t;

	/* Since we have called ecl_length(), we know that SEQ is a valid
	   sequence. Therefore, we only need to check the type of the
	   object, and seq == Cnil i.f.f. t = t_symbol */
	limit = ecl_length(seq);
	start = ecl_fixnum_in_range(@'write-sequence',"start",s,0,limit);
	if (e == Cnil) {
		end = limit;
	} else {
		end = ecl_fixnum_in_range(@'write-sequence',"end",e,0,limit);
	}
	if (end <= start) {
		goto OUTPUT;
	}
	t = type_of(seq);
	if (t == t_list) {
		bool ischar = cl_stream_element_type(stream) == @'base-char';
		cl_object s = ecl_nthcdr(start, seq);
		loop_for_in(s) {
			if (start < end) {
				cl_object elt = CAR(s);
				if (ischar)
					ecl_write_char(ecl_char_code(elt), stream);
				else
					ecl_write_byte(elt, stream);
				start++;
			} else {
				goto OUTPUT;
			}
		} end_loop_for_in;
	} else {
		stream_dispatch_table(stream)->
			write_vector(stream, seq, start, end);
	}
 OUTPUT:
	@(return seq);
}

cl_object
si_do_read_sequence(cl_object seq, cl_object stream, cl_object s, cl_object e)
{
	cl_fixnum start,limit,end;
	cl_type t;

	/* Since we have called ecl_length(), we know that SEQ is a valid
	   sequence. Therefore, we only need to check the type of the
	   object, and seq == Cnil i.f.f. t = t_symbol */
	limit = ecl_length(seq);
	start = ecl_fixnum_in_range(@'read-sequence',"start",s,0,limit);
	if (e == Cnil) {
		end = limit;
	} else {
		end = ecl_fixnum_in_range(@'read-sequence',"end",e,0,limit);
	}
	if (end <= start) {
		goto OUTPUT;
	}
	t = type_of(seq);
	if (t == t_list) {
		bool ischar = cl_stream_element_type(stream) == @'base-char';
		seq = ecl_nthcdr(start, seq);
		loop_for_in(seq) {
			if (start >= end) {
				goto OUTPUT;
			} else {
				cl_object c;
				if (ischar) {
					int i = ecl_read_char(stream);
					if (i < 0) goto OUTPUT;
					c = CODE_CHAR(i);
				} else {
					c = ecl_read_byte(stream);
					if (c == Cnil) goto OUTPUT;
				}
				ECL_RPLACA(seq, c);
				start++;
			}
		} end_loop_for_in;
	} else {
		start = stream_dispatch_table(stream)->
			read_vector(stream, seq, start, end);
	}
 OUTPUT:
	@(return MAKE_FIXNUM(start))
}

/**********************************************************************
 * LISP LEVEL INTERFACE
 */

cl_object
si_file_column(cl_object strm)
{
	@(return MAKE_FIXNUM(ecl_file_column(strm)))
}

cl_object
cl_file_length(cl_object strm)
{
	@(return ecl_file_length(strm))
}

@(defun file-position (file_stream &o position)
	cl_object output;
@
	if (Null(position)) {
		output = ecl_file_position(file_stream);
	} else {
		if (position == @':start') {
			position = MAKE_FIXNUM(0);
		} else if (position == @':end') {
			position = cl_file_length(file_stream);
			if (position == Cnil) {
				output = Cnil;
				goto OUTPUT;
			}
		}
		output = ecl_file_position_set(file_stream, position);
	}
  OUTPUT:
	@(return output)
@)

cl_object
cl_input_stream_p(cl_object strm)
{
	@(return (ecl_input_stream_p(strm) ? Ct : Cnil))
}

cl_object
cl_output_stream_p(cl_object strm)
{
	@(return (ecl_output_stream_p(strm) ? Ct : Cnil))
}

cl_object
cl_interactive_stream_p(cl_object strm)
{
	@(return (stream_dispatch_table(strm)->interactive_p(strm)? Ct : Cnil))
}

cl_object
cl_open_stream_p(cl_object strm)
{
	/* ANSI and Cltl2 specify that open-stream-p should work
	   on closed streams, and that a stream is only closed
	   when #'close has been applied on it */
	if (type_of(strm) != t_stream)
		FEwrong_type_argument(@'stream', strm);
	@(return (strm->stream.closed ? Cnil : Ct))
}

cl_object
cl_stream_element_type(cl_object strm)
{
	@(return ecl_stream_element_type(strm))
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

cl_object
cl_streamp(cl_object strm)
{
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm)) {
		return funcall(2, @'gray::streamp', strm);
	}
#endif
	@(return ((type_of(strm) == t_stream) ? Ct : Cnil))
}

/**********************************************************************
 * OTHER TOOLS
 */

cl_object
si_copy_stream(cl_object in, cl_object out)
{
	int c;
	for (c = ecl_read_char(in); c != EOF; c = ecl_read_char(in)) {
		ecl_write_char(c, out);
	}
	ecl_force_output(out);
	@(return Ct)
}


/**********************************************************************
 * FILE OPENING AND CLOSING
 */

static cl_fixnum
normalize_stream_element_type(cl_object element_type)
{
	cl_fixnum sign = 0;
	cl_index size;
	if (funcall(3, @'subtypep', element_type, @'unsigned-byte') != Cnil) {
		sign = +1;
	} else if (funcall(3, @'subtypep', element_type, @'signed-byte') != Cnil) {
		sign = -1;
	} else {
		FEerror("Not a valid stream element type: ~A", 1, element_type);
	}
	if (CONSP(element_type)) {
		if (CAR(element_type) == @'unsigned-byte')
			return fixnnint(cl_cadr(element_type));
		if (CAR(element_type) == @'signed-byte')
			return -fixnnint(cl_cadr(element_type));
	}
	for (size = 1; 1; size++) {
		cl_object type;
		type = cl_list(2, sign>0? @'unsigned-byte' : @'signed-byte',
			       MAKE_FIXNUM(size));
		if (funcall(3, @'subtypep', element_type, type) != Cnil) {
			return size * sign;
		}
	}
}

cl_object
ecl_open_stream(cl_object fn, enum ecl_smmode smm, cl_object if_exists,
		cl_object if_does_not_exist, cl_fixnum byte_size,
		bool char_stream_p)
{
	cl_env_ptr the_env = &cl_env;
	cl_object x;
	FILE *fp;
	cl_object filename = si_coerce_to_filename(fn);
	char *fname = filename->base_string.self;
	bool appending = FALSE;

	if (char_stream_p && byte_size != 8) {
		FEerror("Tried to make a character stream of byte size /= 8.",0);
	}
	ecl_disable_interrupts_env(the_env);
	if (smm == smm_input || smm == smm_probe) {
		fp = fopen(fname, OPEN_R);
		if (fp == NULL) {
			if (if_does_not_exist == @':error') {
				goto CANNOT_OPEN;
			} else if (if_does_not_exist == @':create') {
				fp = fopen(fname, OPEN_W);
				if (fp == NULL)
					goto CANNOT_OPEN;
				fclose(fp);
				fp = fopen(fname, OPEN_R);
				if (fp == NULL)
					goto CANNOT_OPEN;
			} else if (Null(if_does_not_exist)) {
				x = Cnil;
				goto OUTPUT;
			} else {
				x = @':if-does-not-exist';
				fn = if_does_not_exist;
				goto INVALID_OPTION;
			}
		}
	} else if (smm == smm_output || smm == smm_io) {
		if (if_exists == @':new_version' && if_does_not_exist == @':create')
			goto CREATE;
		fp = fopen(fname, OPEN_R);
		if (fp != NULL) {
			fclose(fp);
			if (if_exists == @':error') {
				goto CANNOT_OPEN;
			} else if (if_exists == @':rename') {
				fp = ecl_backup_fopen(fname, (smm == smm_output)
						      ? OPEN_W
						      : OPEN_RW);
				if (fp == NULL) {
					goto CANNOT_OPEN;
				}
			} else if (if_exists == @':rename_and_delete' ||
				   if_exists == @':new_version' ||
				   if_exists == @':supersede') {
				fp = fopen(fname, (smm == smm_output)
					   ? OPEN_W
					   : OPEN_RW);
				if (fp == NULL) {
					goto CANNOT_OPEN;
				}
			} else if (if_exists == @':overwrite' || if_exists == @':append') {
				/* We cannot use "w+b" because it truncates.
				   We cannot use "a+b" because writes jump to the end. */
				int f = open(filename->base_string.self, (smm == smm_output)?
					     (O_WRONLY|O_CREAT) : (O_RDWR|O_CREAT));
				if (f < 0) {
					goto CANNOT_OPEN;
				}
				fp = fdopen(f, (smm == smm_output)? OPEN_W : OPEN_RW);
				if (fp == NULL) {
					close(f);
					goto CANNOT_OPEN;
				}
				if (if_exists == @':append') {
					ecl_fseeko(fp, 0, SEEK_END);
					appending = TRUE;
				}
			} else if (Null(if_exists)) {
				x = Cnil;
				goto OUTPUT;
			} else {
				x = @':if-exists';
				fn = if_exists;
				goto INVALID_OPTION;
			}
		} else {
			if (if_does_not_exist == @':error') {
				goto CANNOT_OPEN;
			} else if (if_does_not_exist == @':create') {
			CREATE:
				fp = fopen(fname, (smm == smm_output)
					   ? OPEN_W
					   : OPEN_RW);
				if (fp == NULL) {
					goto CANNOT_OPEN;
				}
			} else if (Null(if_does_not_exist)) {
				x = Cnil;
				goto OUTPUT;
			} else {
				x = @':if-does-not-exist';
				fn = if_does_not_exist;
				goto INVALID_OPTION;
			}
		}
	} else {
		goto INVALID_MODE;
	}
	x = ecl_make_stream_from_FILE(fn, fp, smm, byte_size, char_stream_p);
	si_set_buffering_mode(x, char_stream_p? @':line-buffered' : @':fully-buffered');
	if (smm == smm_probe) {
		cl_close(1, x);
	} else {
		si_set_finalizer(x, Ct);
		if (!char_stream_p) {
			/* Set file pointer to the correct position */
			if (appending) {
				ecl_fseeko(fp, -1, SEEK_END);
			} else {
				ecl_fseeko(fp, 0, SEEK_SET);
			}
		}
	}
 OUTPUT:
	ecl_enable_interrupts_env(the_env);
	return x;
 CANNOT_OPEN:
	ecl_enable_interrupts_env(the_env);
	FEcannot_open(fn);
	return Cnil;
 INVALID_OPTION:
	ecl_enable_interrupts_env(the_env);
	FEerror("Invalid value op option ~A: ~A", 2, x, fn);
	return Cnil;
 INVALID_MODE:
	ecl_enable_interrupts_env(the_env);
	FEerror("Illegal stream mode ~S", 1, MAKE_FIXNUM(smm));
	return Cnil;
}


@(defun open (filename
	      &key (direction @':input')
		   (element_type @'base-char')
		   (if_exists Cnil iesp)
		   (if_does_not_exist Cnil idnesp)
	           (external_format @':default')
		   (use_header_p Cnil)
	      &aux strm)
	enum ecl_smmode smm;
	bool char_stream_p;
	cl_fixnum byte_size;
@
	if (external_format != @':default')
		FEerror("~S is not a valid stream external format.", 1,
			external_format);
	/* INV: ecl_open_stream() checks types */
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
		char_stream_p = 1;
		byte_size = 8;
	} else if (element_type == @'signed-byte') {
		char_stream_p = 0;
		byte_size = -8;
	} else if (element_type == @'unsigned-byte') {
		char_stream_p = 0;
		byte_size = 8;
	} else if (funcall(3, @'subtypep', element_type, @'character') != Cnil) {
		char_stream_p = 1;
		byte_size = 8;
	} else {
		char_stream_p = 0;
		byte_size = normalize_stream_element_type(element_type);
	}
	strm = ecl_open_stream(filename, smm, if_exists, if_does_not_exist,
			       byte_size, char_stream_p);
	@(return strm)
@)


@(defun close (strm &key (abort @'nil'))
@
	@(return stream_dispatch_table(strm)->close(strm));
@)

/**********************************************************************
 * BACKEND
 */

static int
flisten(FILE *fp)
{
#ifdef HAVE_SELECT
	fd_set fds;
	int retv, fd;
	struct timeval tv = { 0, 0 };
#endif
#if defined(mingw32) || defined(_MSC_VER)
	HANDLE hnd;
#endif
	if (feof(fp))
		return ECL_LISTEN_EOF;
#ifdef FILE_CNT
	if (FILE_CNT(fp) > 0)
		return ECL_LISTEN_AVAILABLE;
#endif
#if !defined(mingw32) && !defined(_MSC_VER)
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
#else
	hnd = (HANDLE)_get_osfhandle(fileno(fp));
	switch (GetFileType(hnd)) {
		case FILE_TYPE_CHAR: {
			DWORD dw, dw_read, cm;
			if (GetNumberOfConsoleInputEvents(hnd, &dw)) {
				if (!GetConsoleMode(hnd, &cm))
					FEwin32_error("GetConsoleMode() failed", 0);
				if (dw > 0) {
					PINPUT_RECORD recs = (PINPUT_RECORD)GC_malloc(sizeof(INPUT_RECORD)*dw);
					int i;
					if (!PeekConsoleInput(hnd, recs, dw, &dw_read))
						FEwin32_error("PeekConsoleInput failed()", 0);
					if (dw_read > 0) {
						if (cm & ENABLE_LINE_INPUT) {
							for (i=0; i<dw_read; i++)
								if (recs[i].EventType == KEY_EVENT &&
								    recs[i].Event.KeyEvent.bKeyDown &&
								    recs[i].Event.KeyEvent.uChar.AsciiChar == 13)
									return ECL_LISTEN_AVAILABLE;
						} else {
							for (i=0; i<dw_read; i++)
								if (recs[i].EventType == KEY_EVENT &&
								    recs[i].Event.KeyEvent.bKeyDown &&
								    recs[i].Event.KeyEvent.uChar.AsciiChar != 0)
									return ECL_LISTEN_AVAILABLE;
						}
					}
				}
				return ECL_LISTEN_NO_CHAR;
			} else
				FEwin32_error("GetNumberOfConsoleInputEvents() failed", 0);
			break;
		}
		case FILE_TYPE_DISK:
			/* use regular file code below */
			break;
		case FILE_TYPE_PIPE: {
			DWORD dw;
			if (PeekNamedPipe(hnd, NULL, 0, NULL, &dw, NULL))
				return (dw > 0 ? ECL_LISTEN_AVAILABLE : ECL_LISTEN_NO_CHAR);
			else if (GetLastError() == ERROR_BROKEN_PIPE)
				return ECL_LISTEN_EOF;
			else
				FEwin32_error("PeekNamedPipe() failed", 0);
			break;
		}
		default:
			FEerror("Unsupported Windows file type: ~A", 1, MAKE_FIXNUM(GetFileType(hnd)));
			break;
	}
#endif
	/* This code is portable, and implements the expected behavior for regular files.
	   It will fail on noninteractive streams. */
	{
		/* regular file */
		ecl_off_t old_pos = ecl_ftello(fp), end_pos;
		if (ecl_fseeko(fp, 0, SEEK_END) != 0)
			FElibc_error("fseek() returned an error value", 0);
		end_pos = ecl_ftello(fp);
		if (ecl_fseeko(fp, old_pos, SEEK_SET) != 0)
			FElibc_error("fseek() returned an error value", 0);
		return (end_pos > old_pos ? ECL_LISTEN_AVAILABLE : ECL_LISTEN_EOF);
	}
	return !ECL_LISTEN_AVAILABLE;
}

/*
 * When using the same stream for input and output operations, we have to
 * use some file position operation before reading again.
 */

static void io_stream_begin_write(cl_object strm)
{
	if (strm->stream.last_op > 0) {
		ecl_fseeko((FILE*)strm->stream.file, 0, SEEK_CUR);
	}
	strm->stream.last_op = -1;
}

/*
 * When using the same stream for input and output operations, we have to
 * flush the stream before writing.
 */

static void io_stream_begin_read(cl_object strm)
{
	if (strm->stream.last_op < 0) {
		ecl_force_output(strm);
	}
	strm->stream.last_op = +1;
}

static cl_object
ecl_off_t_to_integer(ecl_off_t offset)
{
	cl_object output;
	if (sizeof(ecl_off_t) == sizeof(cl_fixnum)) {
		output = ecl_make_integer(offset);
	} else if (offset <= MOST_POSITIVE_FIXNUM) {
		output = MAKE_FIXNUM((cl_fixnum)offset);
	} else {
		cl_object y = big_register0_get();
#ifdef WITH_GMP
		if (sizeof(y->big.big_limbs[0]) == sizeof(cl_index)) {
			y->big.big_limbs[0] = (cl_index)offset;
			offset >>= FIXNUM_BITS;
			y->big.big_limbs[1] = offset;
			y->big.big_size = offset? 2 : 1;
		} else if (sizeof(y->big.big_limbs[0]) >= sizeof(ecl_off_t)) {
			y->big.big_limbs[0] = offset;
			y->big.big_size = 1;
		}
#else
		y->big.big_num = offset;
#endif
		output = big_register_normalize(y);
	}
	return output;
}

static ecl_off_t
ecl_integer_to_off_t(cl_object offset)
{
	ecl_off_t output = 0;
	if (sizeof(ecl_off_t) == sizeof(cl_fixnum)) {
		output = fixint(offset);
	} else if (FIXNUMP(offset)) {
		output = fixint(offset);
	} else if (type_of(offset) == t_bignum) {
#ifdef WITH_GMP
		if (sizeof(offset->big.big_limbs[0]) == sizeof(cl_index)) {
			if (offset->big.big_size > 2) {
				goto ERR;
			}
			if (offset->big.big_size == 2) {
			    output = offset->big.big_limbs[1];
			    output <<= FIXNUM_BITS;
			}
			output += offset->big.big_limbs[0];
		} else if (sizeof(offset->big.big_limbs[0]) >= sizeof(ecl_off_t)) {
			if (offset->big.big_size > 1) {
				goto ERR;
			}
			output = offset->big.big_limbs[0];
		}
#else
		output = offset->big.big_num;
#endif
	} else {
	ERR:	FEerror("Not a valid file offset: ~S", 1, offset);
	}
	return output;
}

static cl_object
alloc_stream()
{
	cl_object x = ecl_alloc_object(t_stream);
	x->stream.closed = 0;
	x->stream.file = NULL;
	x->stream.object0 =
	x->stream.object1 = OBJNULL;
	x->stream.int0 = x->stream.int1 = 0;
	x->stream.unread = EOF;
	x->stream.char_stream_p = 1;
	x->stream.byte_size = 8;
	x->stream.signed_bytes = 0;
	x->stream.buffer = NULL;
	return x;
}

/**********************************************************************
 * ERROR MESSAGES
 */

static cl_object
not_a_file_stream(cl_object strm)
{
	cl_error(9, @'simple-type-error', @':format-control',
		 make_constant_base_string("~A is not an file stream"),
		 @':format-arguments', cl_list(1, strm),
		 @':expected-type', @'file-stream',
		 @':datum', strm);
}

static void
not_an_input_stream(cl_object strm)
{
	cl_error(9, @'simple-type-error', @':format-control',
		 make_constant_base_string("~A is not an input stream"),
		 @':format-arguments', cl_list(1, strm),
		 @':expected-type', cl_list(2, @'satisfies', @'input-stream-p'),
		 @':datum', strm);
}

static void
not_an_output_stream(cl_object strm)
{
	cl_error(9, @'simple-type-error', @':format-control',
		 make_constant_base_string("~A is not an output stream"),
		 @':format-arguments', cl_list(1, strm),
		 @':expected-type', cl_list(2, @'satisfies', @'output-stream-p'),
		 @':datum', strm);
}

static void
not_a_character_stream(cl_object s)
{
	cl_error(9, @'simple-type-error', @':format-control',
		 make_constant_base_string("~A is not a character stream"),
		 @':format-arguments', cl_list(1, s),
		 @':expected-type', @'character',
		 @':datum', cl_stream_element_type(s));
}

static void
not_a_binary_stream(cl_object s)
{
	cl_error(9, @'simple-type-error', @':format-control',
		 make_constant_base_string("~A is not a binary stream"),
		 @':format-arguments', cl_list(1, s),
		 @':expected-type', @'integer',
		 @':datum', cl_stream_element_type(s));
}

static void
unread_error(cl_object s)
{
	CEerror(Ct, "Error when using UNREAD-CHAR on stream ~D", 1, s);
}

static void
unread_twice(cl_object s)
{
	CEerror(Ct, "Used UNREAD-CHAR twice on stream ~D", 1, s);
}

static int
restartable_io_error(cl_object strm)
{
	cl_env_ptr the_env = &cl_env;
	volatile int old_errno = errno;
	/* ecl_disable_interrupts(); ** done by caller */
	clearerr((FILE*)strm->stream.file);
	ecl_enable_interrupts();
	if (errno == EINTR) {
		return 1;
	} else {
		FElibc_error("Read or write operation to stream ~S signaled an error.",
			     1, strm);
		return 0;
	}
}

static void
io_error(cl_object strm)
{
	cl_env_ptr the_env = &cl_env;
	FILE *f = IO_STREAM_FILE(strm);
	/* ecl_disable_interrupts(); ** done by caller */
	clearerr(f);
	ecl_enable_interrupts();
	FElibc_error("Read or write operation to stream ~S signaled an error.",
		     1, strm);
}

static void
wrong_file_handler(cl_object strm)
{
	FEerror("Internal error: stream ~S has no valid C file handler.", 1, strm);
}

#if defined(ECL_WSOCK)
static void
wsock_error( const char *err_msg, cl_object strm )
{
	char *msg;
	cl_object msg_obj;
	/* ecl_disable_interrupts(); ** done by caller */
	{
		FormatMessage( FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER,
			       0, WSAGetLastError(), 0, ( void* )&msg, 0, NULL );
		msg_obj = make_base_string_copy( msg );
		LocalFree( msg );
	}
	ecl_enable_interrupts();
	FEerror( err_msg, 2, strm, msg_obj );
}
#endif

void
init_file(void)
{
	const cl_env_ptr env = ecl_process_env();
	cl_object standard_input;
	cl_object standard_output;
	cl_object error_output;
	cl_object aux;
	cl_object null_stream;
	cl_object x;

	null_stream = ecl_make_stream_from_FILE(make_constant_base_string("/dev/null"),
						NULL, smm_io, 8, 1);
	generic_close(null_stream);
	null_stream = cl_make_two_way_stream(null_stream, cl_make_broadcast_stream(0));
	cl_core.null_stream = null_stream;

	standard_input = ecl_make_stream_from_FILE(make_constant_base_string("stdin"),
						   stdin, smm_input, 8, 1);

	standard_output = ecl_make_stream_from_FILE(make_constant_base_string("stdout"),
						    stdout, smm_output, 8, 1);

	error_output = ecl_make_stream_from_FILE(make_constant_base_string("stderr"),
						 stderr, smm_output, 8, 1);

	cl_core.terminal_io = aux 
		= cl_make_two_way_stream(standard_input, standard_output);

	ECL_SET(@'*terminal-io*', aux);

	aux = cl_make_synonym_stream(@'*terminal-io*');

	ECL_SET(@'*standard-input*', aux);
	ECL_SET(@'*standard-output*', aux);
	ECL_SET(@'*error-output*', error_output);

	ECL_SET(@'*query-io*', aux);
	ECL_SET(@'*debug-io*', aux);
	ECL_SET(@'*trace-output*', aux);
}
