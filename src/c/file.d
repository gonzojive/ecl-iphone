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
#if !defined(mingw32) && !defined(_MSC_VER)
#include <sys/stat.h>
/* it isn't pulled in by fcntl.h */
#endif
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

/* Maximum number of bytes required to encode a character.
 * This currently corresponds to (4 + 2) for the ISO-2022-JP-* encodings
 * with 4 being the charset prefix, 2 for the character.
 */
#define ENCODING_BUFFER_MAX_SIZE 6

static cl_index ecl_read_byte8(cl_object stream, unsigned char *c, cl_index n);
static cl_index ecl_write_byte8(cl_object stream, unsigned char *c, cl_index n);

struct ecl_file_ops *duplicate_dispatch_table(const struct ecl_file_ops *ops);
const struct ecl_file_ops *stream_dispatch_table(cl_object strm);

static int flisten(FILE *);
static int file_listen(int);
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
static void character_size_overflow(cl_object strm, int c);
static void unsupported_character(cl_object strm);
static void malformed_character(cl_object strm);
static void too_long_utf8_sequence(cl_object strm);
static void invalid_codepoint(cl_object strm, cl_fixnum c);
static void wrong_file_handler(cl_object strm);

/**********************************************************************
 * NOT IMPLEMENTED or NOT APPLICABLE OPERATIONS
 */

static cl_index
not_output_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	not_an_output_stream(strm);
	return 0;
}

static cl_index
not_input_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	not_an_input_stream(strm);
	return 0;
}

static cl_index
not_binary_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	not_a_binary_stream(strm);
	return 0;
}

static cl_index
not_binary_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	not_a_binary_stream(strm);
	return 0;
}

static void
not_output_write_byte(cl_object c, cl_object strm)
{
	return not_an_output_stream(strm);
}

static cl_object
not_input_read_byte(cl_object strm)
{
	not_an_input_stream(strm);
	return OBJNULL;
}

static void
not_binary_write_byte(cl_object c, cl_object strm)
{
	return not_a_binary_stream(strm);
}

static cl_object
not_binary_read_byte(cl_object strm)
{
	not_a_binary_stream(strm);
	return OBJNULL;
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
closed_stream_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	FEclosed_stream(strm);
}

static cl_index
closed_stream_write_byte8(cl_object strm, unsigned char *c, cl_index n)
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
 * Byte operations based on octet operators.
 */
static cl_object
generic_read_byte_unsigned8(cl_object strm)
{
	unsigned char c;
	if (strm->stream.ops->read_byte8(strm, &c, 1) < 1)
		return Cnil;
	return MAKE_FIXNUM(c);
}

static void
generic_write_byte_unsigned8(cl_object byte, cl_object strm)
{
	unsigned char c = fixnnint(byte);
	strm->stream.ops->write_byte8(strm, &c, 1);
}

static cl_object
generic_read_byte_signed8(cl_object strm)
{
	signed char c;
	if (strm->stream.ops->read_byte8(strm, (unsigned char *)&c, 1) < 1)
		return Cnil;
	return MAKE_FIXNUM(c);
}

static void
generic_write_byte_signed8(cl_object byte, cl_object strm)
{
	signed char c = fixint(byte);
	strm->stream.ops->write_byte8(strm, (unsigned char *)&c, 1);
}

static cl_object
generic_read_byte_le(cl_object strm)
{
	cl_index (*read_byte8)(cl_object, unsigned char *, cl_index);
	unsigned char c;
	cl_index nb, bs;
	cl_object output = MAKE_FIXNUM(0);
	read_byte8 = strm->stream.ops->read_byte8;
	bs = strm->stream.byte_size;
	for (nb = 0; bs >= 8; bs -= 8, nb += 8) {
		cl_object aux;
		if (read_byte8(strm, &c, 1) < 1)
			return Cnil;
		if (bs <= 8 && (strm->stream.flags & ECL_STREAM_SIGNED_BYTES))
			aux = MAKE_FIXNUM((signed char)c);
		else
			aux = MAKE_FIXNUM((unsigned char)c);
		output = cl_logior(2, output, cl_ash(aux, MAKE_FIXNUM(nb)));
	}
	return output;
}

static void
generic_write_byte_le(cl_object c, cl_object strm)
{
	cl_index (*write_byte8)(cl_object strm, unsigned char *c, cl_index n);
	cl_index bs;
	write_byte8 = strm->stream.ops->write_byte8;
	bs = strm->stream.byte_size;
	do {
		cl_object b = cl_logand(2, c, MAKE_FIXNUM(0xFF));
		unsigned char aux = (unsigned char)fix(b);
		if (write_byte8(strm, &aux, 1) < 1)
			break;
		c = cl_ash(c, MAKE_FIXNUM(-8));
		bs -= 8;
	} while (bs);
}

static cl_object
generic_read_byte(cl_object strm)
{
	cl_index (*read_byte8)(cl_object, unsigned char *, cl_index);
	unsigned char c;
	cl_object output = NULL;
	cl_index bs;
	read_byte8 = strm->stream.ops->read_byte8;
	bs = strm->stream.byte_size;
	for (; bs >= 8; bs -= 8) {
		cl_object aux;
		if (read_byte8(strm, &c, 1) < 1)
			return Cnil;
		if (output) {
			output = cl_logior(2, MAKE_FIXNUM(c),
					   cl_ash(output, MAKE_FIXNUM(8)));
		} else if (strm->stream.flags & ECL_STREAM_SIGNED_BYTES) {
			output = MAKE_FIXNUM((signed char)c);
		} else {
			output = MAKE_FIXNUM((unsigned char)c);
		}
	}
	return output;
}

static void
generic_write_byte(cl_object c, cl_object strm)
{
	cl_index (*write_byte8)(cl_object strm, unsigned char *c, cl_index n);
	cl_index bs;
	write_byte8 = strm->stream.ops->write_byte8;
	bs = strm->stream.byte_size;
	do {
		unsigned char aux;
		cl_object b;
		bs -= 8;
		b = cl_logand(2, MAKE_FIXNUM(0xFF), bs? cl_ash(c, MAKE_FIXNUM(-bs)) : c);
		aux = (unsigned char)fix(b);
		if (write_byte8(strm, &aux, 1) < 1)
			break;
	} while (bs);
}

static int
generic_peek_char(cl_object strm)
{
	int out = ecl_read_char(strm);
	if (out != EOF) ecl_unread_char(out, strm);
	return out;
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
	const struct ecl_file_ops *ops;
	if (start >= end)
		return start;
	ops = stream_dispatch_table(strm);
	if (data->vector.elttype == aet_bc ||
#ifdef ECL_UNICODE
	    data->vector.elttype == aet_ch ||
#endif
	    (data->vector.elttype == aet_object && CHARACTERP(ecl_elt(data, 0)))) {
		int (*write_char)(cl_object, int) = ops->write_char;			
		for (; start < end; start++) {
			write_char(strm, ecl_char_code(ecl_elt(data, start)));
		}
	} else {
		void (*write_byte)(cl_object, cl_object) = ops->write_byte;
		for (; start < end; start++) {
			write_byte(ecl_elt(data, start), strm);
		}
	}
	return start;
}

static cl_index
generic_read_vector(cl_object strm, cl_object data, cl_index start, cl_index end)
{
	const struct ecl_file_ops *ops;
	cl_object expected_type;
	if (start >= end)
		return start;
	expected_type = ecl_stream_element_type(strm);
	ops = stream_dispatch_table(strm);
	if (expected_type == @'base-char' || expected_type == @'character') {
		int (*read_char)(cl_object) = ops->read_char;			
		for (; start < end; start++) {
			cl_fixnum c = read_char(strm);
			if (c == EOF) break;
			ecl_elt_set(data, start, CODE_CHAR(c));
		}
	} else {
		cl_object (*read_byte)(cl_object) = ops->read_byte;
		for (; start < end; start++) {
			cl_object x = read_byte(strm);
			if (Null(x)) break;
			ecl_elt_set(data, start, x);
		}
	}
	return start;
}

/**********************************************************************
 * CHARACTER AND EXTERNAL FORMAT SUPPORT
 */

static void
eformat_unread_char(cl_object strm, int c)
{
	if (c != strm->stream.last_char) {
		unread_twice(strm);
	}
	{
		cl_object l = Cnil;
		unsigned char buffer[2*ENCODING_BUFFER_MAX_SIZE];
		int ndx = 0;
		cl_fixnum i = strm->stream.last_code[0];
		if (i != EOF) {
			ndx += strm->stream.encoder(strm, buffer, i);
		}
		i = strm->stream.last_code[1];
		if (i != EOF) {
			ndx += strm->stream.encoder(strm, buffer, i);
		}
		while (ndx != 0) {
			l = CONS(MAKE_FIXNUM(buffer[--ndx]), l);
		}
		strm->stream.byte_stack = ecl_nconc(strm->stream.byte_stack, l);
		strm->stream.last_char = EOF;
	}
}

static int
eformat_read_char(cl_object strm)
{
	int c = strm->stream.decoder(strm, strm->stream.ops->read_byte8, strm);
	if (c != EOF) {
		strm->stream.last_char = c;
		strm->stream.last_code[0] = c;
		strm->stream.last_code[1] = EOF;
	}
	return c;
}

static int
eformat_write_char(cl_object strm, int c)
{
	unsigned char buffer[ENCODING_BUFFER_MAX_SIZE];
	int nbytes = strm->stream.encoder(strm, buffer, c);
	if (nbytes == 0) {
		character_size_overflow(strm, c);
	}
	strm->stream.ops->write_byte8(strm, buffer, nbytes);
	if (c == '\n')
		IO_STREAM_COLUMN(strm) = 0;
	else if (c == '\t')
		IO_STREAM_COLUMN(strm) = (IO_STREAM_COLUMN(strm)&~07) + 8;
	else
		IO_STREAM_COLUMN(strm)++;
	return c;
}

static int
eformat_read_char_cr(cl_object strm)
{
	int c = eformat_read_char(strm);
	if (c == ECL_CHAR_CODE_RETURN) {
		c = ECL_CHAR_CODE_NEWLINE;
		strm->stream.last_char = c;
	}
	return c;
}

static int
eformat_write_char_cr(cl_object strm, int c)
{
	if (c == ECL_CHAR_CODE_NEWLINE) {
		eformat_write_char(strm, ECL_CHAR_CODE_RETURN);
		IO_STREAM_COLUMN(strm) = 0;
		return c;
	}
	return eformat_write_char(strm, c);
}

static int
eformat_read_char_crlf(cl_object strm)
{
	int c = eformat_read_char(strm);
	if (c == ECL_CHAR_CODE_RETURN) {
		c = eformat_read_char(strm);
		if (c == ECL_CHAR_CODE_LINEFEED) {
			strm->stream.last_code[1] = c;
			c = ECL_CHAR_CODE_NEWLINE;
		} else {
			eformat_unread_char(strm, c);
			c = ECL_CHAR_CODE_RETURN;
			strm->stream.last_code[0] = c;
			strm->stream.last_code[1] = EOF;
		}
		strm->stream.last_char = c;
	}
	return c;
}

static int
eformat_write_char_crlf(cl_object strm, int c)
{
	if (c == ECL_CHAR_CODE_NEWLINE) {
		eformat_write_char(strm, ECL_CHAR_CODE_RETURN);
		eformat_write_char(strm, ECL_CHAR_CODE_LINEFEED);
		IO_STREAM_COLUMN(strm) = 0;
		return c;
	}
	return eformat_write_char(strm, c);
}

/*
 * If we use Unicode, this is LATIN-1, ISO-8859-1, that is the 256
 * lowest codes of Unicode. Otherwise, we simply assume the file and
 * the strings use the same format.
 */

static int
passthrough_decoder(cl_object stream, cl_eformat_read_byte8 read_byte8, cl_object source)
{
	unsigned char aux;
	if (read_byte8(source, &aux, 1) < 1)
		return EOF;
	else
		return aux;
}

static int
passthrough_encoder(cl_object stream, unsigned char *buffer, int c)
{
	if (c > 0xFF) {
		return 0;
	}
	buffer[0] = c;
	return 1;
}

#ifdef ECL_UNICODE
/*
 * US ASCII, that is the 128 (0-127) lowest codes of Unicode
 */

static int
ascii_decoder(cl_object stream, cl_eformat_read_byte8 read_byte8, cl_object source)
{
	unsigned char aux;
	if (read_byte8(source, &aux, 1) < 1) {
		return EOF;
	} else if (aux > 127) {
		invalid_codepoint(stream, aux);
	} else {
		return aux;
	}
}

static int
ascii_encoder(cl_object stream, unsigned char *buffer, int c)
{
	if (c > 127) {
		return 0;
	}
	buffer[0] = c;
	return 1;
}

/*
 * UCS-4 BIG ENDIAN
 */

static int
ucs_4be_decoder(cl_object stream, cl_eformat_read_byte8 read_byte8, cl_object source)
{
	unsigned char buffer[4];
	if (read_byte8(source, buffer, 4) < 4) {
		return EOF;
	} else {
		return buffer[3]+(buffer[2]<<8)+(buffer[1]<<16)+(buffer[0]<<24);
	}
}

static int
ucs_4be_encoder(cl_object stream, unsigned char *buffer, int c)
{
	buffer[3] = c & 0xFF; c >>= 8;
	buffer[2] = c & 0xFF; c >>= 8;
	buffer[1] = c & 0xFF; c >>= 8;
	buffer[0] = c;
	return 4;
}

/*
 * UCS-4 LITTLE ENDIAN
 */

static int
ucs_4le_decoder(cl_object stream, cl_eformat_read_byte8 read_byte8, cl_object source)
{
	unsigned char buffer[4];
	if (read_byte8(source, buffer, 4) < 4) {
		return EOF;
	} else {
		return buffer[0]+(buffer[1]<<8)+(buffer[2]<<16)+(buffer[3]<<24);
	}
}

static int
ucs_4le_encoder(cl_object stream, unsigned char *buffer, int c)
{
	buffer[0] = c & 0xFF; c >>= 8;
	buffer[1] = c & 0xFF; c >>= 8;
	buffer[2] = c & 0xFF; c >>= 8;
	buffer[3] = c;
	return 4;
}

/*
 * UCS-4 BOM ENDIAN
 */

static int
ucs_4_decoder(cl_object stream, cl_eformat_read_byte8 read_byte8, cl_object source)
{
	cl_fixnum c = ucs_4be_decoder(stream, read_byte8, source);
	if (c == 0xFEFF) {
		stream->stream.decoder = ucs_4be_decoder;
		stream->stream.encoder = ucs_4be_encoder;
		return ucs_4be_decoder(stream, read_byte8, source);
	} else if (c == 0xFFFE0000) {
		stream->stream.decoder = ucs_4le_decoder;
		stream->stream.encoder = ucs_4le_encoder;
		return ucs_4le_decoder(stream, read_byte8, source);
	} else {
		stream->stream.decoder = ucs_4be_decoder;
		stream->stream.encoder = ucs_4be_encoder;
		return c;
	}
}

static int
ucs_4_encoder(cl_object stream, unsigned char *buffer, int c)
{
	stream->stream.decoder = ucs_4be_decoder;
	stream->stream.encoder = ucs_4be_encoder;
	buffer[0] = 0xFF;
	buffer[1] = 0xFE;
	buffer[2] = buffer[3] = 0;
	return 4 + ucs_4be_encoder(stream, buffer+4, c);
}


/*
 * UTF-16 BIG ENDIAN
 */

static int
ucs_2be_decoder(cl_object stream, cl_eformat_read_byte8 read_byte8, cl_object source)
{
	unsigned char buffer[2];
	if (read_byte8(source, buffer, 2) < 2) {
		return EOF;
	} else {
		int c = ((int)buffer[0] << 8) | buffer[1];
		if ((buffer[0] & 0xFC) == 0xD8) {
			if (read_byte8(source, buffer, 2) < 2) {
				return EOF;
			} else {
				int aux = ((int)buffer[0] << 8) | buffer[1];
				if ((buffer[0] & 0xF8) != 0xDC) {
					malformed_character(stream);
				}
				c = ((c & 0x3FFF) << 10) + (aux & 0x3FFF) + 0x10000;
			}
		} else {
			return c;
		}
	}
}

static int
ucs_2be_encoder(cl_object stream, unsigned char *buffer, int c)
{
	if (c >= 0x10000) {
		c -= 0x10000;
		ucs_2be_encoder(stream, buffer, (c >> 10) | 0xD800);
		ucs_2be_encoder(stream, buffer+2, (c & 0x3FFF) | 0xDC00);
		return 4;
	} else {
		buffer[1] = c & 0xFF; c >>= 8;
		buffer[0] = c;
		return 2;
	}
}

/*
 * UTF-16 LITTLE ENDIAN
 */

static int
ucs_2le_decoder(cl_object stream, cl_eformat_read_byte8 read_byte8, cl_object source)
{
	unsigned char buffer[2];
	if (read_byte8(source, buffer, 2) < 2) {
		return EOF;
	} else {
		int c = ((int)buffer[1] << 8) | buffer[0];
		if ((buffer[1] & 0xFC) == 0xD8) {
			if (read_byte8(source, buffer, 2) < 2) {
				return EOF;
			} else {
				int aux = ((int)buffer[1] << 8) | buffer[0];
				if ((buffer[1] & 0xF8) != 0xDC) {
					malformed_character(stream);
				}
				c = ((c & 0x3FFF) << 10) + (aux & 0x3FFF) + 0x10000;
			}
		} else {
			return c;
		}
	}
}

static int
ucs_2le_encoder(cl_object stream, unsigned char *buffer, int c)
{
	if (c >= 0x10000) {
		c -= 0x10000;
		ucs_2le_encoder(stream, buffer, (c >> 10) | 0xD8000);
		ucs_2le_encoder(stream, buffer+2, (c & 0x3FFF) | 0xD800);
		return 4;
	} else {
		buffer[0] = c & 0xFF; c >>= 8;
		buffer[1] = c & 0xFF;
		return 2;
	}
}

/*
 * UTF-16 BOM ENDIAN
 */

static int
ucs_2_decoder(cl_object stream, cl_eformat_read_byte8 read_byte8, cl_object source)
{
	int c = ucs_2be_decoder(stream, read_byte8, source);
	if (c == 0xFEFF) {
		stream->stream.decoder = ucs_2be_decoder;
		stream->stream.encoder = ucs_2be_encoder;
		return ucs_2be_decoder(stream, read_byte8, source);
	} else if (c == 0xFFFE) {
		stream->stream.decoder = ucs_2le_decoder;
		stream->stream.encoder = ucs_2le_encoder;
		return ucs_2le_decoder(stream, read_byte8, source);
	} else {
		stream->stream.decoder = ucs_2be_decoder;
		stream->stream.encoder = ucs_2be_encoder;
		return c;
	}
}

static int
ucs_2_encoder(cl_object stream, unsigned char *buffer, int c)
{
	stream->stream.decoder = ucs_2be_decoder;
	stream->stream.encoder = ucs_2be_encoder;
	buffer[0] = 0xFF;
	buffer[1] = 0xFE;
	return 2 + ucs_2be_encoder(stream, buffer+2, c);
}

/*
 * USER DEFINED ENCODINGS. SIMPLE CASE.
 */

static int
user_decoder(cl_object stream, cl_eformat_read_byte8 read_byte8, cl_object source)
{
	cl_object table = stream->stream.format_table;
	cl_object character;
	unsigned char buffer[2];
	if (read_byte8(source, buffer, 1) < 1) {
		return EOF;
	}
	character = ecl_gethash_safe(MAKE_FIXNUM(buffer[0]), table, Cnil);
	if (Null(character)) {
		invalid_codepoint(stream, buffer[0]);
	}
	if (character == Ct) {
		if (read_byte8(source, buffer+1, 1) < 1) {
			return EOF;
		} else {
			cl_fixnum byte = (buffer[0]<<8) + buffer[1];
			character = ecl_gethash_safe(MAKE_FIXNUM(byte), table, Cnil);
			if (Null(character)) {
				invalid_codepoint(stream, byte);
			}
		}
	}
	return CHAR_CODE(character);
}

static int
user_encoder(cl_object stream, unsigned char *buffer, int c)
{
	cl_object byte = ecl_gethash_safe(CODE_CHAR(c), stream->stream.format_table, Cnil);
	if (Null(byte)) {
		return 0;
	} else {
		cl_fixnum code = fix(byte);
		if (code > 0xFF) {
			buffer[1] = code & 0xFF; code >>= 8;
			buffer[0] = code;
			return 2;
		} else {
			buffer[0] = code;
			return 1;
		}
	}
}

/*
 * USER DEFINED ENCODINGS. SIMPLE CASE.
 */

static int
user_multistate_decoder(cl_object stream, cl_eformat_read_byte8 read_byte8,
			cl_object source)
{
	cl_object table_list = stream->stream.format_table;
	cl_object table = ECL_CONS_CAR(table_list);
	cl_object character;
	cl_fixnum i, j;
	unsigned char buffer[ENCODING_BUFFER_MAX_SIZE];
	for (i = j = 0; i < ENCODING_BUFFER_MAX_SIZE; i++) {
		if (read_byte8(source, buffer+i, 1) < 1) {
			return EOF;
		}
		j = (j << 8) | buffer[i];
		character = ecl_gethash_safe(MAKE_FIXNUM(j), table, Cnil);
		if (CHARACTERP(character)) {
			return CHAR_CODE(character);
		}
		if (Null(character)) {
			invalid_codepoint(stream, buffer[0]);
		}
		if (character == Ct) {
			/* Need more characters */
			continue;
		}
		if (CONSP(character)) {
			/* Changed the state. */
			stream->stream.format_table = table_list = character;
			table = ECL_CONS_CAR(table_list);
			i = j = 0;
			continue;
		}
		break;
	}
	FEerror("Internal error in decoder table.", 0);
}

static int
user_multistate_encoder(cl_object stream, unsigned char *buffer, int c)
{
	cl_object table_list = stream->stream.format_table;
	cl_object p = table_list;
	do {
		cl_object table = ECL_CONS_CAR(p);
		cl_object byte = ecl_gethash_safe(CODE_CHAR(c), table, Cnil);
		if (!Null(byte)) {
			cl_fixnum code = fix(byte);
			int n = 0;
			if (p != table_list) {
				/* Must output a escape sequence */
				cl_object x = ecl_gethash_safe(Ct, table, Cnil);
				while (!Null(x)) {
					buffer[0] = fix(ECL_CONS_CAR(x));
					buffer++;
					x = ECL_CONS_CDR(x);
					n++;
				}
				stream->stream.format_table = p;
			}
			if (code > 0xFF) {
				buffer[1] = code & 0xFF; code >>= 8;
				buffer[0] = code;
				return n+2;
			} else {
				buffer[0] = code;
				return n+1;
			}
		}
		p = ECL_CONS_CDR(p);
	} while (p != table_list);
	/* Exhausted all lists */
	return 0;
}

/*
 * UTF-8
 */

static int
utf_8_decoder(cl_object stream, cl_eformat_read_byte8 read_byte8, cl_object source)
{
	/* In understanding this code:
	 * 0x8 = 1000, 0xC = 1100, 0xE = 1110, 0xF = 1111
	 * 0x1 = 0001, 0x3 = 0011, 0x7 = 0111, 0xF = 1111
	 */
	int cum = 0;
	unsigned char buffer[5];
	int nbytes, i;
	if (read_byte8(source, buffer, 1) < 1)
		return EOF;
	if ((buffer[0] & 0x80) == 0) {
		return buffer[0];
	}
	if ((buffer[0] & 0x40) == 0)
		malformed_character(stream);
	if ((buffer[0] & 0x20) == 0) {
		buffer[0] &= 0x1F;
		nbytes = 1;
	} else if ((buffer[0] & 0x10) == 0) {
		buffer[0] &= 0x0F;
		nbytes = 2;
	} else if ((buffer[0] & 0x08) == 0) {
		buffer[0] &= 0x07;
		nbytes = 3;
	} else {
		unsupported_character(stream);
	}
	if (read_byte8(source, buffer+1, nbytes) < nbytes)
		return EOF;
	for (i = 1, cum = buffer[0]; i <= nbytes; i++) {
		unsigned char c = buffer[i];
		/*printf(": %04x :", c);*/
		if ((c & 0xC0) != 0x80)
			malformed_character(stream);
		cum = (cum << 6) | (c & 0x3F);
		if (cum == 0) too_long_utf8_sequence(stream);
	}
	if (cum >= 0xd800) {
		if (cum <= 0xdfff)
			invalid_codepoint(stream, cum);
		if (cum >= 0xFFFE && cum <= 0xFFFF)
			invalid_codepoint(stream, cum);
	}
	/*printf("; %04x ;", cum);*/
	return cum;
}

static int
utf_8_encoder(cl_object stream, unsigned char *buffer, int c)
{
	int nbytes;
	if (c < 0) {
		return 0;
	} else if (c <= 0x7F) {
		buffer[0] = c;
		nbytes = 1;
	} else if (c <= 0x7ff) {
		buffer[1] = (c & 0x3f) | 0x80; c >>= 6;
		buffer[0] = c | 0xC0;
		/*printf("\n; %04x ;: %04x :: %04x :\n", c_orig, buffer[0], buffer[1]);*/
		nbytes = 2;
	} else if (c <= 0xFFFF) {
		buffer[2] = (c & 0x3f) | 0x80; c >>= 6;
		buffer[1] = (c & 0x3f) | 0x80; c >>= 6;
		buffer[0] = c | 0xE0;
		nbytes = 3;
	} else if (c <= 0x1FFFFFL) {
		buffer[3] = (c & 0x3f) | 0x80; c >>= 6;
		buffer[2] = (c & 0x3f) | 0x80; c >>= 6;
		buffer[1] = (c & 0x3f) | 0x80; c >>= 6;
		buffer[0] = c | 0xF0;
		nbytes = 4;
	}
	return nbytes;
}
#endif

/********************************************************************************
 * CLOS STREAMS
 */

static cl_index
clos_stream_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	cl_index i;
	for (i = 0; i < n; i++) {
		cl_object byte = funcall(2, @'gray::stream-read-byte', strm);
		if (!FIXNUMP(byte))
			break;
		c[i] = fix(byte);
	}
	return i;
}

static cl_index
clos_stream_write_byte8(cl_object strm, unsigned char *c, cl_index n)
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

static cl_object
clos_stream_read_byte(cl_object strm)
{
	return funcall(2, @'gray::stream-read-byte', strm);
}

static void
clos_stream_write_byte(cl_object c, cl_object strm)
{
	funcall(3, @'gray::stream-write-byte', strm, c);
	return;
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

	clos_stream_write_byte,
	clos_stream_read_byte,

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

static cl_object
str_out_element_type(cl_object strm)
{
	cl_object string = STRING_OUTPUT_STRING(strm);
	if (type_of(string) == t_base_string)
		return @'base-char';
	return @'character';
}

static cl_object
str_out_get_position(cl_object strm)
{
	return ecl_make_unsigned_integer(STRING_OUTPUT_STRING(strm)->base_string.fillp);
}

static cl_object
str_out_set_position(cl_object strm, cl_object pos)
{
	cl_object string = STRING_OUTPUT_STRING(strm);
	cl_fixnum disp;
	if (Null(pos)) {
		disp = strm->base_string.dim;
	} else {
		disp = fixnnint(pos);
	}
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

const struct ecl_file_ops str_out_ops = {
	not_output_write_byte8,
	not_binary_read_byte8,

	not_binary_write_byte,
	not_input_read_byte,

	not_input_read_char,
	str_out_write_char,
	not_input_unread_char,
	generic_peek_char,

	generic_read_vector,
	generic_write_vector,

	not_input_listen,
	not_input_clear_input,
	generic_void, /* clear-output */
	generic_void, /* finish-output */
	generic_void, /* force-output */

	generic_always_false, /* input_p */
	generic_always_true, /* output_p */
	generic_always_false,
	str_out_element_type,

	not_a_file_stream, /* length */
	str_out_get_position,
	str_out_set_position,
	str_out_column,
	generic_close
};


cl_object
si_make_string_output_stream_from_string(cl_object s)
{
	cl_object strm = alloc_stream();
	if (!ecl_stringp(s) || !s->base_string.hasfillp)
		FEerror("~S is not a -string with a fill-pointer.", 1, s);
	strm->stream.ops = duplicate_dispatch_table(&str_out_ops);
	strm->stream.mode = (short)smm_string_output;
	STRING_OUTPUT_STRING(strm) = s;
	STRING_OUTPUT_COLUMN(strm) = 0;
#ifndef ECL_UNICODE
	strm->stream.format = @':default';
	strm->stream.flags = ECL_STREAM_DEFAULT_FORMAT;
	strm->stream.byte_size = 8;
#else
	if (type_of(s) == t_base_string) {
		strm->stream.format = @':latin-1';
		strm->stream.flags = ECL_STREAM_LATIN_1;
		strm->stream.byte_size = 8;
	} else {
		strm->stream.format = @':ucs-4';
		strm->stream.flags = ECL_STREAM_UCS_4;
		strm->stream.byte_size = 32;
	}
#endif
	@(return strm)
}

cl_object
ecl_make_string_output_stream(cl_index line_length, int extended)
{
#ifdef ECL_UNICODE
	cl_object s = extended?
		ecl_alloc_adjustable_extended_string(line_length) :
		cl_alloc_adjustable_base_string(line_length);
#else
	cl_object s = cl_alloc_adjustable_base_string(line_length);
#endif
	return si_make_string_output_stream_from_string(s);
}

@(defun make-string-output-stream (&key (element_type @'character'))
	int extended = 0;
@
	if (element_type == @'base-char') {
		(void)0;
	} else if (element_type == @'character') {
#ifdef ECL_UNICODE
		extended = 1;
#endif
	} else if (!Null(funcall(3, @'subtypep', element_type, @'base-char'))) {
		(void)0;
	} else if (!Null(funcall(3, @'subtypep', element_type, @'character'))) {
#ifdef ECL_UNICODE
		extended = 1;
#endif
	} else {
		FEerror("In MAKE-STRING-OUTPUT-STREAM, the argument :ELEMENT-TYPE (~A) must be a subtype of character",
			1, element_type);
	}
	@(return ecl_make_string_output_stream(128, extended))
@)

cl_object
cl_get_output_stream_string(cl_object strm)
{
	cl_object strng;
	if (type_of(strm) != t_stream ||
	    (enum ecl_smmode)strm->stream.mode != smm_string_output)
		FEerror("~S is not a string-output stream.", 1, strm);
	strng = cl_copy_seq(STRING_OUTPUT_STRING(strm));
	STRING_OUTPUT_STRING(strm)->base_string.fillp = 0;
	@(return strng)
}

/**********************************************************************
 * STRING INPUT STREAMS
 */

static int
str_in_read_char(cl_object strm)
{
	cl_fixnum curr_pos = STRING_INPUT_POSITION(strm);
	int c;
	if (curr_pos >= STRING_INPUT_LIMIT(strm)) {
		c = EOF;
	} else {
		c = ecl_char(STRING_INPUT_STRING(strm), curr_pos);
		STRING_INPUT_POSITION(strm) = curr_pos+1;
	}
	return c;
}

static void
str_in_unread_char(cl_object strm, int c)
{
	cl_fixnum curr_pos = STRING_INPUT_POSITION(strm);
	if (c <= 0) {
		unread_error(strm);
	}
	STRING_INPUT_POSITION(strm) = curr_pos - 1;
}

static int
str_in_peek_char(cl_object strm)
{
	cl_index pos = STRING_INPUT_POSITION(strm);
	if (pos >= STRING_INPUT_LIMIT(strm)) {
		return EOF;
	} else {
		return ecl_char(STRING_INPUT_STRING(strm), pos);
	}
}

static int
str_in_listen(cl_object strm)
{
	if (STRING_INPUT_POSITION(strm) < STRING_INPUT_LIMIT(strm))
		return ECL_LISTEN_AVAILABLE;
	else
		return ECL_LISTEN_EOF;
}

static cl_object
str_in_element_type(cl_object strm)
{
	cl_object string = STRING_INPUT_STRING(strm);
	if (type_of(string) == t_base_string)
		return @'base-char';
	return @'character';
}

static cl_object
str_in_get_position(cl_object strm)
{
	return ecl_make_unsigned_integer(STRING_INPUT_POSITION(strm));
}

static cl_object
str_in_set_position(cl_object strm, cl_object pos)
{
	cl_fixnum disp;
	if (Null(pos)) {
		disp = STRING_INPUT_LIMIT(strm);
	}  else {
		disp = fixnnint(pos);
		if (disp >= STRING_INPUT_LIMIT(strm)) {
			disp = STRING_INPUT_LIMIT(strm);
		}
	}
	STRING_INPUT_POSITION(strm) = disp;
	return Ct;
}

const struct ecl_file_ops str_in_ops = {
	not_output_write_byte8,
	not_binary_read_byte8,

	not_output_write_byte,
	not_binary_read_byte,

	str_in_read_char,
	not_output_write_char,
	str_in_unread_char,
	str_in_peek_char,

	generic_read_vector,
	generic_write_vector,

	str_in_listen,
	generic_void, /* clear-input */
	not_output_clear_output,
	not_output_finish_output,
	not_output_force_output,

	generic_always_true, /* input_p */
	generic_always_false, /* output_p */
	generic_always_false,
	str_in_element_type,

	not_a_file_stream, /* length */
	str_in_get_position,
	str_in_set_position,
	generic_column,
	generic_close
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
#ifndef ECL_UNICODE
	strm->stream.format = @':default';
	strm->stream.flags = ECL_STREAM_DEFAULT_FORMAT;
	strm->stream.byte_size = 8;
#else
	if (type_of(strng) == t_base_string) {
		strm->stream.format = @':latin-1';
		strm->stream.flags = ECL_STREAM_LATIN_1;
		strm->stream.byte_size = 8;
	} else {
		strm->stream.format = @':ucs-4';
		strm->stream.flags = ECL_STREAM_UCS_4;
		strm->stream.byte_size = 32;
	}
#endif
	return strm;
}

@(defun make_string_input_stream (strng &o istart iend)
	cl_index s, e;
@
	strng = cl_string(strng);
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
two_way_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	if (strm == cl_core.terminal_io)
		ecl_force_output(TWO_WAY_STREAM_OUTPUT(cl_core.terminal_io));
	return ecl_read_byte8(TWO_WAY_STREAM_INPUT(strm), c, n);
}

static cl_index
two_way_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	return ecl_write_byte8(TWO_WAY_STREAM_OUTPUT(strm), c, n);
}

static void
two_way_write_byte(cl_object byte, cl_object stream)
{
	ecl_write_byte(byte, TWO_WAY_STREAM_OUTPUT(stream));
}

static cl_object
two_way_read_byte(cl_object stream)
{
	return ecl_read_byte(TWO_WAY_STREAM_INPUT(stream));
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

static int
two_way_column(cl_object strm)
{
	return ecl_file_column(TWO_WAY_STREAM_OUTPUT(strm));
}

const struct ecl_file_ops two_way_ops = {
	two_way_write_byte8,
	two_way_read_byte8,

	two_way_write_byte,
	two_way_read_byte,

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

	generic_always_true, /* input_p */
	generic_always_true, /* output_p */
	two_way_interactive_p,
	two_way_element_type,

	not_a_file_stream, /* length */
	generic_always_nil, /* get_position */
	generic_set_position,
	two_way_column,
	generic_close
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
	strm->stream.format = cl_stream_external_format(istrm);
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

static cl_index
broadcast_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	cl_object l;
	cl_index out = n;
	for (l = BROADCAST_STREAM_LIST(strm); !ecl_endp(l); l = ECL_CONS_CDR(l)) {
		out = ecl_write_byte8(ECL_CONS_CAR(l), c, n);
	}
	return out;
}

static int
broadcast_write_char(cl_object strm, int c)
{
	cl_object l;
	for (l = BROADCAST_STREAM_LIST(strm); !ecl_endp(l); l = ECL_CONS_CDR(l)) {
		ecl_write_char(c, ECL_CONS_CAR(l));
	}
	return c;
}

static void
broadcast_write_byte(cl_object c, cl_object strm)
{
	cl_object l;
	for (l = BROADCAST_STREAM_LIST(strm); !ecl_endp(l); l = ECL_CONS_CDR(l)) {
		ecl_write_byte(c, ECL_CONS_CAR(l));
	}
}

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

const struct ecl_file_ops broadcast_ops = {
	broadcast_write_byte8,
	not_input_read_byte8,

	broadcast_write_byte,
	not_input_read_byte,

	not_input_read_char,
	broadcast_write_char,
	not_input_unread_char,
	generic_peek_char,

	generic_read_vector,
	generic_write_vector,

	not_input_listen,
	broadcast_force_output, /* clear_input */ /* FIXME! This is legacy behaviour */
	broadcast_clear_output,
	broadcast_finish_output,
	broadcast_force_output,

	generic_always_false, /* input_p */
	generic_always_true, /* output_p */
	generic_always_false,
	broadcast_element_type,

	broadcast_length,
	broadcast_get_position,
	broadcast_set_position,
	broadcast_column,
	generic_close
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
	if (Null(streams)) {
		x->stream.format = @':default';
	} else {
		x->stream.format = cl_stream_external_format(ECL_CONS_CAR(streams));
	}
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
echo_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	cl_index out = ecl_read_byte8(ECHO_STREAM_INPUT(strm), c, n);
	return ecl_write_byte8(ECHO_STREAM_OUTPUT(strm), c, out);
}

static cl_index
echo_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	return ecl_write_byte8(ECHO_STREAM_OUTPUT(strm), c, n);
}

static void
echo_write_byte(cl_object c, cl_object strm)
{
	return ecl_write_byte(c, ECHO_STREAM_OUTPUT(strm));
}

static cl_object
echo_read_byte(cl_object strm)
{
	cl_object out = ecl_read_byte(ECHO_STREAM_INPUT(strm));
	if (!Null(out)) ecl_write_byte(out, ECHO_STREAM_OUTPUT(strm));
	return out;
}

static int
echo_read_char(cl_object strm)
{
	int c = strm->stream.last_code[0];
	if (c == EOF) {
		c = ecl_read_char(ECHO_STREAM_INPUT(strm));
		if (c != EOF)
			ecl_write_char(c, ECHO_STREAM_OUTPUT(strm));
	} else {
		strm->stream.last_code[0] = EOF;
	}
	return c;
}

static int
echo_write_char(cl_object strm, int c)
{
	return ecl_write_char(c, ECHO_STREAM_OUTPUT(strm));
}

static void
echo_unread_char(cl_object strm, int c)
{
	if (strm->stream.last_code[0] != EOF) {
		unread_twice(strm);
	}
	strm->stream.last_code[0] = c;
}

static int
echo_peek_char(cl_object strm)
{
	int c = strm->stream.last_code[0];
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

static cl_object
echo_element_type(cl_object strm)
{
	return ecl_stream_element_type(ECHO_STREAM_INPUT(strm));
}

static int
echo_column(cl_object strm)
{
	return ecl_file_column(ECHO_STREAM_OUTPUT(strm));
}

const struct ecl_file_ops echo_ops = {
	echo_write_byte8,
	echo_read_byte8,

	echo_write_byte,
	echo_read_byte,

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

	generic_always_true, /* input_p */
	generic_always_true, /* output_p */
	generic_always_false,
	echo_element_type,

	not_a_file_stream, /* length */
	generic_always_nil, /* get_position */
	generic_set_position,
	echo_column,
	generic_close
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
	strm->stream.format = cl_stream_external_format(strm1);
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
concatenated_read_byte8(cl_object strm, unsigned char *c, cl_index n)
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

static cl_object
concatenated_read_byte(cl_object strm)
{
	cl_object l = CONCATENATED_STREAM_LIST(strm);
	cl_object c = Cnil;
	while (!ecl_endp(l)) {
		c = ecl_read_byte(ECL_CONS_CAR(l));
		if (c != Cnil) break;
		CONCATENATED_STREAM_LIST(strm) = l = ECL_CONS_CDR(l);
	}
	return c;
}

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

static void
concatenated_unread_char(cl_object strm, int c)
{
	cl_object l = CONCATENATED_STREAM_LIST(strm);
	if (Null(l))
		unread_error(strm);
	return ecl_unread_char(c, ECL_CONS_CAR(l));
}

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

const struct ecl_file_ops concatenated_ops = {
	not_output_write_byte8,
	concatenated_read_byte8,

	not_output_write_byte,
	concatenated_read_byte,

	concatenated_read_char,
	not_output_write_char,
	concatenated_unread_char,
	generic_peek_char,

	generic_read_vector,
	generic_write_vector,

	concatenated_listen,
	generic_void, /* clear_input */
	not_output_clear_output,
	not_output_finish_output,
	not_output_force_output,

	generic_always_true, /* input_p */
	generic_always_false, /* output_p */
	generic_always_false,
	broadcast_element_type,

	not_a_file_stream, /* length */
	generic_always_nil, /* get_position */
	generic_set_position,
	generic_column,
	generic_close
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
	if (Null(streams)) {
		x->stream.format = @':default';
	} else {
		x->stream.format = cl_stream_external_format(ECL_CONS_CAR(streams));
	}
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
synonym_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	return ecl_read_byte8(SYNONYM_STREAM_STREAM(strm), c, n);
}

static cl_index
synonym_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	return ecl_write_byte8(SYNONYM_STREAM_STREAM(strm), c, n);
}

static void
synonym_write_byte(cl_object c, cl_object strm)
{
	return ecl_write_byte(c, SYNONYM_STREAM_STREAM(strm));
}

static cl_object
synonym_read_byte(cl_object strm)
{
	return ecl_read_byte(SYNONYM_STREAM_STREAM(strm));
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

const struct ecl_file_ops synonym_ops = {
	synonym_write_byte8,
	synonym_read_byte8,

	synonym_write_byte,
	synonym_read_byte,

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
	generic_close
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
 * POSIX FILE STREAM
 */

static cl_index
io_file_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	cl_object l = strm->stream.byte_stack;
	if (l != Cnil) {
		cl_index out = 0;
		do {
			*c = fix(ECL_CONS_CAR(l));
			l = ECL_CONS_CDR(l);
			out++;
			c++;
			n--;
		} while (l != Cnil);
		strm->stream.byte_stack = Cnil;
		return out + io_file_read_byte8(strm, c, n);
	} else {
		int f = IO_FILE_DESCRIPTOR(strm);
		cl_fixnum out = 0;
		ecl_disable_interrupts();
		do {
			out = read(f, c, sizeof(char)*n);
		} while (out < 0 && restartable_io_error(strm));
		ecl_enable_interrupts();
		return out;
	}
}

static cl_index
output_file_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	int f = IO_FILE_DESCRIPTOR(strm);
	cl_index out;
	ecl_disable_interrupts();
	do {
		out = write(f, c, sizeof(char)*n);
	} while (out < 0 && restartable_io_error(strm));
	ecl_enable_interrupts();
	return out;
}

static cl_index
io_file_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	if (strm->stream.byte_stack != Cnil) {
		/* Try to move to the beginning of the unread characters */
		cl_object aux = ecl_file_position(strm);
		if (!Null(aux))
			ecl_file_position_set(strm, aux);
		strm->stream.byte_stack = Cnil;
	}
	return output_file_write_byte8(strm, c, n);
}

static int
io_file_listen(cl_object strm)
{
	if (strm->stream.byte_stack != Cnil)
		return ECL_LISTEN_AVAILABLE;
	if (strm->stream.flags & ECL_STREAM_MIGHT_SEEK) {
		cl_env_ptr the_env = ecl_process_env();
		int f = IO_FILE_DESCRIPTOR(strm);
		ecl_off_t disp, new;
		ecl_disable_interrupts_env(the_env);
		disp = lseek(f, 0, SEEK_CUR);
		ecl_enable_interrupts_env(the_env);
		if (disp != (ecl_off_t)-1) {
			ecl_disable_interrupts_env(the_env);
			new = lseek(f, 0, SEEK_END);
			ecl_enable_interrupts_env(the_env);
			lseek(f, disp, SEEK_SET);
			if (new == disp) {
				return ECL_LISTEN_NO_CHAR;
			} else if (new != (ecl_off_t)-1) {
				return ECL_LISTEN_AVAILABLE;
			}
		}
	}
	return file_listen(IO_FILE_DESCRIPTOR(strm));
}

static void
io_file_clear_input(cl_object strm)
{
	int f = IO_FILE_DESCRIPTOR(strm);
#if defined(mingw32) || defined(_MSC_VER)
	if (isatty(f)) {
		/* Flushes Win32 console */
		if (!FlushConsoleInputBuffer((HANDLE)_get_osfhandle(fileno(fp))))
			FEwin32_error("FlushConsoleInputBuffer() failed", 0);
		/* Do not stop here: the FILE structure needs also to be flushed */
	}
#endif
	while (file_listen(f) == ECL_LISTEN_AVAILABLE) {
		eformat_read_char(strm);
	}
}

#define io_file_clear_output generic_void
#define io_file_force_output generic_void
#define io_file_finish_output io_file_force_output

static int
io_file_interactive_p(cl_object strm)
{
	int f = IO_FILE_DESCRIPTOR(strm);
	return isatty(f);
}

static cl_object
io_file_element_type(cl_object strm)
{
	return IO_FILE_ELT_TYPE(strm);
}

static cl_object
io_file_length(cl_object strm)
{
	int f = IO_FILE_DESCRIPTOR(strm);
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
io_file_get_position(cl_object strm)
{
	int f = IO_FILE_DESCRIPTOR(strm);
	cl_object output;
	ecl_off_t offset;

	ecl_disable_interrupts();
	offset = lseek(f, 0, SEEK_CUR);
	ecl_enable_interrupts();
	if (offset < 0)
		io_error(strm);
	if (sizeof(ecl_off_t) == sizeof(long)) {
		output = ecl_make_integer(offset);
	} else {
		output = ecl_off_t_to_integer(offset);
	}
	{
		/* If there are unread octets, we return the position at which
		 * these bytes begin! */
		cl_object l = strm->stream.byte_stack;
		while (CONSP(l)) {
			output = ecl_one_minus(output);
			l = ECL_CONS_CDR(l);
		}
	}
	if (strm->stream.byte_size != 8) {
		output = ecl_floor2(output, MAKE_FIXNUM(strm->stream.byte_size / 8));
	}
	return output;
}

static cl_object
io_file_set_position(cl_object strm, cl_object large_disp)
{
	int f = IO_FILE_DESCRIPTOR(strm);
	ecl_off_t disp;
	int mode;
	if (Null(large_disp)) {
		disp = 0;
		mode = SEEK_END;
	} else {
		if (strm->stream.byte_size != 8) {
			large_disp = ecl_times(large_disp,
					       MAKE_FIXNUM(strm->stream.byte_size / 8));
		}
		disp = ecl_integer_to_off_t(large_disp);
		mode = SEEK_SET;
	}
	disp = lseek(f, disp, mode);
	return (disp == (ecl_off_t)-1)? Cnil : Ct;
}

static int
io_file_column(cl_object strm)
{
	return IO_FILE_COLUMN(strm);
}

static cl_object
io_file_close(cl_object strm)
{
	int f = IO_FILE_DESCRIPTOR(strm);
	int failed;
	if (f == STDOUT_FILENO)
		FEerror("Cannot close the standard output", 0);
	if (f == STDIN_FILENO)
		FEerror("Cannot close the standard input", 0);
	ecl_disable_interrupts();
	failed = close(f);
	ecl_enable_interrupts();
	if (failed < 0)
		FElibc_error("Cannot close stream ~S.", 1, strm);
	strm->stream.file = (void*)-1;
	return generic_close(strm);
}

static cl_index
io_file_read_vector(cl_object strm, cl_object data, cl_index start, cl_index end)
{
	cl_elttype t = data->vector.elttype;
	if (start >= end)
		return start;
	if (t == aet_b8 || t == aet_i8 || t == aet_bc) {
		if (strm->stream.byte_size == 8) {
			void *aux = data->vector.self.ch + start;
			return strm->stream.ops->read_byte8(strm, aux, end-start);
		}
	} else if (t == aet_fix || t == aet_index) {
		if (strm->stream.byte_size == sizeof(cl_fixnum)*8) {
			void *aux = data->vector.self.fix + start;
			cl_index bytes = (end - start) * sizeof(cl_fixnum);
			bytes = strm->stream.ops->read_byte8(strm, aux, bytes);
			return start + bytes / sizeof(cl_fixnum);
		}
	}
	return generic_read_vector(strm, data, start, end);
}

static cl_index
io_file_write_vector(cl_object strm, cl_object data, cl_index start, cl_index end)
{
	cl_elttype t = data->vector.elttype;
	if (start >= end)
		return start;
	if (t == aet_b8 || t == aet_i8 || t == aet_bc) {
		if (strm->stream.byte_size == 8) {
			void *aux = data->vector.self.fix + start;
			return strm->stream.ops->write_byte8(strm, aux, end-start);
		}
	} else if (t == aet_fix || t == aet_index) {
		if (strm->stream.byte_size == sizeof(cl_fixnum)*8) {
			void *aux = data->vector.self.fix + start;
			cl_index bytes = (end - start) * sizeof(cl_fixnum);
			bytes = strm->stream.ops->write_byte8(strm, aux, bytes);
			return start + bytes / sizeof(cl_fixnum);
		}
	}
	return generic_write_vector(strm, data, start, end);
}

const struct ecl_file_ops io_file_ops = {
	io_file_write_byte8,
	io_file_read_byte8,

	generic_write_byte,
	generic_read_byte,

	eformat_read_char,
	eformat_write_char,
	eformat_unread_char,
	generic_peek_char,

	io_file_read_vector,
	io_file_write_vector,

	io_file_listen,
	io_file_clear_input,
	io_file_clear_output,
	io_file_finish_output,
	io_file_force_output,

	generic_always_true, /* input_p */
	generic_always_true, /* output_p */
	io_file_interactive_p,
	io_file_element_type,

	io_file_length,
	io_file_get_position,
	io_file_set_position,
	io_file_column,
	io_file_close
};

const struct ecl_file_ops output_file_ops = {
	output_file_write_byte8,
	not_input_read_byte8,

	generic_write_byte,
	not_input_read_byte,

	not_input_read_char,
	eformat_write_char,
	not_input_unread_char,
	not_input_read_char,

	generic_read_vector,
	io_file_write_vector,

	not_input_listen,
	not_input_clear_input,
	io_file_clear_output,
	io_file_finish_output,
	io_file_force_output,

	generic_always_false, /* input_p */
	generic_always_true, /* output_p */
	generic_always_false,
	io_file_element_type,

	io_file_length,
	io_file_get_position,
	io_file_set_position,
	io_file_column,
	io_file_close
};

const struct ecl_file_ops input_file_ops = {
	not_output_write_byte8,
	io_file_read_byte8,

	not_output_write_byte,
	generic_read_byte,

	eformat_read_char,
	not_output_write_char,
	eformat_unread_char,
	generic_peek_char,

	io_file_read_vector,
	generic_write_vector,

	io_file_listen,
	io_file_clear_input,
	not_output_clear_output,
	not_output_finish_output,
	not_output_force_output,

	generic_always_true, /* input_p */
	generic_always_false, /* output_p */
	io_file_interactive_p,
	io_file_element_type,

	io_file_length,
	io_file_get_position,
	io_file_set_position,
	generic_column,
	io_file_close
};


static int
parse_external_format(cl_object stream, cl_object format, int flags)
{
	int aux;
	if (CONSP(format)) {
		flags = parse_external_format(stream, ECL_CONS_CDR(format), flags);
		format = ECL_CONS_CAR(format);
	}
	if (format == Cnil) {
		return flags;
	}
	if (format == @':CR') {
		return (flags | ECL_STREAM_CR) & ~ECL_STREAM_LF;
	}
	if (format == @':LF') {
		return (flags | ECL_STREAM_LF) & ~ECL_STREAM_CR;
	}
	if (format == @':CRLF') {
		return flags | (ECL_STREAM_CR+ECL_STREAM_LF);
	}
	if (format == @':LITTLE-ENDIAN') {
		return flags | ECL_STREAM_LITTLE_ENDIAN;
	}
	if (format == @':BIG-ENDIAN') {
		return flags & ~ECL_STREAM_LITTLE_ENDIAN;
	}
	if (format == @':default' || format == Ct) {
		return flags | ECL_STREAM_DEFAULT_FORMAT;
	}
#ifdef ECL_UNICODE
	if (format == @':UTF-8') {
		return flags | ECL_STREAM_UTF_8; 
	}
	if (format == @':UCS-2') {
		return flags | ECL_STREAM_UCS_2;
	}
	if (format == @':UCS-2BE') {
		return flags | ECL_STREAM_UCS_2BE;
	}
	if (format == @':UCS-2LE') {
		return flags | ECL_STREAM_UCS_2LE;
	}
	if (format == @':UCS-4') {
		return flags | ECL_STREAM_UCS_4;
	}
	if (format == @':UCS-4BE') {
		return flags | ECL_STREAM_UCS_4BE;
	}
	if (format == @':UCS-4LE') {
		return flags | ECL_STREAM_UCS_4LE;
	}
	if (format == @':ISO-8859-1') {
		return flags | ECL_STREAM_ISO_8859_1;
	}
	if (format == @':LATIN-1') {
		return flags | ECL_STREAM_LATIN_1;
	}
	if (format == @':US-ASCII') {
		return flags | ECL_STREAM_US_ASCII; 
	}
	if (type_of(format) == t_hashtable) {
		stream->stream.format_table = format;
		return flags | ECL_STREAM_USER_FORMAT;
	}
	if (SYMBOLP(format)) {
		stream->stream.format_table = cl_funcall(2, @'ext::make-encoding',
							 format);
		return flags | ECL_STREAM_USER_FORMAT;
	}
#endif
	FEerror("Unknown or unsupported external format: ~A", 1, format);
	return ECL_STREAM_DEFAULT_FORMAT;
}

static void
set_stream_elt_type(cl_object stream, cl_fixnum byte_size, int flags,
		    cl_object external_format)
{
	cl_object t;
	if (byte_size < 0) {
		byte_size = -byte_size;
		flags |= ECL_STREAM_SIGNED_BYTES;
		t = @'signed-byte';
	} else {
		flags &= ~ECL_STREAM_SIGNED_BYTES;
		t = @'unsigned-byte';
	}
	flags = parse_external_format(stream, external_format, flags);
	stream->stream.ops->read_char = eformat_read_char;
	stream->stream.ops->write_char = eformat_write_char;
	switch (flags & ECL_STREAM_FORMAT) {
	case ECL_STREAM_BINARY:
		IO_STREAM_ELT_TYPE(stream) = cl_list(2, t, MAKE_FIXNUM(byte_size));
		stream->stream.format = t;
		stream->stream.ops->read_char = not_character_read_char;
		stream->stream.ops->write_char = not_character_write_char;
		break;
#ifdef ECL_UNICODE
	/*case ECL_ISO_8859_1:*/
	case ECL_STREAM_LATIN_1:
		IO_STREAM_ELT_TYPE(stream) = @'base-char';
		byte_size = 8;
		stream->stream.format = @':latin-1';
		stream->stream.encoder = passthrough_encoder;
		stream->stream.decoder = passthrough_decoder;
		break;
	case ECL_STREAM_UTF_8:
		IO_STREAM_ELT_TYPE(stream) = @'character';
		byte_size = 8;
		stream->stream.format = @':utf-8';
		stream->stream.encoder = utf_8_encoder;
		stream->stream.decoder = utf_8_decoder;
		break;
	case ECL_STREAM_UCS_2:
		IO_STREAM_ELT_TYPE(stream) = @'character';
		byte_size = 8*2;
		stream->stream.format = @':ucs-2';
		stream->stream.encoder = ucs_2_encoder;
		stream->stream.decoder = ucs_2_decoder;
		break;
	case ECL_STREAM_UCS_2BE:
		IO_STREAM_ELT_TYPE(stream) = @'character';
		byte_size = 8*2;
		if (flags & ECL_STREAM_LITTLE_ENDIAN) {
			stream->stream.format = @':ucs-2le';
			stream->stream.encoder = ucs_2le_encoder;
			stream->stream.decoder = ucs_2le_decoder;
		} else {
			stream->stream.format = @':ucs-2be';
			stream->stream.encoder = ucs_2be_encoder;
			stream->stream.decoder = ucs_2be_decoder;
		}
		break;
	case ECL_STREAM_UCS_4:
		IO_STREAM_ELT_TYPE(stream) = @'character';
		byte_size = 8*4;
		stream->stream.format = @':ucs-4be';
		stream->stream.encoder = ucs_4_encoder;
		stream->stream.decoder = ucs_4_decoder;
		break;
	case ECL_STREAM_UCS_4BE:
		IO_STREAM_ELT_TYPE(stream) = @'character';
		byte_size = 8*4;
		if (flags & ECL_STREAM_LITTLE_ENDIAN) {
			stream->stream.format = @':ucs-4le';
			stream->stream.encoder = ucs_4le_encoder;
			stream->stream.decoder = ucs_4le_decoder;
		} else {
			stream->stream.format = @':ucs-4be';
			stream->stream.encoder = ucs_4be_encoder;
			stream->stream.decoder = ucs_4be_decoder;
		}
		break;
	case ECL_STREAM_USER_FORMAT:
		IO_STREAM_ELT_TYPE(stream) = @'character';
		byte_size = 8;
		stream->stream.format = stream->stream.format_table;
		if (CONSP(stream->stream.format)) {
			stream->stream.encoder = user_multistate_encoder;
			stream->stream.decoder = user_multistate_decoder;
		} else {
			stream->stream.encoder = user_encoder;
			stream->stream.decoder = user_decoder;
		}
		break;
	case ECL_STREAM_US_ASCII:
		IO_STREAM_ELT_TYPE(stream) = @'base-char';
		byte_size = 8;
		stream->stream.format = @':us-ascii';
		stream->stream.encoder = ascii_encoder;
		stream->stream.decoder = ascii_decoder;
		break;
#else
	case ECL_STREAM_DEFAULT_FORMAT:
		IO_STREAM_ELT_TYPE(stream) = @'base-char';
		byte_size = 8;
		stream->stream.format = @':default';
		stream->stream.encoder = passthrough_encoder;
		stream->stream.decoder = passthrough_decoder;
		break;
#endif
	default:
		FEerror("Invalid or unsupported external format ~A with code ~D",
			2, external_format, MAKE_FIXNUM(flags));
 	}
	t = @':LF';
	if (stream->stream.ops->write_char == eformat_write_char &&
	    (flags & ECL_STREAM_CR)) {
		cl_object key;
		if (flags & ECL_STREAM_LF) {
			stream->stream.ops->read_char = eformat_read_char_crlf;
			stream->stream.ops->write_char = eformat_write_char_crlf;
			t = @':CRLF';
		} else {
			stream->stream.ops->read_char = eformat_read_char_cr;
			stream->stream.ops->write_char = eformat_write_char_cr;
			t = @':CR';
		}
	}
	stream->stream.format = cl_list(2, stream->stream.format, t);
	{
		cl_object (*read_byte)(cl_object);
		void (*write_byte)(cl_object,cl_object);
		byte_size = (byte_size+7)&(~(cl_fixnum)7);
		if (byte_size == 8) {
			if (flags & ECL_STREAM_SIGNED_BYTES) {
				read_byte = generic_read_byte_signed8;
				write_byte = generic_write_byte_signed8;
			} else {
				read_byte = generic_read_byte_unsigned8;
				write_byte = generic_write_byte_unsigned8;
			}
		} else if (flags & ECL_STREAM_LITTLE_ENDIAN) {
			read_byte = generic_read_byte_le;
			write_byte = generic_write_byte_le;
		} else {
			read_byte = generic_read_byte;
			write_byte = generic_write_byte;
		}
		if (ecl_input_stream_p(stream)) {
			stream->stream.ops->read_byte = read_byte;
		}
		if (ecl_output_stream_p(stream)) {
			stream->stream.ops->write_byte = write_byte;
		}
	}
	stream->stream.flags = flags;
	stream->stream.byte_size = byte_size;
}

cl_object
ecl_make_file_stream_from_fd(cl_object fname, int fd, enum ecl_smmode smm,
			     cl_fixnum byte_size, int flags, cl_object external_format)
{
	cl_object stream = alloc_stream();
	stream->stream.mode = (short)smm;
	stream->stream.closed = 0;
#if defined (ECL_WSOCK)
	if (smm == smm_input_wsock || smm == smm_io_wsock)
		character_p = 1;
#endif
	switch(smm) {
	case smm_probe:
	case smm_input:
		smm = smm_input_file;
	case smm_input_file:
		stream->stream.ops = duplicate_dispatch_table(&input_file_ops);
		break;
	case smm_output:
		smm = smm_output_file;
	case smm_output_file:
		stream->stream.ops = duplicate_dispatch_table(&output_file_ops);
		break;
	case smm_io:
		smm = smm_io_file;
	case smm_io_file:
		stream->stream.ops = duplicate_dispatch_table(&io_file_ops);
		break;
	default:
		FEerror("make_stream: wrong mode", 0);
	}
	set_stream_elt_type(stream, byte_size, flags, external_format);
	IO_FILE_FILENAME(stream) = fname; /* not really used */
	IO_FILE_COLUMN(stream) = 0;
	stream->stream.file = (void*)fd;
	stream->stream.last_op = 0;
	si_set_finalizer(stream, Ct);
	return stream;
}

/**********************************************************************
 * C STREAMS
 */

static cl_index
input_stream_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	cl_object l = strm->stream.byte_stack;
	if (l != Cnil) {
		cl_index out = 0;
		do {
			*c = fix(ECL_CONS_CAR(l));
			l = ECL_CONS_CDR(l);
			out++;
			c++;
			n--;
		} while (l != Cnil);
		strm->stream.byte_stack = Cnil;
		return out + input_stream_read_byte8(strm, c, n);
	} else {
		FILE *f = IO_STREAM_FILE(strm);
		cl_index out = 0;
		ecl_disable_interrupts();
		do {
			out = fread(c, sizeof(char), n, f);
		} while (out < n && ferror(f) && restartable_io_error(strm));
		ecl_enable_interrupts();
		return out;
	}
}

static cl_index
output_stream_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	cl_index out;
	ecl_disable_interrupts();
	do {
		out = fwrite(c, sizeof(char), n, IO_STREAM_FILE(strm));
	} while (out < n && restartable_io_error(strm));
	ecl_enable_interrupts();
	return out;
}

static cl_index
io_stream_write_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	/* When using the same stream for input and output operations, we have to
	 * use some file position operation before reading again. Besides this, if
	 * there were unread octets, we have to move to the position at the
	 * begining of them.
	 */
	if (strm->stream.byte_stack != Cnil) {
		cl_object aux = ecl_file_position(strm);
		if (!Null(aux))
			ecl_file_position_set(strm, aux);
	} else if (strm->stream.last_op > 0) {
		ecl_fseeko((FILE*)strm->stream.file, 0, SEEK_CUR);
	}
	strm->stream.last_op = -1;
	return input_stream_read_byte8(strm, c, n);
}

static void io_stream_force_output(cl_object strm);

static cl_index
io_stream_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	/* When using the same stream for input and output operations, we have to
	 * flush the stream before reading.
	 */
	if (strm->stream.last_op < 0) {
		io_stream_force_output(strm);
	}
	strm->stream.last_op = +1;
	return output_stream_write_byte8(strm, c, n);
}

static int
io_stream_listen(cl_object strm)
{
	if (strm->stream.byte_stack != Cnil)
		return ECL_LISTEN_AVAILABLE;
	return flisten(IO_STREAM_FILE(strm));
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

static int
io_stream_interactive_p(cl_object strm)
{
	FILE *f = IO_STREAM_FILE(strm);
	return isatty(fileno(f));
}

static cl_object
io_stream_length(cl_object strm)
{
	FILE *f = IO_STREAM_FILE(strm);
	cl_object output = ecl_file_len(fileno(f));
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
	{
		/* If there are unread octets, we return the position at which
		 * these bytes begin! */
		cl_object l = strm->stream.byte_stack;
		while (CONSP(l)) {
			output = ecl_one_minus(output);
			l = ECL_CONS_CDR(l);
		}
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
	int mode;
	if (Null(large_disp)) {
		disp = 0;
		mode = SEEK_END;
	} else {
		if (strm->stream.byte_size != 8) {
			large_disp = ecl_times(large_disp,
					       MAKE_FIXNUM(strm->stream.byte_size / 8));
		}
		disp = ecl_integer_to_off_t(large_disp);
		mode = SEEK_SET;
	}
	ecl_disable_interrupts();
	mode = ecl_fseeko(f, disp, mode);
	ecl_enable_interrupts();
	return mode? Cnil : Ct;
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

#define io_stream_read_vector io_file_read_vector
#define io_stream_write_vector io_file_write_vector

const struct ecl_file_ops io_stream_ops = {
	io_stream_write_byte8,
	io_stream_read_byte8,

	generic_write_byte,
	generic_read_byte,

	eformat_read_char,
	eformat_write_char,
	eformat_unread_char,
	generic_peek_char,

	io_file_read_vector,
	io_file_write_vector,

	io_stream_listen,
	io_stream_clear_input,
	io_stream_clear_output,
	io_stream_finish_output,
	io_stream_force_output,

	generic_always_true, /* input_p */
	generic_always_true, /* output_p */
	io_stream_interactive_p,
	io_file_element_type,

	io_stream_length,
	io_stream_get_position,
	io_stream_set_position,
	io_stream_column,
	io_stream_close
};

const struct ecl_file_ops output_stream_ops = {
	output_stream_write_byte8,
	not_input_read_byte8,

	generic_write_byte,
	not_input_read_byte,

	not_input_read_char,
	eformat_write_char,
	not_input_unread_char,
	not_input_read_char,

	generic_read_vector,
	io_file_write_vector,

	not_input_listen,
	generic_void,
	io_stream_clear_output,
	io_stream_finish_output,
	io_stream_force_output,

	generic_always_false, /* input_p */
	generic_always_true, /* output_p */
	generic_always_false,
	io_file_element_type,

	io_stream_length,
	io_stream_get_position,
	io_stream_set_position,
	io_stream_column,
	io_stream_close
};

const struct ecl_file_ops input_stream_ops = {
	not_output_write_byte8,
	input_stream_read_byte8,

	not_output_write_byte,
	generic_read_byte,

	eformat_read_char,
	not_output_write_char,
	eformat_unread_char,
	generic_peek_char,

	io_file_read_vector,
	generic_write_vector,

	io_stream_listen,
	io_stream_clear_input,
	generic_void,
	generic_void,
	generic_void,

	generic_always_true, /* input_p */
	generic_always_false, /* output_p */
	io_stream_interactive_p,
	io_file_element_type,

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
			  cl_fixnum byte_size, int flags, cl_object external_format)
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
	set_stream_elt_type(stream, byte_size, flags, external_format);
	IO_STREAM_FILENAME(stream) = fname; /* not really used */
	IO_STREAM_COLUMN(stream) = 0;
	stream->stream.file = f;
	stream->stream.last_op = 0;
	si_set_finalizer(stream, Ct);
	return stream;
}

cl_object
ecl_make_stream_from_fd(cl_object fname, int fd, enum ecl_smmode smm,
			cl_fixnum byte_size, int flags, cl_object external_format)
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
	return ecl_make_stream_from_FILE(fname, fp, smm, byte_size, flags,
					 external_format);
}


int
ecl_stream_to_handle(cl_object s, bool output)
{
 BEGIN:
	if (type_of(s) != t_stream)
		return -1;
	switch ((enum ecl_smmode)s->stream.mode) {
	case smm_input:
		if (output) return -1;
		return fileno((FILE*)s->stream.file);
	case smm_input_file:
		if (output) return -1;
		return (int)s->stream.file;
	case smm_output:
		if (!output) return -1;
		return fileno((FILE*)s->stream.file);
	case smm_output_file:
		if (!output) return -1;
		return (int)s->stream.file;
	case smm_io:
		return fileno((FILE*)s->stream.file);
	case smm_io_file:
		return (int)s->stream.file;
	case smm_synonym:
		s = SYNONYM_STREAM_STREAM(s);
		goto BEGIN;
	case smm_two_way:
		s = output? TWO_WAY_STREAM_OUTPUT(s) : TWO_WAY_STREAM_INPUT(s);
		goto BEGIN;
	default:
		ecl_internal_error("illegal stream mode");
	}
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
ecl_read_byte8(cl_object strm, unsigned char *c, cl_index n)
{
	return stream_dispatch_table(strm)->read_byte8(strm, c, n);
}

static cl_index
ecl_write_byte8(cl_object strm, unsigned char *c, cl_index n)
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
	return stream_dispatch_table(strm)->read_byte(strm);
}

void
ecl_write_byte(cl_object c, cl_object strm)
{
	return stream_dispatch_table(strm)->write_byte(c, strm);
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

static cl_index
compute_char_size(cl_object stream, int c)
{
	unsigned char buffer[5];
	int l = 0;
	if (c == ECL_CHAR_CODE_NEWLINE) {
		int flags = stream->stream.flags;
		if (flags & ECL_STREAM_CR) {
			l += stream->stream.encoder(stream, buffer, ECL_CHAR_CODE_RETURN);
			if (flags & ECL_STREAM_LF)
				l += stream->stream.encoder(stream, buffer,
							    ECL_CHAR_CODE_LINEFEED);
		} else {
			l += stream->stream.encoder(stream, buffer, ECL_CHAR_CODE_LINEFEED);
		}
	} else {
		l += stream->stream.encoder(stream, buffer, c);
	}
	return l;
}

cl_object
cl_file_string_length(cl_object stream, cl_object string)
{
	cl_fixnum l = 0;
	/* This is a stupid requirement from the spec. Why returning 1???
	 * Why not simply leaving the value unspecified, as with other
	 * streams one cannot write to???
	 */
 BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(stream)) {
		@(return Cnil)
	}
#endif
	if (type_of(stream) != t_stream) {
		not_a_file_stream(stream);
	}
	if (stream->stream.mode == smm_broadcast) {
		stream = BROADCAST_STREAM_LIST(stream);
		if (ecl_endp(stream)) {
			@(return MAKE_FIXNUM(1));
		} else {
			goto BEGIN;
		}
	}
	if (!ECL_FILE_STREAMP(stream)) {
		not_a_file_stream(stream);
	}
	switch (type_of(string)) {
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string: {
		cl_index i;
		for (i = 0; i < string->base_string.fillp; i++) {
			l += compute_char_size(stream, ecl_char(string, i));
		}
		break;
	}
	case t_character:
		l = compute_char_size(stream, CHAR_CODE(string));
		break;
	default:
		FEwrong_type_argument(@'string', string);
	}
	@(return MAKE_FIXNUM(l))
}

cl_object
si_do_write_sequence(cl_object seq, cl_object stream, cl_object s, cl_object e)
{
	const struct ecl_file_ops *ops;
	cl_fixnum start,limit,end;

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
	ops = stream_dispatch_table(stream);
	if (LISTP(seq)) {
		cl_object elt_type = cl_stream_element_type(stream);
		bool ischar = (elt_type == @'base-char') || (elt_type == @'character');
		cl_object s = ecl_nthcdr(start, seq);
		loop_for_in(s) {
			if (start < end) {
				cl_object elt = CAR(s);
				if (ischar)
					ops->write_char(stream, ecl_char_code(elt));
				else
					ops->write_byte(elt, stream);
				start++;
			} else {
				goto OUTPUT;
			}
		} end_loop_for_in;
	} else {
		ops->write_vector(stream, seq, start, end);
	}
 OUTPUT:
	@(return seq);
}

cl_object
si_do_read_sequence(cl_object seq, cl_object stream, cl_object s, cl_object e)
{
	const struct ecl_file_ops *ops;
	cl_fixnum start,limit,end;

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
	ops = stream_dispatch_table(stream);
	if (LISTP(seq)) {
		cl_object elt_type = cl_stream_element_type(stream);
		bool ischar = (elt_type == @'base-char') || (elt_type == @'character');
		seq = ecl_nthcdr(start, seq);
		loop_for_in(seq) {
			if (start >= end) {
				goto OUTPUT;
			} else {
				cl_object c;
				if (ischar) {
					int i = ops->read_char(stream);
					if (i < 0) goto OUTPUT;
					c = CODE_CHAR(i);
				} else {
					c = ops->read_byte(stream);
					if (c == Cnil) goto OUTPUT;
				}
				ECL_RPLACA(seq, c);
				start++;
			}
		} end_loop_for_in;
	} else {
		start = ops->read_vector(stream, seq, start, end);
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
			position = Cnil;
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
	cl_type t;
 AGAIN:
	t= type_of(strm);
#ifdef ECL_CLOS_STREAMS
	if (t == t_instance)
		output = @':default';
	else
#endif
	if (t != t_stream)
		FEwrong_type_argument(@'stream', strm);
	if (strm->stream.mode == smm_synonym) {
		strm = SYNONYM_STREAM_STREAM(strm);
		goto AGAIN;
	}
	output = strm->stream.format;
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
	for (size = 8; 1; size++) {
		cl_object type;
		type = cl_list(2, sign>0? @'unsigned-byte' : @'signed-byte',
			       MAKE_FIXNUM(size));
		if (funcall(3, @'subtypep', element_type, type) != Cnil) {
			return size * sign;
		}
	}
	FEerror("Not a valid stream element type: ~A", 1, element_type);
}

cl_object
ecl_open_stream(cl_object fn, enum ecl_smmode smm, cl_object if_exists,
		cl_object if_does_not_exist, cl_fixnum byte_size,
		int flags, cl_object external_format)
{
	cl_env_ptr the_env = &cl_env;
	cl_object x;
	int f;
	mode_t mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
	cl_object filename = si_coerce_to_filename(fn);
	char *fname = (char*)filename->base_string.self;
	bool appending = 0;

	ecl_disable_interrupts_env(the_env);
	if (smm == smm_input || smm == smm_probe) {
		f = open(fname, O_RDONLY, mode);
		if (f < 0) {
			if (if_does_not_exist == @':error') {
				goto CANNOT_OPEN;
			} else if (if_does_not_exist == @':create') {
				f = open(fname, O_WRONLY|O_CREAT, mode);
				if (f < 0) goto CANNOT_OPEN;
				close(f);
				f = open(fname, O_RDONLY, mode);
				if (f < 0) goto CANNOT_OPEN;
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
		int base = (smm == smm_output)? O_WRONLY : O_RDWR;
		if (if_exists == @':new_version' && if_does_not_exist == @':create')
			goto CREATE;
		f = open(fname, O_RDONLY, mode);
		if (f >= 0) {
			close(f);
			if (if_exists == @':error') {
				goto CANNOT_OPEN;
			} else if (if_exists == @':rename') {
				f = ecl_backup_open(fname, base|O_CREAT, mode);
				if (f < 0) goto CANNOT_OPEN;
			} else if (if_exists == @':rename_and_delete' ||
				   if_exists == @':new_version' ||
				   if_exists == @':supersede') {
				f = open(fname, base|O_TRUNC, mode);
				if (f < 0) goto CANNOT_OPEN;
			} else if (if_exists == @':overwrite' || if_exists == @':append') {
				f = open(fname, base, mode);
				if (f < 0) goto CANNOT_OPEN;
				appending = (if_exists == @':append');
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
			CREATE:	f = open(fname, base | O_CREAT | O_TRUNC, mode);
				if (f < 0) goto CANNOT_OPEN;
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
	ecl_enable_interrupts_env(the_env);
	if (flags & ECL_STREAM_C_STREAM) {
		FILE *fp;
		switch (smm) {
		case smm_input:		fp = fdopen(f, OPEN_R); break;
		case smm_output:	fp = fdopen(f, OPEN_W); break;
		case smm_io:		fp = fdopen(f, OPEN_RW); break;
		}
		x = ecl_make_stream_from_FILE(fn, fp, smm, byte_size, flags,
					      external_format);
		si_set_buffering_mode(x, byte_size? @':fully-buffered' : @':line-buffered');
	} else {
		x = ecl_make_file_stream_from_fd(fn, f, smm, byte_size, flags,
						 external_format);
	}
	if (smm == smm_probe) {
		cl_close(1, x);
	} else {
		x->stream.flags |= ECL_STREAM_MIGHT_SEEK;
		si_set_finalizer(x, Ct);
		/* Set file pointer to the correct position */
		ecl_file_position_set(x, appending? Cnil : MAKE_FIXNUM(0));
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
		   (cstream Cnil)
	      &aux strm)
	enum ecl_smmode smm;
	int flags = 0;
	cl_fixnum byte_size;
@
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
	if (element_type == @'signed-byte') {
		byte_size = -8;
	} else if (element_type == @'unsigned-byte') {
		byte_size = 8;
	} else if (element_type == @':default') {
		byte_size = 0;
	} else if (funcall(3, @'subtypep', element_type, @'character') != Cnil) {
		byte_size = 0;
	} else {
		byte_size = normalize_stream_element_type(element_type);
	}
	if (byte_size != 0) {
		if (flags & ECL_STREAM_FORMAT) {
			FEerror("Cannot specify a character external format for binary streams.", 0);
		}
		external_format = Cnil;
	}
	if (!Null(cstream)) {
		flags |= ECL_STREAM_C_STREAM;
	}
	strm = ecl_open_stream(filename, smm, if_exists, if_does_not_exist,
			       byte_size, flags, external_format);
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
file_listen(int fileno)
{
#if !defined(mingw32) && !defined(_MSC_VER)
# if defined(HAVE_SELECT)
	fd_set fds;
	int retv, fd;
	struct timeval tv = { 0, 0 };
	FD_ZERO(&fds);
	FD_SET(fileno, &fds);
	retv = select(fileno + 1, &fds, NULL, NULL, &tv);
	if (retv < 0)
		FElibc_error("select() returned an error value", 0);
	else if (retv > 0)
		return ECL_LISTEN_AVAILABLE;
	else
		return ECL_LISTEN_NO_CHAR;
# elif defined(FIONREAD)
	{
		long c = 0;
		ioctl(fileno, FIONREAD, &c);
		return (c > 0)? ECL_LISTEN_AVAILABLE : ECL_LISTEN_NO_CHAR;
	}
# endif /* FIONREAD */
#else
	HANDLE hnd = (HANDLE)_get_osfhandle(fileno);
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
	return -3;
}

static int
flisten(FILE *fp)
{
	int aux;
	if (feof(fp))
		return ECL_LISTEN_EOF;
#ifdef FILE_CNT
	if (FILE_CNT(fp) > 0)
		return ECL_LISTEN_AVAILABLE;
#endif
	aux = file_listen(fileno(fp));
	if (aux != -3)
		return aux;
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
	x->stream.format = Cnil;
	x->stream.flags = 0;
	x->stream.byte_size = 8;
	x->stream.buffer = NULL;
	x->stream.encoder = NULL;
	x->stream.decoder = NULL;
	x->stream.last_char = EOF;
	x->stream.byte_stack = Cnil;
	x->stream.last_code[0] = x->stream.last_code[1] = EOF;
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

static void
maybe_clearerr(cl_object strm)
{
	cl_type t = type_of(strm);
	if (t == smm_io || t == smm_output || t == smm_input) {
		FILE *f = IO_STREAM_FILE(strm);
		if (f != NULL) clearerr(f);
	}
}

static int
restartable_io_error(cl_object strm)
{
	cl_env_ptr the_env = ecl_process_env();
	volatile int old_errno = errno;
	/* ecl_disable_interrupts(); ** done by caller */
	maybe_clearerr(strm);
	ecl_enable_interrupts_env(the_env);
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
	cl_env_ptr the_env = ecl_process_env();
	/* ecl_disable_interrupts(); ** done by caller */
	maybe_clearerr(strm);
	ecl_enable_interrupts_env(the_env);
	FElibc_error("Read or write operation to stream ~S signaled an error.",
		     1, strm);
}

static void
character_size_overflow(cl_object strm, int c)
{
	FEerror("Tried to write a character ~S in a ~A stream.", 2,
		CODE_CHAR(c), cl_stream_external_format(strm));
}

#ifdef ECL_UNICODE
static void
unsupported_character(cl_object stream)
{
	FEerror("In stream ~A, found a Unicode character code that "
		"exceeds the limit of 21 bits.",
		1, stream);
}

static void
invalid_codepoint(cl_object stream, cl_fixnum c)
{
	FEerror("When reading stream ~A with external format ~A,~%"
		"found an invalid character code, ~D.",
		3, stream, cl_stream_external_format(stream), MAKE_FIXNUM(c));
}

static void
malformed_character(cl_object stream)
{
	FEerror("Stream ~A with external format ~A contains an invalid octet sequence.",
		2, stream, cl_stream_external_format(stream));
}

static void
too_long_utf8_sequence(cl_object stream)
{
	CEerror(Cnil, "In stream ~S, found a too long UTF-8 sequence.", 1, stream);
}
#endif

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
	int flags = ECL_STREAM_DEFAULT_FORMAT;
	cl_object standard_input;
	cl_object standard_output;
	cl_object error_output;
	cl_object aux;
	cl_object null_stream;
	cl_object x;
#if defined(_MSVC)
	flags |= ECL_STREAM_CRLF;
#endif

	null_stream = ecl_make_stream_from_FILE(make_constant_base_string("/dev/null"),
						NULL, smm_io, 8, flags, Cnil);
	generic_close(null_stream);
	null_stream = cl_make_two_way_stream(null_stream, cl_make_broadcast_stream(0));
	cl_core.null_stream = null_stream;

#if 1
	standard_input = ecl_make_stream_from_FILE(make_constant_base_string("stdin"),
						   stdin, smm_input, 8, flags, Cnil);
	standard_output = ecl_make_stream_from_FILE(make_constant_base_string("stdout"),
						    stdout, smm_output, 8, flags, Cnil);
	error_output = ecl_make_stream_from_FILE(make_constant_base_string("stderr"),
						 stderr, smm_output, 8, flags, Cnil);
#else
	standard_input = ecl_make_file_stream_from_fd(make_constant_base_string("stdin"),
						      STDIN_FILENO, smm_input, 8, flags,
						      Cnil);
	standard_output = ecl_make_file_stream_from_fd(make_constant_base_string("stdout"),
						       STDOUT_FILENO, smm_output, 8, flags,
						       Cnil);
	error_output = ecl_make_file_stream_from_fd(make_constant_base_string("stderr"),
						    STDERR_FILENO, smm_output, 8, flags,
						    Cnil);
#endif
	cl_core.standard_input = standard_input;
	ECL_SET(@'*standard-input*', standard_input);
	cl_core.standard_output = standard_output;
	ECL_SET(@'*standard-output*', standard_output);
	ECL_SET(@'*trace-output*', standard_output);
	cl_core.error_output = error_output;
	ECL_SET(@'*error-output*', error_output);

	cl_core.terminal_io = aux 
		= cl_make_two_way_stream(standard_input, standard_output);

	ECL_SET(@'*terminal-io*', aux);
	aux = cl_make_synonym_stream(@'*terminal-io*');
	ECL_SET(@'*query-io*', aux);
	ECL_SET(@'*debug-io*', aux);
}

