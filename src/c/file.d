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

#define MAKE_BIT_MASK(n) ((1<<(n))-1)

static int flisten(FILE *fp);

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

/*----------------------------------------------------------------------
 *	ecl_input_stream_p(strm) answers
 *	if stream strm is an input stream or not.
 *	It does not check if it really is possible to read
 *	from the stream,
 *	but only checks the mode of the stream (sm_mode).
 *----------------------------------------------------------------------
 */
bool
ecl_input_stream_p(cl_object strm)
{
BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm))
		return !Null(funcall(2, @'gray::input-stream-p', strm));
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_io:
	case smm_input:
#if defined(ECL_WSOCK)
	case smm_input_wsock:
	case smm_io_wsock:
#endif
	case smm_concatenated:
	case smm_two_way:
	case smm_echo:
	case smm_string_input:
		return(TRUE);

	case smm_output:
#if defined(ECL_WSOCK)
	case smm_output_wsock:
#endif
	case smm_string_output:
	case smm_broadcast:
		return(FALSE);

	case smm_synonym:
		strm = ecl_symbol_value(strm->stream.object0);
		goto BEGIN;

	default:
		ecl_internal_error("illegal stream mode");
	}
}

/*----------------------------------------------------------------------
 *	ecl_output_stream_p(strm) answers
 *	if stream strm is an output stream.
 *	It does not check if it really is possible to write
 *	to the stream,
 *	but only checks the mode of the stream (sm_mode).
 *----------------------------------------------------------------------
 */
bool
ecl_output_stream_p(cl_object strm)
{
BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm))
		return !Null(funcall(2, @'gray::output-stream-p', strm));
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_input:
	case smm_probe:
#if defined(ECL_WSOCK)
	case smm_input_wsock:
#endif
	case smm_concatenated:
	case smm_string_input:
		return(FALSE);

	case smm_output:
#if defined(ECL_WSOCK)
	case smm_output_wsock:
	case smm_io_wsock:
#endif
	case smm_io:
	case smm_two_way:
	case smm_echo:
	case smm_broadcast:
	case smm_string_output:
		return(TRUE);

	case smm_synonym:
		strm = ecl_symbol_value(strm->stream.object0);
		goto BEGIN;

	default:
		ecl_internal_error("illegal stream mode");
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
	if (ECL_INSTANCEP(strm))
		return funcall(2, @'gray::stream-element-type', strm);
#endif
	if (type_of(strm) != t_stream)
		FEtype_error_stream(strm);
	if (strm->stream.closed)
		FEclosed_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_input:
	case smm_output:
#if defined(ECL_WSOCK)
	case smm_input_wsock:
	case smm_output_wsock:
	case smm_io_wsock:
#endif
	case smm_io:
		if (strm->stream.char_stream_p)
			output = @'base-char';
		else {
			cl_fixnum bs = strm->stream.byte_size;
			output = strm->stream.signed_bytes?
				@'signed-byte' : @'unsigned-byte';
			if (bs != 8)
				output = cl_list(2, output, MAKE_FIXNUM(bs));
		}
		break;
	case smm_synonym:
		strm = ecl_symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_broadcast:
		x = strm->stream.object0;
		if (ecl_endp(x)) {
			output = @'t';
			break;
		}
		strm = CAR(x);
		goto BEGIN;

	case smm_concatenated:
		x = strm->stream.object0;
		if (ecl_endp(x))
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
		ecl_internal_error("illegal stream mode");
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

static void not_an_input_stream(cl_object fn) /*__attribute__((noreturn))*/;
static void not_an_output_stream(cl_object fn) /*__attribute__((noreturn))*/;
static void wrong_file_handler(cl_object strm) /*__attribute__((noreturn))*/;

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
io_error(cl_object strm)
{
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
	FormatMessage( FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER,
		       0, WSAGetLastError(), 0, ( void* )&msg, 0, NULL );
	msg_obj = make_base_string_copy( msg );
	LocalFree( msg );
	FEerror( err_msg, 2, strm, msg_obj );
}
#endif

/*----------------------------------------------------------------------
 *	ecl_open_stream(fn, smm, if_exists, if_does_not_exist)
 *	opens file fn with mode smm.
 *	Fn is a pathname designator.
 *----------------------------------------------------------------------
 */
cl_object
ecl_open_stream(cl_object fn, enum ecl_smmode smm, cl_object if_exists,
		cl_object if_does_not_exist, cl_fixnum byte_size,
		bool char_stream_p, bool use_header_p)
{
	cl_object x;
	FILE *fp;
	cl_object filename = si_coerce_to_filename(fn);
	char *fname = filename->base_string.self;
	bool signed_bytes, appending = FALSE;
	uint8_t binary_header = 0, bit_buffer = 0, bits_left = 0;

	if (byte_size < 0) {
		signed_bytes = 1;
		byte_size = -byte_size;
	} else {
		signed_bytes = 0;
	}
	if (char_stream_p && byte_size != 8) {
		FEerror("Tried to make a character stream of byte size /= 8.",0);
	}
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
		} else if (!char_stream_p && use_header_p) {
			/* Read the binary header */
			int c = getc(fp);
			if (c != EOF) {
				binary_header = c & 0xFF;
				if (binary_header & ~7)
					FEerror("~S has an invalid binary header ~S",
					        2, fn, MAKE_FIXNUM(binary_header));
			}
			ecl_fseeko(fp, 0, SEEK_SET);
		}
	} else if (smm == smm_output || smm == smm_io) {
		if (if_exists == @':new_version' && if_does_not_exist == @':create')
			goto CREATE;
		fp = fopen(fname, OPEN_R);
		if (fp != NULL) {
			if (!char_stream_p && use_header_p && (if_exists == @':overwrite' || if_exists == @':append')) {
				/* Read binary header */
				int c = getc(fp);
				if (c != EOF) {
					binary_header = c & 0xFF;
					if (binary_header & ~7)
						FEerror("~S has an invalid binary header ~S",
						        2, fn, MAKE_FIXNUM(binary_header));
					if (binary_header != 0 && if_exists == @':append' &&
					    ecl_fseeko(fp, -1, SEEK_END) == 0) {
						/* Read the last byte */
						bit_buffer = getc(fp) & 0xFF;
						bits_left = binary_header;
					}
				}
			}
			fclose(fp);
			if (if_exists == @':error')
				FEcannot_open(fn);
			else if (if_exists == @':rename') {
				fp = ecl_backup_fopen(fname, (smm == smm_output)
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
			} else if (if_exists == @':overwrite' || if_exists == @':append') {
				/* We cannot use "w+b" because it truncates.
				   We cannot use "a+b" because writes jump to the end. */
				int f = open(filename->base_string.self, (smm == smm_output)?
					     (O_WRONLY|O_CREAT) : (O_RDWR|O_CREAT));
				if (f < 0)
					FEcannot_open(fn);
				fp = fdopen(f, (smm == smm_output)? OPEN_W : OPEN_RW);
				if (fp == NULL) {
					close(f);
					FEcannot_open(fn);
				}
				if (if_exists == @':append') {
					ecl_fseeko(fp, 0, SEEK_END);
					appending = TRUE;
				}
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
	x = cl_alloc_object(t_stream);
	x->stream.mode = (short)smm;
	x->stream.closed = 0;
	x->stream.file = (void*)fp;
	x->stream.char_stream_p = char_stream_p;
	/* Michael, touch this to reactivate support for odd bit sizes! */
	if (!use_header_p) {
		/* binary header not used, round byte_size to a 8 bits */
		byte_size = (byte_size + 7) & ~7;
		/* change header to something detectable */
		binary_header = 0xFF;
	}
	x->stream.byte_size = byte_size;
	x->stream.signed_bytes = signed_bytes;
	x->stream.header = binary_header;
	x->stream.last_op = 0;
	if (bits_left != 0) {
		x->stream.bits_left = bits_left;
		x->stream.bit_buffer = bit_buffer;
		x->stream.buffer_state = -1;
	}
	x->stream.object1 = fn;
	x->stream.int0 = x->stream.int1 = 0;
	si_set_buffering_mode(x, char_stream_p? @':line-buffered' : @':fully-buffered');
	if (smm == smm_probe) {
		cl_close(1, x);
	} else {
		si_set_finalizer(x, Ct);
		if (!char_stream_p) {
			/* Set file pointer to the correct position */
			if (appending) {
				if (bits_left != 0)
					ecl_fseeko(fp, -1, SEEK_END);
			} else {
				ecl_fseeko(fp, (use_header_p ? 1 : 0), SEEK_SET);
			}
		}
	}
	return(x);
}

/* Forward definitions */
static void ecl_write_byte8(int c, cl_object strm);
static int ecl_read_byte8(cl_object strm);
static void flush_output_stream_binary(cl_object strm);

@(defun close (strm &key (abort @'nil'))
	FILE *fp;
@
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm)) {
		return funcall(2, @'gray::close', strm);
	}
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	/* It is permissible to close a closed file */
	if (strm->stream.closed)
		@(return Ct);
	fp = (FILE*)strm->stream.file;
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_output:
		if (fp == stdout)
			FEerror("Cannot close the standard output.", 0);
		goto DO_CLOSE;
	case smm_input:
		if (fp == stdin)
			FEerror("Cannot close the standard input.", 0);
	case smm_io:
	case smm_probe:
	DO_CLOSE:
		if (fp == NULL)
			wrong_file_handler(strm);
		if (ecl_output_stream_p(strm)) {
			ecl_force_output(strm);
			if (!strm->stream.char_stream_p && strm->stream.header != 0xFF) {
				/* write header */
				if (ecl_fseeko(fp, 0, SEEK_SET) != 0)
					io_error(strm);
				ecl_write_byte8(strm->stream.header, strm);
			}
		}
		if (fclose(fp) != 0)
			FElibc_error("Cannot close stream ~S.", 1, strm);
#if !defined(GBC_BOEHM)
		cl_dealloc(strm->stream.buffer);
		strm->stream.file = NULL;
#endif
		break;
#if defined(ECL_WSOCK)
	case smm_input_wsock:
	case smm_output_wsock:
	case smm_io_wsock:
		if ( closesocket( ( int )strm->stream.file ) != 0 )
			wsock_error( "Cannot close Windows Socket ~S~%~A.", strm );
#if !defined(GBC_BOEHM)
		cl_dealloc(strm->stream.buffer);
		strm->stream.file = NULL;
#endif
		break;
#endif

	case smm_two_way:
		strm->stream.object0 = OBJNULL;
	case smm_synonym:
	case smm_broadcast:
	case smm_concatenated:
	case smm_echo:
	case smm_string_input:
	case smm_string_output:
	  /* The elements of a composite stream are not closed. For
	     composite streams we zero object1. For files we do not,
	     as it might contain an useful pathname */
		strm->stream.object1 = OBJNULL;
		break;

	default:
		ecl_internal_error("illegal stream mode");
	}
	strm->stream.closed = 1;
	strm->stream.file = NULL;
	@(return Ct);
@)

cl_object
ecl_make_string_input_stream(cl_object strng, cl_index istart, cl_index iend)
{
	cl_object strm;

	strm = cl_alloc_object(t_stream);
	strm->stream.mode = (short)smm_string_input;
	strm->stream.closed = 0;
	strm->stream.file = NULL;
	strm->stream.object0 = strng;
	strm->stream.object1 = OBJNULL;
	strm->stream.int0 = istart;
	strm->stream.int1 = iend;
	strm->stream.char_stream_p = 1;
	strm->stream.byte_size = 8;
	strm->stream.signed_bytes = 0;
	return(strm);
}

cl_object
ecl_make_string_output_stream(cl_index line_length)
{
	cl_object s = cl_alloc_adjustable_base_string(line_length);
	return si_make_string_output_stream_from_string(s);
}


/**********************************************************************
 * BYTE INPUT/OUTPUT
 *
 * CLOS streams should handle byte input/output separately. For the
 * rest of streams, we decompose each byte into octets and write them
 * from the least significant to the most significant one.
 */

static void
ecl_write_byte8(int c, cl_object strm)
{
	/*
	 * INV: We only get streams of the following four modes.
	 */
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_output:
	case smm_io: {
		FILE *fp = (FILE *)strm->stream.file;
		if (fp == NULL)
			wrong_file_handler(strm);
		if (putc(c, fp) == EOF)
			io_error(strm);
		break;
	}
#if defined(ECL_WSOCK)
	case smm_io_wsock:
	case smm_output_wsock: {
		int fp = (int)strm->stream.file;
		if ( fp == INVALID_SOCKET )
			wrong_file_handler( strm );
		else
		{
			char ch = ( char )c;
			if ( send( fp, &ch, 1, 0 ) == SOCKET_ERROR )
				wsock_error( "Cannot write char to Windows Socket ~S.~%~A", strm );
		}
		break;
	}
#endif
	case smm_string_output:
		strm->stream.int0++;
		ecl_string_push_extend(strm->stream.object0, c);
		break;
	default:
		ecl_internal_error("illegal stream mode");
	}
}

void
ecl_write_byte(cl_object c, cl_object strm)
{
	cl_index bs, nb;
	cl_object aux;
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
	if (type_of(strm) != t_stream)
		FEtype_error_stream(strm);
	if (strm->stream.closed)
		FEclosed_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_io:
		io_stream_begin_write(strm);
	case smm_output:
#if defined(ECL_WSOCK)
	case smm_io_wsock:
	case smm_output_wsock:
#endif
	case smm_string_output:
		break;
	case smm_synonym:
		strm = ecl_symbol_value(strm->stream.object0);
		goto BEGIN;
	case smm_broadcast: {
		cl_object x;
		for (x = strm->stream.object0; !ecl_endp(x); x = CDR(x))
			ecl_write_byte(c, CAR(x));
		return;
	}
	case smm_two_way:
		strm->stream.int0++;
		strm = strm->stream.object1;
		goto BEGIN;
	case smm_echo:
		strm = strm->stream.object1;
		goto BEGIN;
	case smm_input:
#if defined(ECL_WSOCK)
	case smm_input_wsock:
#endif
	case smm_concatenated:
	case smm_string_input:
		not_an_output_stream(strm);
	default:
		ecl_internal_error("illegal stream mode");
	}
	/*
	 * Here is the real output of the byte.
	 */
	bs = strm->stream.byte_size;
	if (bs == 8) {
		cl_fixnum n = fixint(c);
		ecl_write_byte8(n & 0xFF, strm);
	} else if (bs & 7) {
		unsigned char b = strm->stream.bit_buffer;
		int bs_ = bs;
		cl_object c0 = c;
		nb = strm->stream.bits_left;
		if (strm->stream.buffer_state == 1) {
			/* buffer is prepared for reading: re-read (8-nb) bits and throw the rest */
			int c0;
			ecl_fseeko((FILE*)strm->stream.file, -1, SEEK_CUR);
			c0 = ecl_read_byte8(strm);
			if (c0 == EOF)
				/* this should not happen !!! */
				io_error(strm);
			ecl_fseeko((FILE*)strm->stream.file, -1, SEEK_CUR);
			b = (unsigned char)(c0 & MAKE_BIT_MASK(8-nb));
			nb = (8-nb);
		}
		do {
			b |= (unsigned char)(fixnnint(cl_logand(2, c0, MAKE_FIXNUM(MAKE_BIT_MASK(8-nb)))) << nb);
			bs_ -= (8-nb);
			c0 = cl_ash(c0, MAKE_FIXNUM(nb-8));
			if (bs_ >= 0) {
				ecl_write_byte8(b, strm);
				b = nb = 0;
			}
		} while (bs_ > 0);
		strm->stream.bits_left = (bs_ < 0 ? (8+bs_) : 0);
		strm->stream.bit_buffer = (bs_ < 0 ? (b & MAKE_BIT_MASK(8+bs_)) : 0);
		strm->stream.buffer_state = (bs_ < 0 ? -1 : 0);
	} else do {
		cl_object b = cl_logand(2, c, MAKE_FIXNUM(0xFF));
		ecl_write_byte8(fix(b), strm);
		c = cl_ash(c, MAKE_FIXNUM(-8));
		bs -= 8;
	} while (bs);
}

static int
ecl_read_byte8(cl_object strm)
{
	/*
	 * INV: We only get streams of the following four modes.
	 */
	int c;
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_input:
	case smm_io: {
		FILE *fp = (FILE*)strm->stream.file;
		if (fp == NULL)
			wrong_file_handler(strm);
		c = getc(fp);
		if (c == EOF && ferror(fp))
			io_error(strm);
		break;
	}
#if defined(ECL_WSOCK)
	case smm_io_wsock:
	case smm_input_wsock: {
		int fp = (int)strm->stream.file;
		if ( fp == INVALID_SOCKET )
			wrong_file_handler( strm );
		else
		{
			/* check for unread chars first */
			if (CONSP(strm->stream.object0)) {
				c = (unsigned char)CHAR_CODE(CAR(strm->stream.object0));
				strm->stream.object0 = CDR(strm->stream.object0);
			} else {
				char ch;
				if ( recv( fp, &ch, 1, 0 ) == SOCKET_ERROR )
					wsock_error( "Cannot read char from Windows socket ~S.~%~A", strm );
				c = ( unsigned char )ch;
			}
		}
		break;
	}
#endif
	case smm_string_input:
		if (strm->stream.int0 >= strm->stream.int1)
			c = EOF;
		else
			c = strm->stream.object0->base_string.self[strm->stream.int0++];
		break;
	default:
		ecl_internal_error("illegal stream mode");
	}
	return c;
}

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
			char *new_buffer;
			cl_index buffer_size = BUFSIZ;
			new_buffer = stream->stream.buffer = cl_alloc_atomic(buffer_size);
			setvbuf(fp, new_buffer, buffer_mode, buffer_size);
		}
	}
	@(return stream)
}

static void
flush_output_stream_binary(cl_object strm)
{
	if (strm->stream.buffer_state == -1) {
		/* buffer is prepared for writing: flush it */
		unsigned char b = strm->stream.bit_buffer;
		cl_index nb = strm->stream.bits_left;
		bool do_merging = FALSE;
		FILE *fp = (FILE*)strm->stream.file;

		/* do we need to merge with existing byte? */
		ecl_off_t current_offset = ecl_ftello(fp), diff_offset;
		if (ecl_fseeko(fp, 0, SEEK_END) != 0)
			io_error(strm);
		switch ((diff_offset = ecl_ftello(fp)-current_offset)) {
			case 0: break;
			case 1:
				/* (EOF-1): merge only if less bits left than header tells us */
				do_merging = (nb < strm->stream.header);
				break;
			default:
				do_merging = (diff_offset > 1);
				break;
		}
		if (ecl_fseeko(fp, current_offset, SEEK_SET) != 0)
			io_error(strm);

		/* do merging, if required */
		if (do_merging){
			if (strm->stream.mode == smm_io) {
				/* I/O stream: no need to reopen and I/O sync already triggered */
				int c = ecl_read_byte8(strm);
				if (c != EOF)
					b |= (unsigned char)(c & ~MAKE_BIT_MASK(nb));
				/* rewind stream */
				if (ecl_fseeko(fp, -1, SEEK_CUR) != 0)
					io_error(strm);
			} else {
				/* write-only stream: need to reopen the file for reading *
				 * the byte to merge, then reopen it back for writing     */
				cl_object fn = si_coerce_to_filename(strm->stream.object1);
				if (freopen(fn->base_string.self, OPEN_R, fp) == NULL ||
				    ecl_fseeko(fp, current_offset, SEEK_SET) != 0)
					io_error(strm);
				/* cannot use ecl_read_byte8 here, because strm hasn't the right mode */
				b |= (unsigned char)(getc(fp) & ~MAKE_BIT_MASK(nb));
				/* need special trick to re-open the file for writing, avoiding truncation */
				fclose(fp);
				strm->stream.file = fdopen(open(fn->base_string.self, O_WRONLY), OPEN_W);
				if (strm->stream.file == NULL || ecl_fseeko(fp, current_offset, SEEK_SET) != 0)
					io_error(strm);
			}
		} else {
			/* No merging occurs -> header must be overwritten */
			strm->stream.header = nb;
		}

		/* flush byte w/o changing file pointer */
		ecl_write_byte8(b, strm);
		ecl_fseeko(fp, -1, SEEK_CUR);
	}
}

cl_object
ecl_read_byte(cl_object strm)
{
	cl_object c;
	cl_index bs, nb;
	/*
	 * In this first part, we identify the composite streams and
	 * also CLOS streams.
	 */
BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm)) {
		return funcall(2, @'gray::stream-read-byte', strm);
	}
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	if (strm->stream.closed)
		FEclosed_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_io:
		io_stream_begin_read(strm);
	case smm_input:
	case smm_string_input:
#if defined(ECL_WSOCK)
	case smm_io_wsock:
	case smm_input_wsock:
#endif
		break;
	case smm_synonym:
		strm = ecl_symbol_value(strm->stream.object0);
		goto BEGIN;
	case smm_concatenated: {
		cl_object strmi = strm->stream.object0;
		c = Cnil;
		while (!ecl_endp(strmi)) {
			c = ecl_read_byte(CAR(strmi));
			if (c != Cnil)
				break;
			strm->stream.object0 = strmi = CDR(strmi);
		}
		return c;
	}
	case smm_two_way:
		if (strm == cl_core.terminal_io)
			ecl_force_output(cl_core.terminal_io->stream.object1);
		strm->stream.int1 = 0;
		strm = strm->stream.object0;
		goto BEGIN;
	case smm_echo:
		c = ecl_read_byte(strm->stream.object0);
		if (c != Cnil) {
			if (strm->stream.int0 == 0)
				ecl_write_byte(c, strm->stream.object1);
			else		/* don't echo twice if it was unread */
				--(strm->stream.int0);
		}
		return c;
	case smm_output:
#if defined(ECL_WSOCK)
	case smm_output_wsock:
#endif
	case smm_broadcast:
	case smm_string_output:
		not_an_input_stream(strm);
	default:
		ecl_internal_error("illegal stream mode");
	}
	/*
	 * Here we treat the case of streams for which ecl_read_byte8 works.
	 */
	bs = strm->stream.byte_size;
	if (bs == 8) {
		cl_fixnum i = ecl_read_byte8(strm);
		if (i == EOF)
			return Cnil;
		if (strm->stream.signed_bytes) {
			unsigned char c = i;
			return MAKE_FIXNUM((signed char)c);
		}
		return MAKE_FIXNUM(i);
	} else if (bs & 7) {
		unsigned char b = strm->stream.bit_buffer;
		nb = strm->stream.bits_left;
		if (strm->stream.buffer_state == -1) {
			/* buffer is prepared for writing: flush it */
			flush_output_stream_binary(strm);
			b = ((unsigned char)ecl_read_byte8(strm)) >> nb;
			nb = (8-nb);
		}
		if (nb >= bs) {
			c = MAKE_FIXNUM(b & (unsigned char)MAKE_BIT_MASK(bs));
			strm->stream.bits_left = (nb-bs);
			strm->stream.bit_buffer = (strm->stream.bits_left > 0 ? (b >> bs): 0);
		} else {
			cl_index i;
			c = MAKE_FIXNUM(b);
			while (nb < bs) {
				int c0 = ecl_read_byte8(strm);
				if (c0 == EOF)
					return Cnil;
				b = (unsigned char)(c0 & 0xFF);
				for (i=8; i>0 && nb<bs; i--, nb++, b>>=1) {
					c = cl_logior(2, c, cl_ash(MAKE_FIXNUM(b&0x01), MAKE_FIXNUM(nb)));
				}
			}
			strm->stream.bits_left = i;
			strm->stream.bit_buffer = b;
		}
		strm->stream.buffer_state = (strm->stream.bits_left > 0 ? 1 : 0);
	} else {
		cl_index bs_ = bs;
		c = MAKE_FIXNUM(0);
		for (nb = 0; bs_ >= 8; bs_ -= 8, nb += 8) {
			cl_fixnum i = ecl_read_byte8(strm);
			if (i == EOF)
				return Cnil;
			c = cl_logior(2, c, cl_ash(MAKE_FIXNUM(i), MAKE_FIXNUM(nb)));
		}
	}
	if (strm->stream.signed_bytes && cl_logbitp(MAKE_FIXNUM(bs-1), c) != Cnil) {
		c = cl_logandc1(cl_ash(MAKE_FIXNUM(1), MAKE_FIXNUM(bs-1)), c);
		c = ecl_minus(c, cl_ash(MAKE_FIXNUM(1), MAKE_FIXNUM(bs-1)));
	}
	return c;
}


/**********************************************************************
 * CHARACTER INPUT/OUTPUT
 */

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
ecl_read_char(cl_object strm)
{
	int c;
	FILE *fp;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm)) {
		cl_object c = funcall(2, @'gray::stream-read-char', strm);
		return CHARACTERP(c)? CHAR_CODE(c) : EOF;
	}
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	if (strm->stream.closed)
		FEclosed_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_io:
		io_stream_begin_read(strm);
	case smm_input: {
		FILE *fp = (FILE*)strm->stream.file;
		if (!strm->stream.char_stream_p)
			not_a_character_stream(strm);
		if (fp == NULL)
			wrong_file_handler(strm);
		c = getc(fp);
		if (c == EOF && ferror(fp))
			io_error(strm);
		break;
	}
#if defined(ECL_WSOCK)
	case smm_io_wsock:
	case smm_input_wsock: {
		int fp = (int)strm->stream.file;
		if (!strm->stream.char_stream_p)
			not_a_character_stream(strm);
		if ( fp == INVALID_SOCKET )
			wrong_file_handler( strm );
		else {
			if (CONSP(strm->stream.object0)) {
				c = (unsigned char)CHAR_CODE(CAR(strm->stream.object0));
				strm->stream.object0 = CDR(strm->stream.object0);
			} else {
				char ch;
				if ( recv( fp, &ch, 1, 0 ) == SOCKET_ERROR )
					wsock_error( "Cannot read char from Windows socket ~S.~%~A", strm );
				c = ( unsigned char )ch;
			}
		}
		break;
	}
#endif

	case smm_synonym:
		strm = ecl_symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_concatenated: {
		cl_object strmi = strm->stream.object0;
		c = EOF;
		while (!ecl_endp(strmi)) {
			c = ecl_read_char(CAR(strmi));
			if (c != EOF)
				break;
			strm->stream.object0 = strmi = CDR(strmi);
		}
		break;
	}
	case smm_two_way:
		if (strm == cl_core.terminal_io)
			ecl_force_output(cl_core.terminal_io->stream.object1);
		strm->stream.int1 = 0;
		strm = strm->stream.object0;
		goto BEGIN;

	case smm_echo:
		c = ecl_read_char(strm->stream.object0);
		if (c != EOF) {
			if (strm->stream.int0 == 0)
				ecl_write_char(c, strm->stream.object1);
			else		/* don't echo twice if it was unread */
				--(strm->stream.int0);
		}
		break;

	case smm_string_input:
		if (strm->stream.int0 >= strm->stream.int1)
			c = EOF;
		else
			c = strm->stream.object0->base_string.self[strm->stream.int0++];
		break;

	case smm_output:
#if defined(ECL_WSOCK)
	case smm_output_wsock:
#endif
	case smm_broadcast:
	case smm_string_output:
		not_an_input_stream(strm);

	default:
		ecl_internal_error("illegal stream mode");
	}
	return c;
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
	int c;
	FILE *fp;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm)) {
		cl_object c = funcall(2, @'gray::stream-peek-char', strm);
		return CHARACTERP(c)? CHAR_CODE(c) : EOF;
	}
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	if (strm->stream.closed)
		FEclosed_stream(strm);
	fp = (FILE*)strm->stream.file;
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_io:
		io_stream_begin_read(strm);
	case smm_input:
		if (!strm->stream.char_stream_p)
			not_a_character_stream(strm);
		if (fp == NULL)
			wrong_file_handler(strm);
		c = getc(fp);
		if (c == EOF && ferror(fp))
			io_error(strm);
		ungetc(c, fp);
		break;

#if defined(ECL_WSOCK)
	case smm_io_wsock:
	case smm_input_wsock: {
		int fp = strm->stream.file;
		if (!strm->stream.char_stream_p)
			not_a_character_stream(strm);
		if ( fp == INVALID_SOCKET )
			wrong_file_handler( strm );
		else {
			if (CONSP(strm->stream.object0)) {
				c = (unsigned char)CHAR_CODE(CAR(strm->stream.object0));
			} else {
				char ch;
				if ( recv( fp, &ch, 1, MSG_PEEK ) == SOCKET_ERROR )
					wsock_error( "Cannot peek char from Windows socket ~S.~%~A", strm );
				c = ( unsigned char )ch;
			}
		}
		break;
	}
#endif

	case smm_synonym:
		strm = ecl_symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_concatenated: {
		cl_object strmi = strm->stream.object0;
		c = EOF;
		while (!ecl_endp(strmi)) {
			c = ecl_peek_char(CAR(strmi));
			if (c != EOF)
				break;
			strm->stream.object0 = strmi = CDR(strmi);
		}
		break;
	}
	case smm_two_way:
		if (strm == cl_core.terminal_io)
			ecl_force_output(cl_core.terminal_io->stream.object1);
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
			c = strm->stream.object0->base_string.self[strm->stream.int0];
		break;

	case smm_output:
#if defined(ECL_WSOCK)
	case smm_output_wsock:
#endif
	case smm_broadcast:
	case smm_string_output:
		not_an_input_stream(strm);

	default:
		ecl_internal_error("illegal stream mode");
	}
	return c;
}

int
ecl_read_char_noeof(cl_object strm)
{
	int c = ecl_read_char(strm);
	if (c == EOF)
		FEend_of_file(strm);
	return c;
}

void
ecl_unread_char(int c, cl_object strm)
{
	FILE *fp;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm)) {
		funcall(3, @'gray::stream-unread-char', strm, CODE_CHAR(c));
		return;
	}
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	if (strm->stream.closed)
		FEclosed_stream(strm);
	fp = (FILE*)strm->stream.file;
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_io:
		if (strm->stream.last_op < 0) {
			goto UNREAD_ERROR;
		}
		strm->stream.last_op = +1;
	case smm_input:
		if (!strm->stream.char_stream_p)
			not_a_character_stream(strm);
		if (fp == NULL)
			wrong_file_handler(strm);
		ungetc(c, fp);
		if (c == EOF)
			io_error(strm);
/*		--strm->stream.int0; useless in smm_io, Beppe */
		break;

#if defined(ECL_WSOCK)
	case smm_io_wsock:
	case smm_input_wsock:
		strm->stream.object0 = CONS(CODE_CHAR((unsigned char)c), strm->stream.object0);
		break;
#endif

	case smm_synonym:
		strm = ecl_symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_concatenated:
		if (ecl_endp(strm->stream.object0))
			goto UNREAD_ERROR;
		strm = CAR(strm->stream.object0);
		goto BEGIN;

	case smm_two_way:
		strm = strm->stream.object0;
		goto BEGIN;

	case smm_echo:
		ecl_unread_char(c, strm->stream.object0);
		(strm->stream.int0)++;
		break;

	case smm_string_input:
		if (strm->stream.int0 <= 0 || (int)strm->stream.object0->base_string.self[strm->stream.int0-1] != c)
			goto UNREAD_ERROR;
		--strm->stream.int0;
		break;

	case smm_output:
#if defined(ECL_WSOCK)
	case smm_output_wsock:
#endif
	case smm_broadcast:
	case smm_string_output:
		not_an_input_stream(strm);

	default:
		ecl_internal_error("illegal stream mode");
	}
	return;

UNREAD_ERROR:
	FEerror("Cannot unread the stream ~S.", 1, strm);
}

int
ecl_write_char(int c, cl_object strm)
{
	cl_object x;
	FILE *fp;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm)) {
		funcall(3, @'gray::stream-write-char', strm, CODE_CHAR(c));
		return c;
	}
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	if (strm->stream.closed)
		FEclosed_stream(strm);
	fp = (FILE*)strm->stream.file;
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_io:
		io_stream_begin_write(strm);
	case smm_output:
		if (!strm->stream.char_stream_p)
			not_a_character_stream(strm);
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

#if defined(ECL_WSOCK)
	case smm_io_wsock:
	case smm_output_wsock:
		if (!strm->stream.char_stream_p)
			not_a_character_stream(strm);
		if (c == '\n')
			strm->stream.int1 = 0;
		else if (c == '\t')
			strm->stream.int1 = (strm->stream.int1&~07) + 8;
		else
			strm->stream.int1++;
		if ( ( int )fp == INVALID_SOCKET )
			wrong_file_handler( strm );
		else
		{
			char ch = ( char )c;
			if ( send( ( int )fp, &ch, 1, 0 ) == SOCKET_ERROR )
				wsock_error( "Cannot write char to Windows Socket ~S.~%~A", strm );
		}
		break;
#endif

	case smm_synonym:
		strm = ecl_symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_broadcast:
		for (x = strm->stream.object0; !ecl_endp(x); x = CDR(x))
			ecl_write_char(c, CAR(x));
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
#if defined(ECL_WSOCK)
	case smm_input_wsock:
#endif
	case smm_concatenated:
	case smm_string_input:
		not_an_output_stream(strm);

	default:
		ecl_internal_error("illegal stream mode");
	}
	return(c);
}

void
writestr_stream(const char *s, cl_object strm)
{
	while (*s != '\0')
		ecl_write_char(*s++, strm);
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
				cl_write_byte(ischar? cl_char_code(elt) : elt,
					      stream);
				start++;
			} else {
				goto OUTPUT;
			}
		} end_loop_for_in;
		goto OUTPUT;
	}
	if (t != t_base_string &&
	    !(t == t_vector &&
	      (seq->vector.elttype == aet_b8 || seq->vector.elttype == aet_i8)))
	{
		bool ischar = cl_stream_element_type(stream) == @'base-char';
		while (start < end) {
			cl_object elt = ecl_aref(seq, start++);
			if (ischar) {
				ecl_write_char(ecl_char_code(elt), stream);
			} else {
				ecl_write_byte(elt, stream);
			}
		}
		goto OUTPUT;
	}
 AGAIN:
	if ((t = type_of(stream)) == t_stream &&
	    (stream->stream.mode == smm_io ||
	     stream->stream.mode == smm_output))
	{
		size_t towrite = end - start;
		if (stream->stream.mode == smm_io) {
			io_stream_begin_write(stream);
		}
		if (fwrite(seq->vector.self.ch + start, sizeof(char),
			   towrite, (FILE*)stream->stream.file) < towrite) {
			io_error(stream);
		}
	} else if (t == t_stream && stream->stream.mode == smm_two_way) {
		stream = stream->stream.object1;
		goto AGAIN;
	} else {
		char *p;
		for (p= seq->vector.self.ch; start < end; start++) {
			ecl_write_char(p[start], stream);
		}
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
		goto OUTPUT;
	}
	if (t != t_base_string &&
	    !(t == t_vector &&
	      (seq->vector.elttype == aet_b8 || seq->vector.elttype == aet_i8)))
	{
		bool ischar = cl_stream_element_type(stream) == @'base-char';
		while (start < end) {
			cl_object c;
			if (ischar) {
				int i = ecl_read_char(stream);
				if (i < 0) goto OUTPUT;
				c = CODE_CHAR(i);
			} else {
				c = ecl_read_byte(stream);
				if (c == Cnil) goto OUTPUT;
			}
			ecl_aset(seq, start++, c);
		}
		goto OUTPUT;
	}
 AGAIN:
	if ((t = type_of(stream)) == t_stream &&
	    (stream->stream.mode == smm_io ||
	     stream->stream.mode == smm_input))
	{
		size_t toread, n;
		if (stream->stream.mode == smm_io) {
			io_stream_begin_write(stream);
		}
		toread = end - start;
		n = fread(seq->vector.self.ch + start, sizeof(char),
			  toread, stream->stream.file);
		if (n < toread && ferror((FILE*)stream->stream.file))
			io_error(stream);
		start += n;
	} else if (t == t_stream && stream->stream.mode == smm_two_way) {
		stream = stream->stream.object0;
		goto AGAIN;
	} else {
		char *p;
		for (p = seq->vector.self.ch; start < end; start++) {
			int c = ecl_read_char(stream);
			if (c == EOF)
				break;
			p[start] = c;
		}
	}
 OUTPUT:
	@(return MAKE_FIXNUM(start))
}

void
ecl_force_output(cl_object strm)
{
	cl_object x;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm)) {
		funcall(2, @'gray::stream-force-output', strm);
		return;
	}
#endif
	if (type_of(strm) != t_stream)
		FEtype_error_stream(strm);
	if (strm->stream.closed)
		FEclosed_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_io:
		strm->stream.last_op = 0;
	case smm_output: {
		FILE *fp = (FILE*)strm->stream.file;
		if (fp == NULL)
			wrong_file_handler(strm);
		if ((strm->stream.byte_size & 7) && strm->stream.buffer_state == -1) {
			flush_output_stream_binary(strm);
		}
		if (fflush(fp) == EOF)
			io_error(strm);
		break;
	}
#if defined(ECL_WSOCK)
	case smm_io_wsock:
	case smm_output_wsock:
		/* do not do anything (yet) */
		break;
#endif

	case smm_synonym:
		strm = ecl_symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_broadcast:
		for (x = strm->stream.object0; !ecl_endp(x); x = CDR(x))
			ecl_force_output(CAR(x));
		break;

	case smm_two_way:
	case smm_echo:
		strm = strm->stream.object1;
		goto BEGIN;

	case smm_string_output:
		break;
	case smm_input:
#if defined(ECL_WSOCK)
	case smm_input_wsock:
#endif
	case smm_concatenated:
	case smm_string_input:
		FEerror("Cannot flush the stream ~S.", 1, strm);

	default:
		ecl_internal_error("illegal stream mode");
	}
}

void
ecl_clear_input(cl_object strm)
{
	cl_object x;
	FILE *fp;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm)) {
		funcall(2, @'gray::stream-clear-input', strm);
		return;
	}
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	if (strm->stream.closed)
		FEclosed_stream(strm);
	fp = (FILE*)strm->stream.file;
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_input:
		if (fp == NULL)
			wrong_file_handler(strm);
#if defined(mingw32) || defined(_MSC_VER)
		if (isatty(fileno(fp))) {
			/* Flushes Win32 console */
			if (!FlushConsoleInputBuffer((HANDLE)_get_osfhandle(fileno(fp))))
				FEwin32_error("FlushConsoleInputBuffer() failed", 0);
			/* Do not stop here: the FILE structure needs also to be flushed */
		}
#endif
		while (flisten(fp) == ECL_LISTEN_AVAILABLE) {
			getc(fp);
		}
		break;

#if defined(ECL_WSOCK)
	case smm_io_wsock:
	case smm_input_wsock:
		/* flush at least the unread chars */
		strm->stream.object0 = Cnil;
		/* do not do anything (yet) */
		printf( "Trying to clear input on windows socket stream!\n" );
		break;
#endif

	case smm_synonym:
		strm = ecl_symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_broadcast:
		for (x = strm->stream.object0; !ecl_endp(x); x = CDR(x))
			ecl_force_output(CAR(x));
		break;

	case smm_two_way:
	case smm_echo:
		strm = strm->stream.object0;
		goto BEGIN;

	case smm_string_output:
	case smm_io:
	case smm_output:
#if defined(ECL_WSOCK)
	case smm_output_wsock:
#endif
	case smm_concatenated:
	case smm_string_input:
		break;

	default:
		ecl_internal_error("illegal stream mode");
	}
}

void
ecl_clear_output(cl_object strm)
{
	cl_object x;
	FILE *fp;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm)) {
		funcall(2, @'gray::stream-clear-output',strm);
		return;
	}
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	if (strm->stream.closed)
		FEclosed_stream(strm);
	fp = (FILE*)strm->stream.file;
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_output:
#if 0
		if (fp == NULL)
			wrong_file_handler(strm);
		if (ecl_fseeko(fp, 0L, 2) != 0)
			io_error(strm);
#endif
		break;

#if defined(ECL_WSOCK)
	case smm_io_wsock:
	case smm_output_wsock:
		/* do not do anything (yet) */
		printf( "Trying to clear output windows socket stream\n!" );
		break;
#endif

	case smm_synonym:
		strm = ecl_symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_broadcast:
		for (x = strm->stream.object0; !ecl_endp(x); x = CDR(x))
			ecl_force_output(CAR(x));
		break;

	case smm_two_way:
	case smm_echo:
		strm = strm->stream.object1;
		goto BEGIN;

	case smm_string_output:
	case smm_io:
	case smm_input:
#if defined(ECL_WSOCK)
	case smm_input_wsock:
#endif
	case smm_concatenated:
	case smm_string_input:
		break;

	default:
		ecl_internal_error("illegal stream mode");
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

int
ecl_listen_stream(cl_object strm)
{
	FILE *fp;

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm)) {
		cl_object flag = funcall(2, @'gray::stream-listen', strm);
		return !(flag == Cnil);
	}
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	if (strm->stream.closed)
		FEclosed_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_io:
		io_stream_begin_read(strm);
	case smm_input:
		fp = (FILE*)strm->stream.file;
		if (fp == NULL)
			wrong_file_handler(strm);
		return flisten(fp);

#if defined(ECL_WSOCK)
	case smm_io_wsock:
	case smm_input_wsock:
		fp = (FILE*)strm->stream.file;
		if ( ( int )fp == INVALID_SOCKET )
			wrong_file_handler( strm );
		else
		{
			if (CONSP(strm->stream.object0))
				return ECL_LISTEN_AVAILABLE;
			else {
				struct timeval tv = { 0, 0 };
				fd_set fds;
				int result;

				FD_ZERO( &fds );
				FD_SET( ( int )fp, &fds );
				result = select( 0, &fds, NULL, NULL,  &tv );
				if ( result == SOCKET_ERROR )
					wsock_error( "Cannot listen on Windows socket ~S.~%~A", strm );
				return ( result > 0 ? ECL_LISTEN_AVAILABLE : ECL_LISTEN_NO_CHAR );
			}
		}
#endif

	case smm_synonym:
		strm = ecl_symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_concatenated: {
		cl_object l = strm->stream.object0;
		while (!ecl_endp(l)) {
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
#if defined(ECL_WSOCK)
	case smm_output_wsock:
#endif
	case smm_broadcast:
	case smm_string_output:
		not_an_input_stream(strm);

	default:
		ecl_internal_error("illegal stream mode");
	}
}

cl_object
ecl_file_position(cl_object strm)
{
	cl_object output;
BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm))
		FEerror("file-position not implemented for CLOS streams", 0);
#endif
	if (type_of(strm) != t_stream)
		FEtype_error_stream(strm);
	if (strm->stream.closed)
		FEclosed_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_io:
		strm->stream.last_op = 0;
	case smm_output:
		ecl_force_output(strm);
	case smm_input: {
		/* FIXME! This does not handle large file sizes */
		ecl_off_t offset;
		FILE *fp = (FILE*)strm->stream.file;
		if (fp == NULL)
			wrong_file_handler(strm);
		offset = ecl_ftello(fp);
		if (offset < 0)
			io_error(strm);
		if (sizeof(ecl_off_t) == sizeof(long)) {
			output = ecl_make_integer(offset);
		} else {
			output = ecl_off_t_to_integer(offset);
		}
		break;
	}
	case smm_string_output:
		/* INV: The size of a string never exceeds a fixnum. */
		output = MAKE_FIXNUM(strm->stream.object0->base_string.fillp);
		break;
	case smm_string_input:
		/* INV: The size of a string never exceeds a fixnum. */
		output = MAKE_FIXNUM(strm->stream.int0);
		break;

	case smm_synonym:
		strm = ecl_symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_broadcast:
		strm = strm->stream.object0;
		if (ecl_endp(strm))
			return MAKE_FIXNUM(0);
		strm = CAR(strm);
		goto BEGIN;

#if defined(ECL_WSOCK)
	case smm_input_wsock:
	case smm_output_wsock:
	case smm_io_wsock:
#endif
	case smm_concatenated:
	case smm_two_way:
	case smm_echo:
		return Cnil;

	default:
		ecl_internal_error("illegal stream mode");
	}
	if (!strm->stream.char_stream_p) {
		/* deduce header and convert to bits */
		output = ecl_times(strm->stream.header != 0xFF ? ecl_one_minus(output) : output, MAKE_FIXNUM(8));
		switch (strm->stream.buffer_state) {
			case 0: break;
			case -1:
				/* bits left for writing, use them */
				output = ecl_plus(output, MAKE_FIXNUM(strm->stream.bits_left));
				break;
			case 1:
				/* bits left for reading, deduce them */
				output = ecl_minus(output, MAKE_FIXNUM(strm->stream.bits_left));
				break;
		}
		/* normalize to byte_size */
		output = ecl_floor2(output, MAKE_FIXNUM(strm->stream.byte_size));
		if (VALUES(1) != MAKE_FIXNUM(0)) {
			ecl_internal_error("File position is not on byte boundary");
		}
	}
	return output;
}

cl_object
ecl_file_position_set(cl_object strm, cl_object large_disp)
{
	ecl_off_t disp;
	int extra = 0;
BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm))
		FEerror("file-position not implemented for CLOS streams", 0);
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	if (strm->stream.closed)
		FEclosed_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_io:
	case smm_output:
		ecl_force_output(strm);
	case smm_input:{
		FILE *fp = (FILE*)strm->stream.file;
		if (!strm->stream.char_stream_p) {
			large_disp = ecl_floor2(ecl_times(large_disp, MAKE_FIXNUM(strm->stream.byte_size)),
					    MAKE_FIXNUM(8));
			extra = fix(VALUES(1));
			/* include the header in byte offset */
			if (strm->stream.header != 0xFF)
				large_disp = ecl_one_plus(large_disp);
			/* flush output stream: required, otherwise internal buffer is lost */
			flush_output_stream_binary(strm);
			/* reset internal buffer: should be set again if extra != 0 */
			strm->stream.bit_buffer = strm->stream.bits_left = strm->stream.buffer_state = 0;
		}
		disp = ecl_integer_to_off_t(large_disp);
		if (fp == NULL)
			wrong_file_handler(strm);
		if (ecl_fseeko(fp, disp, 0) != 0)
			return Cnil;
		if (extra != 0) {
			if (ecl_input_stream_p(strm)) {
				/* prepare the buffer for reading */
				int c = ecl_read_byte8(strm);
				if (c == EOF)
					return Cnil;
				strm->stream.bit_buffer = (c & 0xFF) >> extra;
				strm->stream.bits_left = (8-extra);
				strm->stream.buffer_state = 1;
				/* reset extra to avoid error */
				extra = 0;
			}
			/* FIXME: consider case of output-only stream */
		}
		break;
	}
	case smm_string_output: {
		/* INV: byte_size == 8 */
		disp = fixnnint(large_disp);
		if (disp < strm->stream.object0->base_string.fillp) {
			strm->stream.object0->base_string.fillp = disp;
			strm->stream.int0 = disp;
		} else {
			disp -= strm->stream.object0->base_string.fillp;
			while (disp-- > 0)
				ecl_write_char(' ', strm);
		}
		return Ct;
	}
	case smm_string_input: {
		/* INV: byte_size == 8 */
		disp = fixnnint(large_disp);
		if (disp >= strm->stream.int1) {
			strm->stream.int0 = strm->stream.int1;
		} else {
			strm->stream.int0 = disp;
		}
		return Ct;
	}
	case smm_synonym:
		strm = ecl_symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_broadcast:
		strm = strm->stream.object0;
		if (ecl_endp(strm))
			return Cnil;
		strm = CAR(strm);
		goto BEGIN;

#if defined(ECL_WSOCK)
	case smm_input_wsock:
	case smm_output_wsock:
	case smm_io_wsock:
#endif
	case smm_concatenated:
	case smm_two_way:
	case smm_echo:
		return Cnil;

	default:
		ecl_internal_error("illegal stream mode");
	}
	if (extra) {
		FEerror("Unsupported stream byte size", 0);
	}
	return Ct;
}

cl_object
cl_file_length(cl_object strm)
{
	cl_object output;
BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm))
		FEwrong_type_argument(c_string_to_object("(OR BROADCAST-STREAM SYNONYM-STREAM FILE-STREAM)"),
				      strm);
#endif
	if (type_of(strm) != t_stream) 
		FEtype_error_stream(strm);
	if (strm->stream.closed)
		FEclosed_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_io:
	case smm_output:
		ecl_force_output(strm);
	case smm_input: {
		FILE *fp = (FILE*)strm->stream.file;
		cl_index bs;
		if (fp == NULL)
			wrong_file_handler(strm);
		output = ecl_file_len(fp);
		if (!strm->stream.char_stream_p) {
			bs = strm->stream.byte_size;
			if (strm->stream.header != 0xFF)
				output = ecl_floor2(ecl_minus(ecl_times(ecl_one_minus(output), MAKE_FIXNUM(8)),
				                             MAKE_FIXNUM((8-strm->stream.header)%8)),
						MAKE_FIXNUM(bs));
			else
				output = ecl_floor2(ecl_times(output, MAKE_FIXNUM(8)),
						MAKE_FIXNUM(bs));
			if (VALUES(1) != MAKE_FIXNUM(0)) {
				FEerror("File length is not on byte boundary", 0);
			}
		}
		break;
	}
	case smm_synonym:
		strm = ecl_symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_broadcast:
		strm = strm->stream.object0;
		if (ecl_endp(strm)) {
			output = MAKE_FIXNUM(0);
			break;
		}
		strm = CAR(strm);
		goto BEGIN;

#if defined(ECL_WSOCK)
	case smm_input_wsock:
	case smm_output_wsock:
	case smm_io_wsock:
#endif
	case smm_concatenated:
	case smm_two_way:
	case smm_echo:
	case smm_string_input:
	case smm_string_output:
		FEwrong_type_argument(@'file-stream', strm);

	default:
		ecl_internal_error("illegal stream mode");
	}
	@(return output)
}

cl_object si_file_column(cl_object strm)
{
	@(return MAKE_FIXNUM(ecl_file_column(strm)))
}

int
ecl_file_column(cl_object strm)
{

BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm)) {
		cl_object col = funcall(2, @'gray::stream-line-column', strm);
		/* FIXME! The Gray streams specifies NIL is a valid
		 * value but means "unknown". Should we make it
		 * zero? */
		if (col == Cnil)
			return 0;
		else
			return fixnnint(col);
	}
#endif
	if (type_of(strm) != t_stream)
		FEtype_error_stream(strm);
	if (strm->stream.closed)
		FEclosed_stream(strm);
	switch ((enum ecl_smmode)strm->stream.mode) {
	case smm_output:
#if defined(ECL_WSOCK)
	case smm_output_wsock:
	case smm_io_wsock:
#endif
	case smm_io:
	case smm_two_way:
	case smm_string_output:
		return(strm->stream.int1);

	case smm_synonym:
		strm = ecl_symbol_value(strm->stream.object0);
		goto BEGIN;

	case smm_echo:
		strm = strm->stream.object1;
		goto BEGIN;

	case smm_input:
#if defined(ECL_WSOCK)
	case smm_input_wsock:
#endif
	case smm_string_input:
		return 0;

	case smm_concatenated:
	case smm_broadcast:
		strm = strm->stream.object0;
		if (ecl_endp(strm))
			return 0;
		strm = CAR(strm);
		goto BEGIN;
	default:
		ecl_internal_error("illegal stream mode");
	}
}

cl_object
cl_make_synonym_stream(cl_object sym)
{
	cl_object x;

	sym = ecl_check_cl_type(@'make-synonym-stream',sym,t_symbol);
	x = cl_alloc_object(t_stream);
	x->stream.mode = (short)smm_synonym;
	x->stream.closed = 0;
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
		if (!ecl_output_stream_p(x))
			not_an_output_stream(x);
		streams = CONS(x, streams);
	}
	x = cl_alloc_object(t_stream);
	x->stream.mode = (short)smm_broadcast;
	x->stream.closed = 0;
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
		if (!ecl_input_stream_p(x))
			not_an_input_stream(x);
		streams = CONS(x, streams);
	}
	x = cl_alloc_object(t_stream);
	x->stream.mode = (short)smm_concatenated;
	x->stream.closed = 0;
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
cl_make_two_way_stream(cl_object istrm, cl_object ostrm)
{
	cl_object strm;
	if (!ecl_input_stream_p(istrm))
		not_an_input_stream(istrm);
	if (!ecl_output_stream_p(ostrm))
		not_an_output_stream(ostrm);
	strm = cl_alloc_object(t_stream);
	strm->stream.mode = (short)smm_two_way;
	strm->stream.closed = 0;
	strm->stream.file = NULL;
	strm->stream.object0 = istrm;
	strm->stream.object1 = ostrm;
	strm->stream.int0 = strm->stream.int1 = 0;
	@(return strm)
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
	if (!ecl_input_stream_p(strm1))
		not_an_input_stream(strm1);
	if (!ecl_output_stream_p(strm2))
		not_an_output_stream(strm2);
	output = cl_make_two_way_stream(strm1, strm2);
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
	strng = si_copy_to_simple_base_string(strm->stream.object0);
	strm->stream.object0->base_string.fillp = 0;
	@(return strng)
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
			       byte_size, char_stream_p,
			       (use_header_p != Cnil));
	@(return strm)
@)

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
si_make_string_output_stream_from_string(cl_object s)
{
	cl_object strm;

	if (type_of(s) != t_base_string || !s->base_string.hasfillp)
		FEerror("~S is not a base-string with a fill-pointer.", 1, s);
	strm = cl_alloc_object(t_stream);
	strm->stream.mode = (short)smm_string_output;
	strm->stream.closed = 0;
	strm->stream.file = NULL;
	strm->stream.object0 = s;
	strm->stream.object1 = OBJNULL;
	strm->stream.int0 = s->base_string.fillp;
	strm->stream.int1 = 0;
	strm->stream.char_stream_p = 1;
	strm->stream.byte_size = 8;
	strm->stream.signed_bytes = 0;
	@(return strm)
}

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

cl_object
cl_interactive_stream_p(cl_object strm)
{
	cl_object output = Cnil;
	cl_type t;
 BEGIN:
	t = type_of(strm);
#ifdef ECL_CLOS_STREAMS
	if (t == t_instance)
		return funcall(2, @'gray::stream-interactive-p', strm);
#endif
	if (t != t_stream)
		FEtype_error_stream(strm);
	if (strm->stream.closed)
		FEclosed_stream(strm);
	switch(strm->stream.mode) {
	case smm_synonym:
		strm = ecl_symbol_value(strm->stream.object0);
		goto BEGIN;
	case smm_two_way:
		strm = strm->stream.object0;
		goto BEGIN;
	case smm_input:
	case smm_io:
#ifdef HAVE_ISATTY
		/* Here we should check for the type of file descriptor,
		 * and whether it is connected to a tty. */
		output = isatty(fileno((FILE*)strm->stream.file))? Ct : Cnil;
#endif
		break;
	default:;
	}
	@(return output)
}

cl_object
ecl_make_stream_from_FILE(cl_object fname, void *fp, enum ecl_smmode smm)
{
	cl_object stream;
	stream = cl_alloc_object(t_stream);
	stream->stream.mode = (short)smm;
	stream->stream.closed = 0;
	stream->stream.file = fp;
#if defined (ECL_WSOCK)
	if ( smm == smm_input_wsock || smm == smm_io_wsock )
		stream->stream.object0 = Cnil;
	else
		stream->stream.object0 = @'base-char';
#else
	stream->stream.object0 = @'base-char';
#endif
	stream->stream.object1 = fname; /* not really used */
	stream->stream.int0 = stream->stream.int1 = 0;
	stream->stream.char_stream_p = 1;
	stream->stream.byte_size = 8;
	stream->stream.signed_bytes = 0;
	stream->stream.last_op = 0;
	si_set_finalizer(stream, Ct);
	return(stream);
}

cl_object
ecl_make_stream_from_fd(cl_object fname, int fd, enum ecl_smmode smm)
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
#if defined(ECL_WSOCK)
	if ( smm == smm_input_wsock || smm == smm_output_wsock || smm == smm_io_wsock )
		fp = ( FILE* )fd;
	else
		fp = fdopen( fd, mode );
#else
	fp = fdopen(fd, mode);
#endif
	return ecl_make_stream_from_FILE(fname, fp, smm);
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
		s = ecl_symbol_value(s->stream.object0);
		goto BEGIN;
	case smm_two_way:
		s = output? s->stream.object1 : s->stream.object0;
		goto BEGIN;
	default:
		ecl_internal_error("illegal stream mode");
	}
	return fileno(f);
}

void
init_file(void)
{
	cl_object standard_input;
	cl_object standard_output;
	cl_object error_output;
	cl_object standard;
	cl_object null_stream;
	cl_object x;

	null_stream = cl_alloc_object(t_stream);
	null_stream->stream.mode = (short)smm_io;
	null_stream->stream.closed = 1;
	null_stream->stream.file = NULL;
	null_stream->stream.object0 = @'base-char';
	null_stream->stream.object1 = make_constant_base_string("/dev/null");
	null_stream->stream.int0 = 0;
	null_stream->stream.int1 = 0;
	null_stream->stream.char_stream_p = 1;
	null_stream->stream.byte_size = 8;
	null_stream->stream.signed_bytes = 0;
	null_stream = cl_make_two_way_stream(null_stream, cl_make_broadcast_stream(0));
	cl_core.null_stream = null_stream;

	standard_input = cl_alloc_object(t_stream);
	standard_input->stream.mode = (short)smm_input;
	standard_input->stream.closed = 0;
	standard_input->stream.file = stdin;
	standard_input->stream.object0 = @'base-char';
	standard_input->stream.object1 = make_constant_base_string("stdin");
	standard_input->stream.int0 = 0;
	standard_input->stream.int1 = 0;
	standard_input->stream.char_stream_p = 1;
	standard_input->stream.byte_size = 8;
	standard_input->stream.signed_bytes = 0;

	standard_output = cl_alloc_object(t_stream);
	standard_output->stream.mode = (short)smm_output;
	standard_output->stream.closed = 0;
	standard_output->stream.file = stdout;
	standard_output->stream.object0 = @'base-char';
	standard_output->stream.object1= make_constant_base_string("stdout");
	standard_output->stream.int0 = 0;
	standard_output->stream.int1 = 0;
	standard_output->stream.char_stream_p = 1;
	standard_output->stream.byte_size = 8;
	standard_output->stream.signed_bytes = 0;

	error_output = cl_alloc_object(t_stream);
	error_output->stream.mode = (short)smm_output;
	error_output->stream.closed = 0;
	error_output->stream.file = stderr;
	error_output->stream.object0 = @'base-char';
	error_output->stream.object1= make_constant_base_string("stderr");
	error_output->stream.int0 = 0;
	error_output->stream.int1 = 0;
	error_output->stream.char_stream_p = 1;
	error_output->stream.byte_size = 8;
	error_output->stream.signed_bytes = 0;

	cl_core.terminal_io = standard
	= cl_make_two_way_stream(standard_input, standard_output);

	ECL_SET(@'*terminal-io*', standard);

	x = cl_alloc_object(t_stream);
	x->stream.mode = (short)smm_synonym;
	x->stream.closed = 0;
	x->stream.file = NULL;
	x->stream.object0 = @'*terminal-io*';
	x->stream.object1 = OBJNULL;
	x->stream.int0 = x->stream.int1 = 0;
	standard = x;

	ECL_SET(@'*standard-input*', standard);
	ECL_SET(@'*standard-output*', standard);
	ECL_SET(@'*error-output*', error_output);

	ECL_SET(@'*query-io*', standard);
	ECL_SET(@'*debug-io*', standard);
	ECL_SET(@'*trace-output*', standard);
}
