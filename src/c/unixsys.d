/*
    unixsys.s  -- Unix shell interface.
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

#include <stdlib.h>
#include "ecl.h"
#include "machines.h"
#include "internal.h"

cl_object
si_system(cl_object cmd)
{
	volatile char *s;
	volatile int code;

	assert_type_string(cmd);
	s = cmd->string.self;
	code = system((const char *)s);
	/* FIXME! Are there any limits for system()? */
	/* if (cmd->string.fillp >= 1024)
		FEerror("Too long command line: ~S.", 1, cmd);*/
	/* FIXME! This is a non portable way of getting the exit code */
	@(return MAKE_FIXNUM(code >> 8))
}

cl_object
si_open_pipe(cl_object cmd)
{
	FILE *ptr;
	cl_object stream;

	assert_type_string(cmd);
	ptr = popen(cmd->string.self, "r");
	if (ptr == NULL)
		@(return Cnil);
	stream = cl_alloc_object(t_stream);
	stream->stream.mode = smm_input;
	stream->stream.file = ptr;
	stream->stream.object0 = @'base-char';
	stream->stream.object1 = @'si::open-pipe';
	stream->stream.int0 = stream->stream.int1 = 0;
#if !defined(GBC_BOEHM)
	setbuf(ptr, stream->stream.buffer = cl_alloc_atomic(BUFSIZ));
#endif
	@(return stream)
}

cl_object
si_close_pipe(cl_object stream)
{
	if (type_of(stream) == t_stream &&
	    stream->stream.object1 == @'si::open-pipe') {
		stream->stream.mode = smm_closed;
		pclose(stream->stream.file);
		stream->stream.file = NULL;
		stream->stream.object0 = OBJNULL;
	}
	@(return)
}
