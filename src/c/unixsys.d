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
#include <fcntl.h>
#include "ecl.h"
#include "internal.h"

cl_object
si_system(cl_object cmd)
{
	volatile int code;

	assert_type_string(cmd);
	cmd = copy_simple_string(cmd);
	code = system((const char *)(cmd->string.self));
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

@(defun ext::run-program (command argv &key (input @':stream') (output @':stream')
	  		  (error @'nil'))
	cl_object input_pipe_read, input_pipe_write;
	cl_object output_pipe_read, output_pipe_write;
	int input_pipe[2], output_pipe[2];
	int child_stdin, child_stdout, child_stderr;
	int parent_write = 0, parent_read = 0;
	int child_pid;
	cl_object stream_write;
	cl_object stream_read;
@{
	if (input == @':stream') {
		int fd[2];
		pipe(fd);
		parent_write = fd[1];
		child_stdin = fd[0];
	} else if (input == @'t') {
		child_stdin = dup(0);
	} else {
		child_stdin = open("/dev/null", O_RDONLY);
	}
	if (output == @':stream') {
		int fd[2];
		pipe(fd);
		parent_read = fd[0];
		child_stdout = fd[1];
	} else if (output == @'t') {
		child_stdout = dup(1);
	} else {
		child_stdout = open("/dev/null", O_WRONLY);
	}
	if (error == @':output') {
		child_stderr = dup(child_stdout);
	} else if (error == @'t') {
		child_stderr = dup(2);
	} else {
		child_stderr = open("/dev/null", O_WRONLY);
	}
	command = cl_string(command);
	argv = cl_mapcar(2, @'string', argv);
	argv = CONS(command, nconc(argv, CONS(Cnil, Cnil)));
	argv = cl_funcall(3, @'coerce', argv, @'vector');
	child_pid = fork();
	if (child_pid == 0) {
		/* Child */
		int j;
		void **argv_ptr = (void **)argv->vector.self.t;
		close(0);
		dup(child_stdin);
		if (parent_write) close(parent_write);
		close(1);
		dup(child_stdout);
		if (parent_read) close(parent_read);
		close(2);
		dup(child_stderr);
		for (j = 0; j < argv->vector.fillp; j++) {
			cl_object arg = argv->vector.self.t[j];
			if (arg == Cnil) {
				argv_ptr[j] = NULL;
			} else {
				argv_ptr[j] = arg->string.self;
			}
		}
		if (execvp(command->string.self, (const char **)argv_ptr) < 0) {
			abort();
		}
	} else {
		/* Parent */
		close(child_stdin);
		close(child_stdout);
		close(child_stderr);
		if (child_pid < 0) {
			if (parent_write) close(parent_write);
			if (parent_read) close(parent_read);
			FEerror("Could not spawn subprocess to run ~S.", 1, command);
		}
		if (parent_write) {
			stream_write = ecl_make_stream_from_fd(command, parent_write,
							       smm_output);
		} else {
			stream_write = cl_core.null_stream;
		}
		if (parent_read) {
			stream_read = ecl_make_stream_from_fd(command, parent_read,
							      smm_input);
		} else {
			stream_read = cl_core.null_stream;
		}
	}
	@(return ((parent_read || parent_write)?
		  make_two_way_stream(stream_read, stream_write) :
		  Cnil))
@)
