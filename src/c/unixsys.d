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
#ifdef mingw32
#include "w32api.h"
#include "wtypes.h"
#include "winbase.h"
#include "io.h"
#endif
#ifdef _MSC_VER
#include <windows.h>
#endif

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
#ifdef _MSC_VER
	FEerror("Pipes are not supported under Win32/MSVC", 0);
#else
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
#endif
}

cl_object
si_close_pipe(cl_object stream)
{
#ifdef _MSC_VER
	FEerror("Pipes are not supported under Win32/MSVC", 0);
#else
	if (type_of(stream) == t_stream &&
	    stream->stream.object1 == @'si::open-pipe') {
		stream->stream.mode = smm_closed;
		pclose(stream->stream.file);
		stream->stream.file = NULL;
		stream->stream.object0 = OBJNULL;
	}
	@(return)
#endif
}

@(defun ext::run-program (command argv &key (input @':stream') (output @':stream')
	  		  (error @'t'))
	int parent_write = 0, parent_read = 0;
	int child_pid;
	cl_object stream_write;
	cl_object stream_read;
@{
	command = cl_string(command);
	argv = cl_mapcar(2, @'string', argv);
#if defined(mingw32) || defined (_MSC_VER)
{
	BOOL ok;
	STARTUPINFO st_info;
	PROCESS_INFORMATION pr_info;
	HANDLE child_stdout, child_stdin, child_stderr;
	HANDLE current = GetCurrentProcess();
	HANDLE saved_stdout, saved_stdin, saved_stderr;
	SECURITY_ATTRIBUTES attr;

	/* Enclose each argument, as well as the file name
	   in double quotes, to avoid problems when these
	   arguments or file names have spaces */
	command =
		cl_format(4, Cnil,
			  make_simple_string("~S~{ ~S~}"),
			  command, argv);

	attr.nLength = sizeof(SECURITY_ATTRIBUTES);
	attr.lpSecurityDescriptor = NULL;
	attr.bInheritHandle = TRUE;
	if (input == @':stream') {
		/* Creates a pipe that we can read from what the child
		   writes to it. We duplicate one extreme of the pipe
		   so that the child does not inherit it. */
		HANDLE tmp;
		ok = CreatePipe(&child_stdin, &tmp, &attr, 0);
		if (ok) {
			ok = DuplicateHandle(current, tmp, current,
					     &tmp, 0, FALSE,
					     DUPLICATE_CLOSE_SOURCE |
					     DUPLICATE_SAME_ACCESS);
			if (ok) {
				parent_write = _open_osfhandle(tmp, _O_WRONLY | _O_TEXT);
				if (parent_write < 0)
					printf("open_osfhandle failed\n");
			}
		}
	} else if (input == @'t') {
		/* The child inherits a duplicate of our input
		   handle. Creating a duplicate avoids problems when
		   the child closes it */
		DuplicateHandle(current, GetStdHandle(STD_INPUT_HANDLE),
				current, &child_stdin, 0, TRUE,
				DUPLICATE_SAME_ACCESS);
	} else {
		child_stdin = NULL;
		/*child_stdin = open("/dev/null", O_RDONLY);*/
	}
	if (output == @':stream') {
		/* Creates a pipe that we can write to and the
		   child reads from. We duplicate one extreme of the
		   pipe so that the child does not inherit it. */
		HANDLE tmp;
		ok = CreatePipe(&tmp, &child_stdout, &attr, 0);
		if (ok) {
			ok = DuplicateHandle(current, tmp, current,
					     &tmp, 0, FALSE,
					     DUPLICATE_CLOSE_SOURCE |
					     DUPLICATE_SAME_ACCESS);
			if (ok) {
				parent_read = _open_osfhandle(tmp, _O_RDONLY | _O_TEXT);
				if (parent_read < 0)
					printf("open_osfhandle failed\n");
			}
		}
	} else if (output == @'t') {
		/* The child inherits a duplicate of our output
		   handle. Creating a duplicate avoids problems when
		   the child closes it */
		DuplicateHandle(current, GetStdHandle(STD_OUTPUT_HANDLE),
				current, &child_stdout, 0, TRUE,
				DUPLICATE_SAME_ACCESS);
	} else {
		child_stdout = NULL;
		/*child_stdout = open("/dev/null", O_WRONLY);*/
	}
	if (error == @':output') {
		/* The child inherits a duplicate of its own output
		   handle.*/
		DuplicateHandle(current, child_stdout, current,
				&child_stderr, 0, TRUE,
				DUPLICATE_SAME_ACCESS);
	} else if (error == @'t') {
		/* The child inherits a duplicate of our output
		   handle. Creating a duplicate avoids problems when
		   the child closes it */
		DuplicateHandle(current, GetStdHandle(STD_ERROR_HANDLE),
				current, &child_stderr, 0, TRUE,
				DUPLICATE_SAME_ACCESS);
	} else {
		child_stderr = NULL;
		/*child_stderr = open("/dev/null", O_WRONLY);*/
	}
#if 1
	ZeroMemory(&st_info, sizeof(STARTUPINFO));
	st_info.cb = sizeof(STARTUPINFO);
	st_info.lpTitle = NULL; /* No window title, just exec name */
	st_info.dwFlags = STARTF_USESTDHANDLES; /* Specify std{in,out,err} */
	st_info.hStdInput = child_stdin;
	st_info.hStdOutput = child_stdout;
	st_info.hStdError = child_stderr;
	ZeroMemory(&pr_info, sizeof(PROCESS_INFORMATION));
	ok = CreateProcess(NULL, command->string.self,
			   NULL, NULL, /* lpProcess/ThreadAttributes */
			   TRUE, /* Inherit handles (for files) */
			   /*CREATE_NEW_CONSOLE |*/
			   0,
			   NULL, /* Inherit environment */
			   NULL, /* Current directory */
			   &st_info, /* Startup info */
			   &pr_info); /* Process info */
#else
	saved_stdin = GetStdHandle(STD_INPUT_HANDLE);
	saved_stdout = GetStdHandle(STD_OUTPUT_HANDLE);
	saved_stderr = GetStdHandle(STD_ERROR_HANDLE);
	SetStdHandle(STD_INPUT_HANDLE, child_stdin);
	SetStdHandle(STD_OUTPUT_HANDLE, child_stdout);
	SetStdHandle(STD_ERROR_HANDLE, child_stderr);
	ZeroMemory(&st_info, sizeof(STARTUPINFO));
	st_info.cb = sizeof(STARTUPINFO);
	ZeroMemory(&pr_info, sizeof(PROCESS_INFORMATION));
	ok = CreateProcess(NULL, command->string.self,
			   NULL, NULL, /* lpProcess/ThreadAttributes */
			   TRUE, /* Inherit handles (for files) */
			   /*CREATE_NEW_CONSOLE |*/
			   0,
			   NULL, /* Inherit environment */
			   NULL, /* Current directory */
			   &st_info, /* Startup info */
			   &pr_info); /* Process info */
	SetStdHandle(STD_INPUT_HANDLE, saved_stdin);
	SetStdHandle(STD_OUTPUT_HANDLE, saved_stdout);
	SetStdHandle(STD_ERROR_HANDLE, saved_stderr);
#endif
	if (ok) {
		CloseHandle(pr_info.hProcess);
		CloseHandle(pr_info.hThread);
		child_pid = pr_info.dwProcessId;
	} else {
		const char *message;
		FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM |
			      FORMAT_MESSAGE_ALLOCATE_BUFFER,
			      0, GetLastError(), 0, (void*)&message, 0, NULL);
		printf("%s\n", message);
		LocalFree(message);
		child_pid = -1;
	}
}
#else
{
	int child_stdin, child_stdout, child_stderr;
	argv = CONS(command, nconc(argv, CONS(Cnil, Cnil)));
	argv = cl_funcall(3, @'coerce', argv, @'vector');
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
		execvp(command->string.self, argv_ptr);
		/* at this point exec has failed */
		perror("exec");
		abort();
	}
	close(child_stdin);
	close(child_stdout);
	close(child_stderr);
}
#endif
	if (child_pid < 0) {
		if (parent_write) close(parent_write);
		if (parent_read) close(parent_read);
		parent_write = 0;
		parent_read = 0;
		FEerror("Could not spawn subprocess to run ~S.", 1, command);
	}
	if (parent_write > 0) {
		stream_write = ecl_make_stream_from_fd(command, parent_write,
						       smm_output);
	} else {
		parent_write = 0;
		stream_write = cl_core.null_stream;
	}
	if (parent_read > 0) {
		stream_read = ecl_make_stream_from_fd(command, parent_read,
						      smm_input);
	} else {
		parent_read = 0;
		stream_read = cl_core.null_stream;
	}
	@(return ((parent_read || parent_write)?
		  make_two_way_stream(stream_read, stream_write) :
		  Cnil))
@)
