/* -*- mode: c; c-basic-offset: 8 -*- */
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

#include <ecl/ecl.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <ecl/internal.h>
#ifdef mingw32
#include <w32api.h>
#include <wtypes.h>
#include <winbase.h>
#include <io.h>
#endif
#ifdef _MSC_VER
#include <windows.h>
#endif

cl_object
si_system(cl_object cmd_string)
{
	cl_object cmd = si_copy_to_simple_base_string(cmd_string);
	int code = system((const char *)(cmd->base_string.self));
	/* FIXME! Are there any limits for system()? */
	/* if (cmd->base_string.fillp >= 1024)
		FEerror("Too long command line: ~S.", 1, cmd);*/
	/* FIXME! This is a non portable way of getting the exit code */
	@(return MAKE_FIXNUM(code >> 8))
}

cl_object
si_getpid(void)
{
	@(return MAKE_FIXNUM(getpid()))
}

cl_object
si_make_pipe()
{
	cl_object output;
	int fds[2], ret;
#if defined(_MSC_VER) || defined(mingw32)
	ret = _pipe(fds, 4096, _O_BINARY);
#else
	ret = pipe(fds);
#endif
	if (ret < 0) {
		FElibc_error("Unable to create pipe", 0);
		output = Cnil;
	} else {
		cl_object fake_in_name = make_simple_base_string("PIPE-READ-ENDPOINT");
		cl_object in = ecl_make_stream_from_fd(fake_in_name, fds[0], smm_input);
		cl_object fake_out_name = make_simple_base_string("PIPE-WRITE-ENDPOINT");
		cl_object out = ecl_make_stream_from_fd(fake_out_name, fds[1], smm_output);
		output = cl_make_two_way_stream(in, out);
	}
	@(return output)
}

@(defun ext::run-program (command argv &key (input @':stream') (output @':stream')
	  		  (error @'t') (wait @'t'))
	int parent_write = 0, parent_read = 0;
	int child_pid;
	cl_object stream_write;
	cl_object stream_read;
@{
	command = si_copy_to_simple_base_string(command);
	argv = cl_mapcar(2, @'si::copy-to-simple-base-string', argv);
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
			  make_simple_base_string("~S~{ ~S~}"),
			  command, argv);
	command = ecl_null_terminated_base_string(command);

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
				parent_write = _open_osfhandle(tmp, _O_WRONLY /*| _O_TEXT*/);
				if (parent_write < 0)
					printf("open_osfhandle failed\n");
			}
		}
	} else if (input == @'t') {
		/* The child inherits a duplicate of our input
		   handle. Creating a duplicate avoids problems when
		   the child closes it */
		int stream_handle = ecl_stream_to_handle(SYM_VAL(@'*standard-input*'), 0);
		if (stream_handle >= 0)
			DuplicateHandle(current, _get_osfhandle(stream_handle) /*GetStdHandle(STD_INPUT_HANDLE)*/,
					current, &child_stdin, 0, TRUE,
					DUPLICATE_SAME_ACCESS);
		else
			child_stdin = NULL;
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
				parent_read = _open_osfhandle(tmp, _O_RDONLY /*| _O_TEXT*/);
				if (parent_read < 0)
					printf("open_osfhandle failed\n");
			}
		}
	} else if (output == @'t') {
		/* The child inherits a duplicate of our output
		   handle. Creating a duplicate avoids problems when
		   the child closes it */
		int stream_handle = ecl_stream_to_handle(SYM_VAL(@'*standard-output*'), 1);
		if (stream_handle >= 0)
			DuplicateHandle(current, _get_osfhandle(stream_handle) /*GetStdHandle(STD_OUTPUT_HANDLE)*/,
					current, &child_stdout, 0, TRUE,
					DUPLICATE_SAME_ACCESS);
		else
			child_stdout = NULL;
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
		int stream_handle = ecl_stream_to_handle(SYM_VAL(@'*error-output*'), 1);
		if (stream_handle >= 0)
			DuplicateHandle(current, _get_osfhandle(stream_handle) /*GetStdHandle(STD_ERROR_HANDLE)*/,
					current, &child_stderr, 0, TRUE,
					DUPLICATE_SAME_ACCESS);
		else
			child_stderr = NULL;
	} else {
		child_stderr = NULL;
		/*child_stderr = open("/dev/null", O_WRONLY);*/
	}
#if 1
	ZeroMemory(&st_info, sizeof(STARTUPINFO));
	st_info.cb = sizeof(STARTUPINFO);
	st_info.lpTitle = NULL; /* No window title, just exec name */
	st_info.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW; /* Specify std{in,out,err} */
	st_info.wShowWindow = SW_HIDE;
	st_info.hStdInput = child_stdin;
	st_info.hStdOutput = child_stdout;
	st_info.hStdError = child_stderr;
	ZeroMemory(&pr_info, sizeof(PROCESS_INFORMATION));
	ok = CreateProcess(NULL, command->base_string.self,
			   NULL, NULL, /* lpProcess/ThreadAttributes */
			   TRUE, /* Inherit handles (for files) */
			   /*CREATE_NEW_CONSOLE |*/
			   0 /*(input == Ct || output == Ct || error == Ct ? 0 : CREATE_NO_WINDOW)*/,
			   NULL, /* Inherit environment */
			   NULL, /* Current directory */
			   &st_info, /* Startup info */
			   &pr_info); /* Process info */
#else /* 1 */
	saved_stdin = GetStdHandle(STD_INPUT_HANDLE);
	saved_stdout = GetStdHandle(STD_OUTPUT_HANDLE);
	saved_stderr = GetStdHandle(STD_ERROR_HANDLE);
	SetStdHandle(STD_INPUT_HANDLE, child_stdin);
	SetStdHandle(STD_OUTPUT_HANDLE, child_stdout);
	SetStdHandle(STD_ERROR_HANDLE, child_stderr);
	ZeroMemory(&st_info, sizeof(STARTUPINFO));
	st_info.cb = sizeof(STARTUPINFO);
	ZeroMemory(&pr_info, sizeof(PROCESS_INFORMATION));
	ok = CreateProcess(NULL, command->base_string.self,
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
#endif /* 1 */
	/* Child handles must be closed in the parent process */
	/* otherwise the created pipes are never closed       */
	if (child_stdin) CloseHandle(child_stdin);
	if (child_stdout) CloseHandle(child_stdout);
	if (child_stderr) CloseHandle(child_stderr);
	if (ok) {
		CloseHandle(pr_info.hThread);
		child_pid = pr_info.dwProcessId;
		if (wait != Cnil) {
			  WaitForSingleObject(pr_info.hProcess, INFINITE);
		}
		CloseHandle(pr_info.hProcess);
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
#else /* mingw */
{
	int child_stdin, child_stdout, child_stderr;
	argv = CONS(command, ecl_nconc(argv, ecl_list1(Cnil)));
	argv = cl_funcall(3, @'coerce', argv, @'vector');
	if (input == @':stream') {
		int fd[2];
		pipe(fd);
		parent_write = fd[1];
		child_stdin = fd[0];
	} else {
		child_stdin = -1;
		if (input == @'t')
			child_stdin = ecl_stream_to_handle(SYM_VAL(@'*standard-input*'), 0);
		if (child_stdin >= 0)
			child_stdin = dup(child_stdin);
		else
			child_stdin = open("/dev/null", O_RDONLY);
	}
	if (output == @':stream') {
		int fd[2];
		pipe(fd);
		parent_read = fd[0];
		child_stdout = fd[1];
	} else {
		child_stdout = -1;
		if (output == @'t')
			child_stdout = ecl_stream_to_handle(SYM_VAL(@'*standard-output*'), 1);
		if (child_stdout >= 0)
			child_stdout = dup(child_stdout);
		else
			child_stdout = open("/dev/null", O_WRONLY);
	}
	if (error == @':output') {
		child_stderr = child_stdout;
	} else if (error == @'t') {
		child_stderr = ecl_stream_to_handle(SYM_VAL(@'*error-output*'), 1);
	} else {
		child_stderr = -1;
	}
	if (child_stderr < 0) {
		child_stderr = open("/dev/null", O_WRONLY);
	} else {
		child_stderr = dup(child_stderr);
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
				argv_ptr[j] = arg->base_string.self;
			}
		}
		execvp(command->base_string.self, argv_ptr);
		/* at this point exec has failed */
		perror("exec");
		abort();
	}
	close(child_stdin);
	close(child_stdout);
	close(child_stderr);
	if (child_pid > 0 && wait != Cnil) {
	   	int status[0];
		waitpid(child_pid, status, 0);
	}
}
#endif /* mingw */
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
		  cl_make_two_way_stream(stream_read, stream_write) :
		  Cnil))
@)
