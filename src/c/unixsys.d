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

#include "ecl.h"

#if !defined(__stdlib_h) && !defined(_STDLIB_H_) && !defined(__STDLIB_H__) &&  !defined(_STDLIB_H)
#include <signal.h>
int
system(const char *command)
{
	char buf[4];
	extern sigint();

	signal(SIGINT, SIG_IGN);
	write(4, command, strlen(command)+1);
	read(5, buf, 1);
	signal(SIGINT, sigint);
	return(buf[0]<<8);
}
#endif __STDLIB_H__

#if defined(__FreeBSD__) || defined(__NetBSD__)

/* due to the calls to realloc in system.c/exec.c (memory which hasn't been
malloc'ed can't be realloced in ecl) we have to patch this a bit.
We use execv and supply the arg list, so execl doesn't have to realloc. CvdL */

#include <sys/types.h>
#include <sys/signal.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <stddef.h>
#include <unistd.h>
#include <paths.h>

int
system(command)
	const char *command;
{
	union wait pstat;
	pid_t pid;
	int omask;
	sig_t intsave, quitsave;

	if (!command)		/* just checking... */
		return(1);

	omask = sigblock(sigmask(SIGCHLD));
	switch(pid = vfork()) {
	case -1:			/* error */
		(void)sigsetmask(omask);
		pstat.w_status = 0;
		pstat.w_retcode = 127;
		return(pstat.w_status);
	case 0:	{			/* child */
		const char *args[] = { "sh", "-c", command, (char *)NULL };
		(void)sigsetmask(omask);
	        execv(_PATH_BSHELL, args);
		_exit(127);
	}
	}
	intsave = signal(SIGINT, SIG_IGN);
	quitsave = signal(SIGQUIT, SIG_IGN);
	pid = waitpid(pid, (int *)&pstat, 0);
	(void)sigsetmask(omask);
	(void)signal(SIGINT, intsave);
	(void)signal(SIGQUIT, quitsave);
	return(pid == -1 ? -1 : pstat.w_status);
}
#endif

@(defun si::system (cmd)
	volatile char *s;
	volatile int code;
@
	assert_type_string(cmd);
	s = cmd->string.self;
	code = system(s);
	/* FIXME! Are there any limits for system()? */
	/* if (cmd->string.fillp >= 1024)
		FEerror("Too long command line: ~S.", 1, cmd);*/
	/* FIXME! This is a non portable way of getting the exit code */
	@(return MAKE_FIXNUM(code >> 8))
@)

@(defun si::open_pipe (cmd)
  FILE *ptr;
  cl_object stream;
@
  assert_type_string(cmd);
 
  if ((ptr = popen(cmd->string.self, OPEN_R)) == NULL)
    @(return Cnil)
  stream = alloc_object(t_stream);
  stream->stream.mode = smm_input;
  stream->stream.file = ptr;
  stream->stream.object0 = @'base-char';
  stream->stream.object1 = cmd;
  stream->stream.int0 = stream->stream.int1 = 0;
#if !defined(GBC_BOEHM)
  setbuf(ptr, stream->stream.buffer = alloc_atomic(BUFSIZ));
#endif
  @(return stream)
@)
