/*
    unixint.c -- Unix interrupt interface.
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
#include <signal.h>
#include <unistd.h>
#ifdef ECL_THREADS
#include <pthread.h>
#endif

/******************************* ------- ******************************/

void
handle_signal(int sig)
{
	switch (sig) {
#ifdef ECL_THREADS
	case SIGUSR1:
		funcall(1, cl_env.own_process->process.interrupt);
		break;
#endif
	case SIGINT:
		funcall(2, @'si::terminal-interrupt', Ct);
		break;
	case SIGFPE:
		FEerror("Floating-point exception.", 0);
		break;
	case SIGSEGV:
		FEerror("Segmentation violation.", 0);
		break;
	default:
		FEerror("Serious signal ~D caught.", 0, MAKE_FIXNUM(sig));
	}
}

static void
signal_catcher(int sig)
{
	if (symbol_value(@'si::*interrupt-enable*') == Cnil) {
		signal(sig, signal_catcher);
		cl_env.interrupt_pending = sig;
		return;
	}
	signal(sig, signal_catcher);
#ifdef HAVE_SIGPROCMASK
	CL_UNWIND_PROTECT_BEGIN {
		handle_signal(sig);
	} CL_UNWIND_PROTECT_EXIT {
		sigset_t block_mask;
		sigemptyset(&block_mask);
		sigaddset(&block_mask, sig);
#ifdef ECL_THREADS
		pthread_sigmask(SIG_UNBLOCK, &block_mask, NULL);
#else
		sigprocmask(SIG_UNBLOCK, &block_mask, NULL);
#endif
	} CL_UNWIND_PROTECT_END;
#else
	handle_signal(sig);
#endif
}

cl_object
si_check_pending_interrupts(void)
{
	int what = cl_env.interrupt_pending;
	cl_env.interrupt_pending = 0;
	handle_signal(what);
	@(return)
}

cl_object
si_catch_bad_signals()
{
	signal(SIGILL, signal_catcher);
#ifndef GBC_BOEHM
	signal(SIGBUS, signal_catcher);
#endif
	signal(SIGSEGV, signal_catcher);
#ifdef SIGIOT
	signal(SIGIOT, signal_catcher);
#endif
#ifdef SIGEMT
	signal(SIGEMT, signal_catcher);
#endif
#ifdef SIGSYS
	signal(SIGSYS, signal_catcher);
#endif
	@(return Ct)
}

cl_object
si_uncatch_bad_signals()
{
	signal(SIGILL, SIG_DFL);
#ifndef GBC_BOEHM
	signal(SIGBUS, SIG_DFL);
#endif
	signal(SIGSEGV, SIG_DFL);
#ifdef SIGIOT
	signal(SIGIOT, SIG_DFL);
#endif
#ifdef SIGEMT
	signal(SIGEMT, SIG_DFL);
#endif
#ifdef SIGSYS
	signal(SIGSYS, SIG_DFL);
#endif
	@(return Ct)
}

void
init_unixint(void)
{
	signal(SIGFPE, signal_catcher);
	signal(SIGINT, signal_catcher);
#ifdef ECL_THREADS
	signal(SIGUSR1, signal_catcher);
#endif
	ECL_SET(@'si::*interrupt-enable*', Ct);
}
