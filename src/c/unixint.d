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
#include "internal.h"
#include <signal.h>
#if defined(mingw32) || defined(_MSC_VER)
# include <windows.h>
#endif
#if !defined(_MSC_VER)
# include <unistd.h>
#endif

/******************************* ------- ******************************/

bool ecl_interrupt_enable;

static void
handle_signal(int sig)
{
	switch (sig) {
#if defined(ECL_THREADS) && !defined(_MSC_VER) && !defined(mingw32)
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

/*
 * TODO: Use POSIX signals, and in particular use sigaltstack to
 * handle stack overflows gracefully.
 */
static void
signal_catcher(int sig)
{
	if (!ecl_interrupt_enable ||
	    symbol_value(@'si::*interrupt-enable*') == Cnil) {
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

#ifdef _MSC_VER
LONG WINAPI W32_exception_filter(struct _EXCEPTION_POINTERS* ep)
{
	LONG excpt_result;

	excpt_result = EXCEPTION_CONTINUE_EXECUTION;
	switch (ep->ExceptionRecord->ExceptionCode)
	{
		/* Catch all arithmetic exceptions */
		case EXCEPTION_INT_DIVIDE_BY_ZERO:
		case EXCEPTION_INT_OVERFLOW:
		case EXCEPTION_FLT_DIVIDE_BY_ZERO:
		case EXCEPTION_FLT_OVERFLOW:
		case EXCEPTION_FLT_UNDERFLOW:
		case EXCEPTION_FLT_INEXACT_RESULT:
		case EXCEPTION_FLT_DENORMAL_OPERAND:
		case EXCEPTION_FLT_INVALID_OPERATION:
		case EXCEPTION_FLT_STACK_CHECK:
			handle_signal(SIGFPE);
			break;
		/* Catch segmentation fault */
		case EXCEPTION_ACCESS_VIOLATION:
			handle_signal(SIGSEGV);
			break;
		/* Catch illegal instruction */
		case EXCEPTION_ILLEGAL_INSTRUCTION:
			handle_signal(SIGILL);
			break;
		/* Do not catch anything else */
		default:
			excpt_result = EXCEPTION_CONTINUE_SEARCH;
			break;
	}

	return excpt_result;
}

BOOL WINAPI W32_console_ctrl_handler(DWORD type)
{
	switch (type)
	{
		/* Catch CTRL-C */
		case CTRL_C_EVENT:
			handle_signal(SIGINT);
			return TRUE;
	}
	return FALSE;
}
#endif

void
init_unixint(void)
{
	signal(SIGFPE, signal_catcher);
	signal(SIGINT, signal_catcher);
#if defined(ECL_THREADS) && !defined(_MSC_VER) && !defined(mingw32)
	signal(SIGUSR1, signal_catcher);
#endif
#ifdef _MSC_VER
	SetUnhandledExceptionFilter(W32_exception_filter);
	SetConsoleCtrlHandler(W32_console_ctrl_handler, TRUE);
#endif
	ECL_SET(@'si::*interrupt-enable*', Ct);
	ecl_interrupt_enable = 1;
}
