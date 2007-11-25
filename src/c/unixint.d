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

#include <ecl/ecl.h>
#if defined(HAVE_FENV_H)
# define _GNU_SOURCE
# include <fenv.h>
# ifndef FE_UNDERFLOW
#  define FE_UNDERFLOW 0
# endif
# ifndef FE_OVERFLOW
#  define FE_OVERFLOW 0
# endif
# ifndef FE_INVALID
#  define FE_INVALID 0
# endif
# ifndef FE_DIVBYZERO
#  define FE_DIVBYZERO 0
# endif
# ifndef FE_INEXACT
#  define FE_INEXACT 0
# endif
#endif
#include <signal.h>
#if defined(mingw32) || defined(_MSC_VER)
# include <windows.h>
void handle_fpe_signal(int,int);
#endif
#if !defined(_MSC_VER)
# include <unistd.h>
#endif
#include <ecl/internal.h>

/******************************* ------- ******************************/

bool ecl_interrupt_enable;

#ifdef HAVE_SIGPROCMASK
static void
mysignal(int code, void *handler)
{
	struct sigaction new_action, old_action;

#ifdef SA_SIGINFO
	new_action.sa_sigaction = handler;
	sigemptyset(&new_action.sa_mask);
	new_action.sa_flags = SA_SIGINFO;
#else
	new_action.sa_handler = handler;
	sigemptyset(&new_action.sa_mask);
	new_action.sa_flags = 0;
#endif
	sigaction(code, &new_action, &old_action);
}
#else
#define mysignal(x,y) signal(x,y)
#endif

static void
#ifdef SA_SIGINFO
handle_signal(int sig, siginfo_t *info, void *aux)
#else
handle_signal(int sig)
#endif
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
	case SIGFPE: {
		cl_object condition = @'arithmetic-error';
#if defined(HAVE_FENV_H)
		int bits = fetestexcept(FE_ALL_EXCEPT);
		if (bits & FE_DIVBYZERO)
			condition = @'division-by-zero';
		if (bits & FE_OVERFLOW)
			condition = @'floating-point-overflow';
		if (bits & FE_UNDERFLOW)
			condition = @'floating-point-underflow';
		if (bits & FE_INEXACT)
			condition = @'floating-point-inexact';
		if (bits & FE_INVALID)
			condition = @'floating-point-invalid-operation';
#endif
#ifdef SA_SIGINFO
		if (info) {
			if (info->si_code == FPE_INTDIV || info->si_code == FPE_FLTDIV)
				condition = @'division-by-zero';
			if (info->si_code == FPE_FLTOVF)
				condition = @'floating-point-overflow';
			if (info->si_code == FPE_FLTUND)
				condition = @'floating-point-underflow';
		}
#endif
		si_trap_fpe(@'last', Ct);
		cl_error(1, condition);
		break;
	}
	case SIGSEGV:
		FEerror("Segmentation violation.", 0);
		break;
	default:
		FEerror("Serious signal ~D caught.", 1, MAKE_FIXNUM(sig));
	}
}

/*
 * TODO: Use POSIX signals, and in particular use sigaltstack to
 * handle stack overflows gracefully.
 */
static void
#ifdef SA_SIGINFO
signal_catcher(int sig, siginfo_t *siginfo, void *data)
#else
signal_catcher(int sig)
#endif
{
	if (!ecl_interrupt_enable ||
	    ecl_symbol_value(@'si::*interrupt-enable*') == Cnil) {
		mysignal(sig, signal_catcher);
		cl_env.interrupt_pending = sig;
		return;
	}
	mysignal(sig, signal_catcher);
#ifdef HAVE_SIGPROCMASK
	CL_UNWIND_PROTECT_BEGIN {
#ifdef SA_SIGINFO
		handle_signal(sig, siginfo, data);
#else
		handle_signal(sig);
#endif
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
#if defined (_MSC_VER)
	if (sig == SIGFPE) {
		handle_fpe_signal(sig, _fpecode);
	}
#endif
	handle_signal(sig);
#endif
}

cl_object
si_check_pending_interrupts(void)
{
	int what = cl_env.interrupt_pending;
	cl_env.interrupt_pending = 0;
	if (what) {
#if defined (HAVE_SIGPROCMASK) && defined(SA_SIGINFO)
		handle_signal(what, 0, 0);
#else
		handle_signal(what);
#endif
	}
	@(return)
}

cl_object
si_catch_bad_signals()
{
	mysignal(SIGILL, signal_catcher);
#ifndef GBC_BOEHM
	mysignal(SIGBUS, signal_catcher);
#endif
	mysignal(SIGSEGV, signal_catcher);
#ifdef SIGIOT
	mysignal(SIGIOT, signal_catcher);
#endif
#ifdef SIGEMT
	mysignal(SIGEMT, signal_catcher);
#endif
#ifdef SIGSYS
	mysignal(SIGSYS, signal_catcher);
#endif
	@(return Ct)
}

cl_object
si_uncatch_bad_signals()
{
	mysignal(SIGILL, SIG_DFL);
#ifndef GBC_BOEHM
	mysignal(SIGBUS, SIG_DFL);
#endif
	mysignal(SIGSEGV, SIG_DFL);
#ifdef SIGIOT
	mysignal(SIGIOT, SIG_DFL);
#endif
#ifdef SIGEMT
	mysignal(SIGEMT, SIG_DFL);
#endif
#ifdef SIGSYS
	mysignal(SIGSYS, SIG_DFL);
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

void handle_fpe_signal(int sig, int num)
{
	cl_object condition = @'arithmetic-error';

	switch (num) {
	case _FPE_OVERFLOW:
		condition = @'floating-point-overflow';
		break;
	case _FPE_UNDERFLOW:
		condition = @'floating-point-underflow';
		break;
	case _FPE_ZERODIVIDE:
		condition = @'division-by-zero';
		break;
	}

	si_trap_fpe(@'last', Ct);
	cl_error(1, condition);
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

cl_object
si_trap_fpe(cl_object condition, cl_object flag)
{
#if (defined(HAVE_FENV_H) && defined(HAVE_FEENABLEEXCEPT)) || defined(_MSC_VER) || defined(mingw32)
	static int last_bits = 0;
	int bits = 0;
	if (condition == @'division-by-zero')
		bits = FE_DIVBYZERO;
	else if (condition == @'floating-point-overflow')
		bits = FE_OVERFLOW;
	else if (condition == @'floating-point-underflow')
		bits = FE_UNDERFLOW;
	else if (condition == Ct)
		bits = FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW;
	else if (condition == @'last')
		bits = last_bits;
#if defined(_MSC_VER) || defined(mingw32)
	_fpreset();
#endif
	if (bits) {
		if (flag == Cnil) {
			fedisableexcept(bits);
			last_bits &= ~bits;
		} else {
			feenableexcept(bits);
			last_bits |= bits;
		}
	}
#endif
	@(return flag)
}	

void
init_unixint(void)
{
	mysignal(SIGFPE, signal_catcher);
	si_trap_fpe(Ct, Ct);
	mysignal(SIGINT, signal_catcher);
#if defined(ECL_THREADS) && !defined(_MSC_VER) && !defined(mingw32)
	mysignal(SIGUSR1, signal_catcher);
#endif
#ifdef _MSC_VER
	SetUnhandledExceptionFilter(W32_exception_filter);
	SetConsoleCtrlHandler(W32_console_ctrl_handler, TRUE);
#endif
	ECL_SET(@'si::*interrupt-enable*', Ct);
	ecl_interrupt_enable = 1;
}
