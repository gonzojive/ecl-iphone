/* -*- mode: c; c-basic-offset: 8 -*- */
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
#if defined(HAVE_FENV_H) && !defined(ECL_AVOID_FENV_H)
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

static struct {
	int code;
	char *text;
} known_signals[] = {
#ifdef SIGHUP
	{ SIGHUP, "+SIGHUP+" },
#endif
#ifdef SIGINT
	{ SIGINT, "+SIGINT+" },
#endif
#ifdef SIGQUIT
	{ SIGQUIT, "+SIGQUIT+" },
#endif
#ifdef SIGILL
	{ SIGILL, "+SIGILL+" },
#endif
#ifdef SIGTRAP
	{ SIGTRAP, "+SIGTRAP+" },
#endif
#ifdef SIGABRT
	{ SIGABRT, "+SIGABRT+" },
#endif
#ifdef SIGEMT
	{ SIGEMT, "+SIGEMT+" },
#endif
#ifdef SIGFPE
	{ SIGFPE, "+SIGFPE+" },
#endif
#ifdef SIGKILL
	{ SIGKILL, "+SIGKILL+" },
#endif
#ifdef SIGBUS
	{ SIGBUS, "+SIGBUS+" },
#endif
#ifdef SIGSEGV
	{ SIGSEGV, "+SIGSEGV+" },
#endif
#ifdef SIGSYS
	{ SIGSYS, "+SIGSYS+" },
#endif
#ifdef SIGPIPE
	{ SIGPIPE, "+SIGPIPE+" },
#endif
#ifdef SIGALRM
	{ SIGALRM, "+SIGALRM+" },
#endif
#ifdef SIGTERM
	{ SIGTERM, "+SIGTERM+" },
#endif
#ifdef SIGURG
	{ SIGURG, "+SIGURG+" },
#endif
#ifdef SIGSTOP
	{ SIGSTOP, "+SIGSTOP+" },
#endif
#ifdef SIGTSTP
	{ SIGTSTP, "+SIGTSTP+" },
#endif
#ifdef SIGCONT
	{ SIGCONT, "+SIGCONT+" },
#endif
#ifdef SIGCHLD
	{ SIGCHLD, "+SIGCHLD+" },
#endif
#ifdef SIGTTIN
	{ SIGTTIN, "+SIGTTIN+" },
#endif
#ifdef SIGTTOU
	{ SIGTTOU, "+SIGTTOU+" },
#endif
#ifdef SIGIO
	{ SIGIO, "+SIGIO+" },
#endif
#ifdef SIGXCPU
	{ SIGXCPU, "+SIGXCPU+" },
#endif
#ifdef SIGXFSZ
	{ SIGXFSZ, "+SIGXFSZ+" },
#endif
#ifdef SIGVTALRM
	{ SIGVTALRM, "+SIGVTALRM+" },
#endif
#ifdef SIGPROF
	{ SIGPROF, "+SIGPROF+" },
#endif
#ifdef SIGWINCH
	{ SIGWINCH, "+SIGWINCH+" },
#endif
#ifdef SIGINFO
	{ SIGINFO, "+SIGINFO+" },
#endif
#ifdef SIGUSR1
	{ SIGUSR1, "+SIGUSR1+" },
#endif
#ifdef SIGUSR2
	{ SIGUSR2, "+SIGUSR2+" },
#endif
#ifdef SIGTHR
	{ SIGTHR, "+SIGTHR+" },
#endif
	{ -1, "" }
};

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
	if (code == SIGSEGV) {
		new_action.sa_flags |= SA_ONSTACK;
	}
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
#if defined(HAVE_FENV_H) & !defined(ECL_AVOID_FENV_H)
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
#ifdef SA_SIGINFO
		if (sbrk(0) < info->si_addr) {
			GC_disable();
			cl_error(3, @'ext::stack-overflow', @':type', @'ext::c-stack');
		}
#endif
		cl_error(1, @'ext::segmentation-violation');
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
#ifdef GBC_BOEHM
	int old_GC_enabled = GC_enabled();
#endif
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
		if (old_GC_enabled) GC_enable() else GC_disable();
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
si_catch_signal(cl_object code, cl_object boolean)
{
	int code_int = fixnnint(signal);
	int i;
#ifdef GBC_BOEHM
	if ((code_int == SIGSEGV && ecl_get_option(ECL_INCREMENTAL_GC)) ||
	    (code_int == SIGBUS)) {
		FEerror("It is not allowed to change the behavior of SIGBUS/SEGV.",
			0);
	}
#endif
#if defined(ECL_THREADS) && !defined(_MSC_VER) && !defined(mingw32)
	if (code_int == SIGUSR1) {
		FEerror("It is not allowed to change the behavior of SIGUSR1", 0);
	}
#endif
	for (i = 0; known_signals[i].code >= 0; i++) {
		if (known_signals[i].code == code) {
			mysignal(code, Null(boolean)? SIG_DFL : signal_catcher);
			@(return Ct)
		}
	}
	@(return Cnil)
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
#if (defined(HAVE_FENV_H) && defined(HAVE_FEENABLEEXCEPT) && !defined(ECL_AVOID_FENV_H)) || defined(_MSC_VER) || defined(mingw32)
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
init_unixint(int pass)
{
	if (pass == 0) {
		if (ecl_get_option(ECL_TRAP_SIGSEGV)) {
			mysignal(SIGSEGV, signal_catcher);
		}
#ifndef GBC_BOEHM
		if (ecl_get_option(ECL_TRAP_SIGBUS)) {
			mysignal(SIGBUS, signal_catcher);
		}
#endif
		if (ecl_get_option(ECL_TRAP_SIGINT)) {
			mysignal(SIGINT, signal_catcher);
		}
		if (ecl_get_option(ECL_TRAP_SIGFPE)) {
			mysignal(SIGFPE, signal_catcher);
			si_trap_fpe(Ct, Ct);
		}
#if defined(ECL_THREADS) && !defined(_MSC_VER) && !defined(mingw32)
		mysignal(SIGUSR1, signal_catcher);
#endif
#ifdef _MSC_VER
		SetUnhandledExceptionFilter(W32_exception_filter);
		SetConsoleCtrlHandler(W32_console_ctrl_handler, TRUE);
#endif
	} else {
		int i;
		for (i = 0; known_signals[i].code >= 0; i++) {
			cl_object name =
				_ecl_intern(known_signals[i].text,
					    cl_core.system_package);
			si_Xmake_constant(name, MAKE_FIXNUM(known_signals[i].code));
		}
	}
	ECL_SET(@'si::*interrupt-enable*', Ct);
	ecl_interrupt_enable = 1;
}
