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

#include <errno.h>
#include <string.h>
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
#ifdef ECL_USE_MPROTECT
# ifndef SA_SIGINFO
#  error "We cannot use the mmap code without siginfo"
# endif
# include <sys/mman.h>
#endif
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

#ifdef HAVE_SIGPROCMASK
#define define_handler(name, sig, info, aux) name(sig, info, aux)
#define call_handler(name, sig, info, aux) name(sig, info, aux)
#define reinstall_signal(x,y)
#define copy_siginfo(x,y) memcpy(x, y, sizeof(struct sigaction))
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
#define define_handler(name, sig, info, aux) name(sig)
#define call_handler(name, sig, info, aux) name(sig)
#define mysignal(x,y) signal(x,y)
#define reinstall_signal(x,y) signal(x,y)
#define copy_siginfo(x,y)
#endif

static bool
interrupts_disabled_by_C(cl_env_ptr the_env)
{
	return the_env->disable_interrupts;
}

static bool
interrupts_disabled_by_lisp(cl_env_ptr the_env)
{
	return (ecl_get_option(ECL_OPT_BOOTED) &&
		ecl_symbol_value(@'si::*interrupt-enable*') == Cnil);
}

static void
jump_to_sigsegv_handler(cl_env_ptr the_env)
{
	ecl_frame_ptr destination = frs_sch(OBJNULL);
	if (destination) {
		the_env->nvalues = 0;
		ecl_unwind(destination);
	}
	ecl_internal_error("SIGSEGV without handler to jump to.");
}

static void
define_handler(lisp_signal_handler, int sig, siginfo_t *info, void *aux)
{
	cl_env_ptr the_env = &cl_env;
	switch (sig) {
#if defined(ECL_THREADS) && !defined(_MSC_VER) && !defined(mingw32)
	case SIGUSR1:
		funcall(1, the_env->own_process->process.interrupt);
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
	case SIGSEGV: {
		ecl_frame_ptr destination = frs_sch(OBJNULL);
		if (destination) {
			the_env->nvalues = 0;
			ecl_unwind(destination);
		}
		ecl_internal_error("SIGSEGV without handler to jump to.");
	}
	case SIGBUS: {
		ecl_frame_ptr destination = frs_sch(OBJNULL);
		if (destination) {
			the_env->nvalues = 0;
			ecl_unwind(destination);
		}
		ecl_internal_error("SIGSEGV without handler to jump to.");
	}
	default:
		FEerror("Serious signal ~D caught.", 1, MAKE_FIXNUM(sig));
	}
}

#ifdef HAVE_SIGPROCMASK
static void
unblock_signal(int signal)
{
	struct sigaction oact;
	sigset_t block_mask;
	sigaction(signal, NULL, &oact);
	block_mask = oact.sa_mask;
	sigaddset(&block_mask, signal);
# ifdef ECL_THREADS
	pthread_sigmask(SIG_UNBLOCK, &block_mask, NULL);
# else
	sigprocmask(SIG_UNBLOCK, &block_mask, NULL);
# endif
}
#else
#define unblock_signal(sig)
#endif

static void
define_handler(handle_signal_now, int sig, siginfo_t *info, void *aux)
{
#if defined (_MSC_VER)
	if (sig == SIGFPE) {
		handle_fpe_signal(sig, _fpecode);
	}
#endif
	unblock_signal(sig);
	call_handler(lisp_signal_handler, sig, info, aux);
}

static void define_handler(sigsegv_handler, int sig, siginfo_t *info, void *aux);

static void
define_handler(non_evil_signal_handler, int sig, siginfo_t *siginfo, void *data)
{
	int old_errno = errno;
	cl_env_ptr the_env;
	if (!ecl_get_option(ECL_OPT_BOOTED)) {
		ecl_internal_error("Got signal before environment was installed"
				   " on our thread.");
	}
	the_env = ecl_process_env();
	reinstall_signal(sig, non_evil_signal_handler);
	printf("Non evil handler\n");
	/*
	 * If interrupts are disabled by C we are not so eager on
	 * detecting when the interrupts become enabled again. We
	 * queue the signal and are done with that.
	 */
	if (interrupts_disabled_by_lisp(the_env)) {
		if (!the_env->interrupt_pending) {
			the_env->interrupt_pending = sig;
			copy_siginfo(the_env->interrupt_info, siginfo);
		}
		errno = old_errno;
		return;
	}
	/*
	 * If interrupts are disabled by C, and we have not pushed a
	 * pending signal, save this signal and return. On platforms
	 * in which mprotect() works, we block all write access to the
	 * environment for a cheap check of pending interrupts. On other
	 * platforms we change the value of disable_interrupts to 3, so
	 * that we detect changes.
	 */
	if (interrupts_disabled_by_C(the_env)) {
		the_env->disable_interrupts = 3;
		if (!the_env->interrupt_pending) {
			struct sigaction oact;
			the_env->interrupt_pending = sig;
			copy_siginfo(the_env->interrupt_info, siginfo);
			printf("Postponing signal %d\n", sig);
			sigaction(SIGSEGV, NULL, &oact);
			printf("SIGSEGV Handler: %x\n", oact.sa_sigaction);
			sigaction(SIGBUS, NULL, &oact);
			printf("SIGBUS Handler: %x\n", oact.sa_sigaction);
			printf("sigsegv_handler: %x\n", sigsegv_handler);
#ifdef ECL_USE_MPROTECT
			printf("Protecting %x\n", the_env);
			if (mprotect(the_env, sizeof(*the_env), PROT_READ) < 0)
				ecl_internal_error("Unable to mprotect environment.");
#endif
		}
		errno = old_errno;
		return;
	}
	/*
	 * If interrupts are enabled, that means we are in a safe area
	 * and may execute arbitrary lisp code. We can thus call the
	 * appropriate handlers.
	 */
	errno = old_errno;
	call_handler(handle_signal_now, sig, siginfo, data);
}

static void
define_handler(sigsegv_handler, int sig, siginfo_t *info, void *aux)
{
	cl_env_ptr the_env = ecl_process_env();
	if (!ecl_get_option(ECL_OPT_BOOTED)) {
		ecl_internal_error("Got signal before environment was installed"
				   " on our thread.");
	}
	the_env = ecl_process_env();
#ifdef HAVE_SIGPROCMASK
# ifdef ECL_DOWN_STACK
	if ((cl_fixnum*)info->si_addr > the_env->cs_barrier &&
	    (cl_fixnum*)info->si_addr <= the_env->cs_org) {
		return jump_to_sigsegv_handler(the_env);
	}
# else
	if ((cl_fixnum*)info->si_addr < the_env->cs_barrier &&
	    (cl_fixnum*)info->si_addr >= the_env->cs_org) {
		return jump_to_sigsegv_handler(the_env);
	}
# endif
	if (interrupts_disabled_by_lisp(the_env)) {
		if (!the_env->interrupt_pending) {
			the_env->interrupt_pending = sig;
			copy_siginfo(the_env->interrupt_info, info);
		}
		return;
	}
	if (interrupts_disabled_by_C(the_env)) {
		if (!the_env->interrupt_pending) {
			the_env->interrupt_pending = sig;
			copy_siginfo(the_env->interrupt_info, info);
# ifdef ECL_USE_MPROTECT
			printf("Protecting %p\n", the_env);
			if (mprotect(the_env, sizeof(*the_env), PROT_READ) < 0)
				ecl_internal_error("Unable to mprotect environment.");
# endif
		}
		return;
	}
	handle_signal_now(sig, info, aux);
#else
	reinstall_signal_handler(sig, sigsegv_signal_handler);
	/*
	 * We cannot distinguish between a stack overflow and a simple
	 * access violation. Thus we assume the worst case and jump to
	 * the outermost handler.
	 */
	jump_to_sigsegv_handler(&cl_env);
#endif
}

static void
define_handler(sigbus_handler, int sig, siginfo_t *info, void *aux)
{
	cl_env_ptr the_env = &cl_env;
	printf("Entering sigbus_handler for address %0p\n", info->si_addr);
#if defined(SA_SIGINFO) && defined(ECL_USE_MPROTECT)
	/* We access the environment when it was protected. That
	 * means there was a pending signal. */
	if (the_env == info->si_addr) {
		int signal = the_env->interrupt_pending;
		siginfo_t info = *(siginfo_t*)(the_env->interrupt_info);
		printf("Unprotecting %p\n", the_env);
		mprotect(the_env, sizeof(*the_env), PROT_READ | PROT_WRITE);
		the_env->interrupt_pending = 0;
		the_env->disable_interrupts = 0;
		unblock_signal(sig);
		return handle_signal_now(signal, &info, aux);
	}
#endif
	call_handler(handle_signal_now, sig, info, aux);
}

cl_object
si_check_pending_interrupts(void)
{
	ecl_check_pending_interrupts();
	@(return)
}

void
ecl_check_pending_interrupts(void)
{
	int sig;
	void *info;
	cl_env.disable_interrupts = 0;
	info = cl_env.interrupt_info;
	sig = cl_env.interrupt_pending;
	if (sig) {
		call_handler(handle_signal_now, sig, info, 0);
	}
}

cl_object
si_catch_signal(cl_object code, cl_object boolean)
{
	int code_int = fixnnint(code);
	int i;
#ifdef GBC_BOEHM
	int error = 0;
#ifdef SIGSEGV
	if ((code_int == SIGSEGV) && ecl_get_option(ECL_OPT_INCREMENTAL_GC))
		FEerror("It is not allowed to change the behavior of SIGSEGV.",
			0);
#endif
#ifdef SIGBUS
	if (code_int == SIGBUS)
		FEerror("It is not allowed to change the behavior of SIGBUS.",
			0);
#endif
#endif
#if defined(ECL_THREADS) && !defined(_MSC_VER) && !defined(mingw32)
	if (code_int == SIGUSR1) {
		FEerror("It is not allowed to change the behavior of SIGUSR1", 0);
	}
#endif
	for (i = 0; known_signals[i].code >= 0; i++) {
		if (known_signals[i].code == code_int) {
			if (Null(boolean))
				mysignal(code_int, SIG_DFL);
			else if (code_int == SIGSEGV)
				mysignal(code_int, sigsegv_handler);
			else if (code_int == SIGBUS)
				mysignal(code_int, sigbus_handler);
			else
				mysignal(code_int, non_evil_signal_handler);
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
			non_evil_signal_handler(SIGFPE);
			break;
		/* Catch segmentation fault */
		case EXCEPTION_ACCESS_VIOLATION:
			sigsegv_handler(SIGSEGV);
			break;
		/* Catch illegal instruction */
		case EXCEPTION_ILLEGAL_INSTRUCTION:
			non_evil_signal_handler(SIGILL);
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
			non_evil_signal_handler(SIGINT);
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
#ifdef SIGSEGV
		if (ecl_get_option(ECL_OPT_TRAP_SIGSEGV)) {
			mysignal(SIGSEGV, sigsegv_handler);
		}
#endif
#if defined(SIGBUS) /*&& !defined(GBC_BOEHM)*/
		if (ecl_get_option(ECL_OPT_TRAP_SIGBUS)) {
			mysignal(SIGBUS, sigbus_handler);
		}
#endif
#ifdef SIGINT
		if (ecl_get_option(ECL_OPT_TRAP_SIGINT)) {
			mysignal(SIGINT, non_evil_signal_handler);
		}
#endif
#if defined(ECL_THREADS) && !defined(_MSC_VER) && !defined(mingw32)
		mysignal(SIGUSR1, non_evil_signal_handler);
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
		ECL_SET(@'si::*interrupt-enable*', Ct);
#ifdef SIGFPE
		if (ecl_get_option(ECL_OPT_TRAP_SIGFPE)) {
			mysignal(SIGFPE, non_evil_signal_handler);
			si_trap_fpe(Ct, Ct);
		}
#endif
		cl_env.disable_interrupts = 0;
	}
}
