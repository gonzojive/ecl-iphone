/*
    unixint.c -- Unix interrupt interface.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECLS is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#include "ecls.h"
#include <signal.h>

/******************************* EXPORTS ******************************/

int interrupt_enable;		/* console interupt enable */
int interrupt_flag;		/* console interupt flag */

/******************************* ------- ******************************/

static cl_object SVinterrupt_enable;

#ifndef THREADS

void
sigalrm(void)
{
	if (interrupt_flag) {
		interrupt_flag = FALSE;
		terminal_interrupt(TRUE);
	}
}

void
sigint(void)
{
  if (!interrupt_enable || interrupt_flag) {
    if (!interrupt_enable) {
      fprintf(stdout, "\n;;;Interrupt delayed.\n"); fflush(stdout);
      interrupt_flag = TRUE;
    }
    signal(SIGINT, sigint);
    return;
  }
  if (symbol_value(SVinterrupt_enable) == Cnil) {
    SYM_VAL(SVinterrupt_enable) = Ct;
    signal(SIGINT, sigint);
    return;
  }
#ifdef __GO32__
  if (interrupt_flag)
    sigalrm();
#endif
  interrupt_flag = TRUE;
  signal(SIGALRM, sigalrm);
  alarm(1);
  signal(SIGINT, sigint);
}

#else /* THREADS */

extern int critical_level;
bool scheduler_interrupted = FALSE;
int scheduler_interruption = 0;

void
sigint()
{
#ifdef SYSV
  signal(SIGINT, sigint);
#endif
  if (critical_level > 0) {
    scheduler_interrupted = TRUE;
    scheduler_interruption = ERROR_INT;
    return;
  }

  if (symbol_value(SVinterrupt_enable) == Cnil) {
    SVinterrupt_enable->symbol.dbind = Ct;
    return;
  }

  terminal_interrupt(TRUE);
}

#endif /*THREADS */

void
sigfpe(void)
{
	signal(SIGFPE, sigfpe);
	FEerror("Floating-point exception.", 0);
}

#ifdef unix
void
signal_catcher(int sig, int code, int scp)
{
	char str[64];

	if (!interrupt_enable) {
		sprintf(str, "signal %d caught (during GC)", sig);
		error(str);
	}
	else if (sig == SIGSEGV)
	  FEerror("Segmentation violation.~%\
Wrong type argument to a compiled function.", 0);
	else {
	  printf("System error. Trying to recover ...\n");
	  fflush(stdout);
	  FEerror("Signal ~D caught.~%\
The internal memory may be broken.~%\
You should check the signal and exit from Lisp.", 1,
		  MAKE_FIXNUM(sig));
	}
}

@(defun si::catch_bad_signals ()
@
	signal(SIGILL, signal_catcher);
	signal(SIGBUS, signal_catcher);
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
@)

@(defun si::uncatch_bad_signals ()
@
	signal(SIGILL, SIG_DFL);
	signal(SIGBUS, SIG_DFL);
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
@)
#endif unix

void
enable_interrupt(void)
{
	interrupt_enable = TRUE;
	signal(SIGFPE, sigfpe);
	signal(SIGINT, sigint);
#ifdef __EMX__
	signal(SIGBREAK, sigint);
#endif
}

void
init_interrupt(void)
{
	SVinterrupt_enable = make_si_special("*INTERRUPT-ENABLE*", Ct);
}
