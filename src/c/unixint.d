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

/******************************* EXPORTS ******************************/

int interrupt_enable;		/* console interupt enable */
int interrupt_flag;		/* console interupt flag */

/******************************* ------- ******************************/

typedef void (*signalfn)(int);

#ifndef THREADS

#ifdef SIGALRM
static void
sigalrm(void)
{
	if (interrupt_flag) {
		interrupt_flag = FALSE;
		terminal_interrupt(TRUE);
	}
}
#endif

void
sigint(void)
{
  if (!interrupt_enable || interrupt_flag) {
    if (!interrupt_enable) {
      fprintf(stdout, "\n;;;Interrupt delayed.\n"); fflush(stdout);
      interrupt_flag = TRUE;
    }
    signal(SIGINT, (signalfn)sigint);
    return;
  }
  if (symbol_value(@'si::*interrupt-enable*') == Cnil) {
    SYM_VAL(@'si::*interrupt-enable*') = Ct;
    signal(SIGINT, (signalfn)sigint);
    return;
  }
#ifdef SIGALRM
#ifdef __GO32__
  if (interrupt_flag)
    sigalrm();
#endif
  interrupt_flag = TRUE;
  signal(SIGALRM, (signalfn)sigalrm);
  alarm(1);
#endif
  signal(SIGINT, (signalfn)sigint);
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

  if (symbol_value(@'si::*interrupt-enable*') == Cnil) {
    SYM_VAL(@'si::*interrupt-enable*') = Ct;
    return;
  }

  terminal_interrupt(TRUE);
}

#endif /*THREADS */

static void
sigfpe(void)
{
	signal(SIGFPE, (signalfn)sigfpe);
	FEerror("Floating-point exception.", 0);
}

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

cl_object
si_catch_bad_signals()
{
	signal(SIGILL, (signalfn)signal_catcher);
#ifndef GBC_BOEHM
	signal(SIGBUS, (signalfn)signal_catcher);
#endif
	signal(SIGSEGV, (signalfn)signal_catcher);
#ifdef SIGIOT
	signal(SIGIOT, (signalfn)signal_catcher);
#endif
#ifdef SIGEMT
	signal(SIGEMT, (signalfn)signal_catcher);
#endif
#ifdef SIGSYS
	signal(SIGSYS, (signalfn)signal_catcher);
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
enable_interrupt(void)
{
	interrupt_enable = TRUE;
	signal(SIGFPE, (signalfn)sigfpe);
	signal(SIGINT, (signalfn)sigint);
#ifdef __EMX__
	signal(SIGBREAK, (signalfn)sigint);
#endif
}
