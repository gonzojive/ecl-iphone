/*
    lwp.d -- Light weight processes.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include "ecl.h"

/******************************* EXPORTS ******************************/

lpd main_lpd;
lpd *clwp = &main_lpd;
int critical_level = 0;
pd *running_head;		/* front of running pd's  */
pd *running_tail;		/* back of running pd's   */
pd main_pd;

/******************************* IMPORTS ******************************/

extern scheduler_interruption; /* in unixint.c */
extern int writec_PRINTstream();
extern cl_object readc();
extern gc();
extern cl_type garbage_parameter;

/******************************* ------- ******************************/

#define thread_switch()  { setTimer(0); enable_scheduler(); \
			     scheduler(0, 0, NULL); }

static bool timer_active = FALSE;
static bool scheduler_disabled = FALSE;
static int scheduler_level = 0;            /* tito */
static bool reset_timer = FALSE;
static int running_processes = 1;
static int absolute_time = 0;

static cl_object main_thread;

static
setTimer(long time)
{
  struct itimerval oldtimer;
  struct itimerval itimer;
  itimer.it_value.tv_sec = 0;
  itimer.it_value.tv_usec = time;
  itimer.it_interval.tv_sec = 0;
  itimer.it_interval.tv_usec = 0;
  setitimer(ITIMER_REAL, &itimer, &oldtimer);
}

pd *
dequeue()
{
  pd *tmp;
  tmp = running_head;
  if  (running_head != NULL)
    running_head = running_head->pd_next;
  return tmp;
}

pd *
make_pd()
{
  pd *new_pd; lpd *npd;
  int i;
  
  /* Allocate a new descriptor for the new lwp */
  new_pd = (pd *)malloc(sizeof(pd));
  
  /* create a new stack ... */
  new_pd->pd_base = (int *)malloc(STACK_SIZE * sizeof(int));
  new_pd->pd_status = SUSPENDED;

  /* allocate a lisp descriptor:
   * using the calloc here it's possible to avoid the
   * critical section in the various push operations
   */
  npd = new_pd->pd_lpd = (lpd *)calloc(sizeof(lpd), 1);

  /* initialize it */

  /* bind stack */
  npd->lwp_bdssize = BDSSIZE + 2*BDSGETA;
  npd->lwp_bdsorg = malloc(npd->lwp_bdssize * sizeof(*npd->lwp_bdsorg));
  npd->lwp_bdstop = npd->lwp_bdsorg-1;
  npd->lwp_bdslimit = &npd->lwp_bdsorg[npd->lwp_bdssize - 2*BDSGETA];

  /* C stack */
  /* cssize is different now for the main thread only, but you might
     want to create threads with different stack sizes             */

#ifdef DOWN_STACK
  npd->lwp_cs_org = new_pd->pd_base + STACK_SIZE - 1;
  npd->lwp_cs_limit = new_pd->pd_base - 1;
#else
  npd->lwp_cs_org = new_pd->pd_base;
  npd->lwp_cs_limit = npd->lwp_cs_org + STACK_SIZE;
#endif   
  /* invocation history stack */
  npd->lwp_ihstop = 0
  /* frame stack */
  npd->lwp_frs_size = FRSSIZE + 2*FRSGETA;
  npd->lwp_frs_org = malloc(npd->lwp_frs_size * sizeof(*npd->lwp_frs_org));
  npd->lwp_frs_top = npd->lwp_frs_org-1;
  npd->lwp_frs_limit = &npd->lwp_frs_org[npd->lwp_frs_size - 2*FRSGETA];
  
  npd->lwp_alloc_temporary = OBJNULL;
  npd->lwp_eval1 = 0;
  /* for gc */
  npd->lwp_fmt_temporary_stream = OBJNULL;
  npd->lwp_fmt_temporary_string = OBJNULL;
  
  npd->lwp_PRINTstream = Cnil;
  npd->lwp_PRINTescape = TRUE;
  npd->lwp_PRINTpretty = FALSE;
  npd->lwp_PRINTcircle = FALSE;
  npd->lwp_PRINTbase = 10;
  npd->lwp_PRINTradix = FALSE;
  npd->lwp_PRINTcase = @':downcase';
  npd->lwp_PRINTgensym = TRUE;
  npd->lwp_PRINTlevel = -1;
  npd->lwp_PRINTlength = -1;
  npd->lwp_PRINTarray = FALSE;
  
  npd->lwp_delimiting_char = OBJNULL;
  npd->lwp_detect_eos_flag = FALSE;
  npd->lwp_in_list_flag = FALSE;
  npd->lwp_dot_flag = FALSE;
  npd->lwp_sharp_eq_context_max = 0;
  
  /* for gc */
  npd->lwp_token = OBJNULL;
    
  /* lex_env copy */
  npd->lwp_lex_env = npd->lwp_lex;
  
  /* ihs_push(Cnil) */
  (++npd->lwp_ihs_top)->ihs_function = Cnil;
  npd->lwp_ihs_top->ihs_base = npd->lwp_lex_env;

  /* Now the allocation. If the gc is invoked we are able to mark
   * the objects already allocated
   */
  npd->lwp_fmt_temporary_stream = make_string_output_stream(64);
  npd->lwp_fmt_temporary_string =
    npd->lwp_fmt_temporary_stream->stream.object0;
  
  npd->lwp_token = cl_alloc_adjustable_string(LISP_PAGESIZE);

  for (i=0; i<3; i++)
    npd->lwp_bignum_register[i] = cl_alloc_object(t_bignum);
  
  return new_pd;
}

update_queue()
{
  register pd *dead_pd;
  pd *last = running_tail;

  do
    switch (running_head->pd_status) {

    case DEAD:

      /* remove the dead process */
      dead_pd = dequeue();
      /* free the lisp descriptor */
      free(dead_pd->pd_lpd);
      /* free the memory allocated for the stack and the descriptor */
      free(dead_pd->pd_base);
      free(dead_pd);
      break;

/*    case SUSPENDED: */
    case DELAYED:

      if (running_head->pd_slice != 0) 
	if (absolute_time > running_head->pd_slice) {

	  /* the time slice has expired */
	  running_head->pd_slice = 0;

	  if ((running_head->pd_thread->thread.cont) != OBJNULL) {
	    /* in this case a continuation was created before %delay */
	    running_head->pd_thread->thread.cont->cn.cn_timed_out = TRUE;
	    running_head->pd_thread->thread.cont = OBJNULL;
	  }
	  running_head->pd_status = RUNNING;
	  return;		/* now you are a running process */
	}
      ROTQUEUE();
      break;
	  
    case WAITING:		/* waiting processes need to be scheduled  */
    case RUNNING:
      return;			/* found schedulable process */
	
    default:			/* currently is only STOPPED */
      ROTQUEUE();
      break;
    }
  while (running_head != last);
}

activate_thread(cl_object thread)
{
  pd *npd = thread->thread.data;

  /* jump on the new C stack */
  if (sigsetjmp(npd->pd_env, 1) == 0) {
#define STACK_MARGIN 160	/* longjmp writes also under the sp level */
#ifdef DOWN_STACK
# ifdef __linux
    npd->pd_env[0].__jmpbuf[0].__sp =
      stack_align((int)(npd->pd_base) + sizeof(int)*STACK_SIZE - STACK_MARGIN);
    npd->pd_lpd->lwp_cssize =
      npd->pd_env[0].__jmpbuf[0].__sp - (int)npd->pd_base;
# else
    npd->pd_env[JB_SP] =
      stack_align((int)(npd->pd_base) + sizeof(int)*STACK_SIZE - STACK_MARGIN);
    npd->pd_lpd->lwp_cssize =
      npd->pd_env[JB_SP] - (int)npd->pd_base;
# endif
#else
    npd->pd_env[JB_SP] =
      stack_align((int)(npd->pd_base));
    npd->pd_lpd->lwp_cssize = sizeof(int) * STACK_SIZE - STACK_MARGIN;
#endif /* DOWN_STACK */
#ifdef JB_FP
    npd->pd_env[JB_FP] = npd->pd_env[JB_SP];
#endif
    return;
  }

  /* Back here when thread is first resumed.
   *
   * WARNING: args and locals are no more accessible from here on,
   * since we reenter with longjmp onto a new stack.
   */
  
#ifndef sun4sol2
  /* on SunOS sigmask is 0x82001 here */
  sigsetmask(sigblock(0) & ~(sigmask(SIGALRM)));
#endif

  { int i;
    for (i = clwp->lwp_nValues; i > 0;)
      VALUES(i) = VALUES(--i);
    VALUES(0) = clwp->lwp_thread->thread.entry;
    apply(clwp->lwp_nValues+1, @'si::thread-top-level', &VALUES(0));
  }
  /* Termination */
  
  terpri(Cnil);
  running_head->pd_status = DEAD;
  running_head->pd_thread->thread.data = NULL;
  running_processes--;

  update_queue();
  thread_next();		/* update_queue has freed our stack!!! */
}

/*
 * switch to the first thread on queue
 */
thread_next()
{
  /* unwind the bind stack */
  lwp_bds_unwind(clwp->lwp_bind_stack, clwp->lwp_bds_top);

  /* switch clwp */
  clwp = running_head->pd_lpd;

  /* restore Values pointer */
  Values = clwp->lwp_Values;

  /* wind the bind stack */
  lwp_bds_wind(clwp->lwp_bind_stack, clwp->lwp_bds_top);

  /* reset the timer */
  if (running_processes > 1) {
    timer_active = TRUE;
    setTimer(REALQUANTUM); 
  } else { 
    timer_active = FALSE;
    absolute_time = 0;
  }
  siglongjmp(running_head->pd_env, 1); 
}

/*
 * Called when time slice expires or explicitily to switch thread
 */
scheduler(int sig, int code, struct sigcontext *scp)
{
  int val;

#if defined(SYSV) || defined(__svr4__) || defined(__linux)
  signal(SIGALRM, scheduler);
#endif

  absolute_time++;
  if (critical_level > 0) {	/* within critical section */
    scheduler_interrupted = TRUE;
    scheduler_interruption = SCHEDULER_INT;
    return;
  }
  if (scheduler_level > 0) {	/* abilitation  check */
    scheduler_interrupted = TRUE;
    return;
  }

  val = sigsetjmp(running_head->pd_env, 1);

  if (val == 1)			/* resume interrupted thread execution */
    return;			/* coming back from longjmp in thread_next */

  if (val == 2)			/* coming back from longjmp in GC */
    gc(garbage_parameter);	/* GC will return to the previous thread */

  ROTQUEUE();
  thread_next();
}

/*
 * Handle signal received within critical section
 */
interruption_handler()
{
  scheduler_interrupted = FALSE;

  switch (scheduler_interruption) {

  case SCHEDULER_INT:
    thread_switch();
    break;

  case ERROR_INT:
    sigint();
    break;
  }
}

lwp_bds_wind(bds_ptr base, bds_ptr top)
{
  cl_object temp;

  for (; top >= base; base++) {
    temp = SYM_VAL(base->bds_sym);
    SYM_VAL(base->bds_sym) = base->bds_val;
    base->bds_val = temp;
  }
}

lwp_bds_unwind(bds_ptr base, bds_ptr top)
{
  cl_object temp;

  for (; top >= base; top--) {
    temp = SYM_VAL(top->bds_sym);
    SYM_VAL(top->bds_sym) = top->bds_val;
    top->bds_val = temp;
  }
}

resume(pd *rpd)
{
  register pd *p;

  start_critical_section();
  running_processes++;

  rpd->pd_status = RUNNING;
  for (p = running_head; (p != rpd) && (p != NULL); p = p->pd_next) ;
  if (p == NULL)
    ENQUEUE(rpd);
  end_critical_section();

  if (!timer_active)  {
    timer_active = TRUE;
    setTimer(REALQUANTUM);
  }
}

/***********
 *
 * THREADS
 *
 ***********/


@(defun si::thread_break_in ()
@
	alarm(0);
	@(return Cnil)
@)

@(defun si::thread_break_quit ()
  /* reset everything in MT */
  pd *p;
@
  /* this is done in any case to remedy the problem with C-c handling */
  signal(SIGALRM, scheduler);

  if (timer_active) {
     /* reset the critical and disable-scheduler environment    */
      scheduler_disabled = FALSE;
      scheduler_level = 0;
      critical_level = 0;
      scheduler_interrupted = 0;

      for (p = running_head; (p != NULL); p = p->pd_next)
	if (p != &main_pd)
	  p->pd_status = DEAD;
	else {
	  p->pd_status = RUNNING;
	  p->pd_thread->thread.cont = OBJNULL;
	}

      if (running_head != &main_pd) {
	update_queue();
	thread_next(); 
	/*  here one should deallocate the main-thread function     */
      }
      else
	thread_switch();
    }
  @(return Cnil)
@)

@(defun si::thread_break_resume ()
@
  /* Restart the timer that might have been
   * changed by the interrupt handling
   */
  signal(SIGALRM, scheduler);  
  if (timer_active)
    thread_switch();
  @(return Cnil)
@)

@(defun thread_list ()
  pd *p;
  cl_object tmp, x = CONS(running_head->pd_thread, Cnil);
@
  tmp = x;
  start_critical_section();

  for (p = running_head->pd_next; (p != NULL); p = p->pd_next) {
      CDR(tmp) = CONS(p->pd_thread, Cnil);
      tmp = CDR(tmp);
    }

  end_critical_section();

  @(return x)
@)

@(defun make_thread (fun)
  cl_object x;
  pd *npd;
@
  /* Just one argument for the time being */

  if (SYMBOLP(fun)) {
    if (fun->symbol.isform || fun->symbol.mflag)
      FEinvalid_function(fun);
    if (SYM_FUN(fun) == OBJNULL)
      FEundefined_function(fun);
    /*  fun = SYM_FUN(fun); confusing */
  }

  x = cl_alloc_object(t_thread);
  x->thread.entry = fun;
  x->thread.size = sizeof(pd);
  x->thread.data = npd = make_pd();
  x->thread.cont = OBJNULL;

  npd->pd_thread = x;
  npd->pd_slice = 0;

  /* Backpointer to thread */
  npd->pd_lpd->lwp_thread = x;

  activate_thread(x);

  @(return x)
@)

@(defun deactivate (thread)
@
  if (type_of(thread) != t_thread)
    FEwrong_type_argument(@'thread', thread);

  if (thread->thread.data == NULL ||
      thread->thread.data->pd_status != RUNNING)
    FEerror("Cannot deactivate a thread not running", 0);

  start_critical_section();	/* tito */
  thread->thread.data->pd_status = STOPPED;
  running_processes--;
  if (thread->thread.data == running_head) {
    critical_level--; /* end_critical_section() */
    update_queue();
    thread_next();
  } else
    end_critical_section();
  @(return Cnil)
@)

@(defun reactivate (thread)
@
  start_critical_section();

  if (type_of(thread) != t_thread) {
    FEwrong_type_argument(@'thread', thread);
  }

  if (thread->thread.data == NULL ||
      thread->thread.data->pd_status != STOPPED)
    FEerror("Cannot reactivate a thread not stopped", 0);

  start_critical_section();	/* tito */
  thread->thread.data->pd_status = RUNNING;
  running_processes++;

  if (!timer_active) {
     timer_active = TRUE;
     setTimer(REALQUANTUM);
   }

  end_critical_section();
  @(return Cnil)
@)

@(defun kill_thread (thread)
@
  /* The following code is not enough.
     Consider: The scheduler can be disabled
     What about killing the current thread?
     */
  if (type_of(thread) != t_thread)
    FEwrong_type_argument(@'thread', thread);

  if (thread->thread.data != NULL) {
    start_critical_section();
    thread->thread.data->pd_status = DEAD;
    if (thread->thread.data->pd_lpd == clwp) {
       /* if a thread kills itself the scheduler is to be called */
       thread->thread.data = NULL;
       critical_level--; /*  end_critical_section() */
       update_queue();
       thread_next();
     }
     else {
       thread->thread.data = NULL;
       end_critical_section();
     }
  }
  @(return)
@)

@(defun current_thread ()
@
  @(return clwp->lwp_thread)
@)

@(defun thread_status (thread)
  cl_object output;
@
  if (type_of(thread) != t_thread)
    FEwrong_type_argument(@'thread', thread);

  if (thread->thread.data != NULL)
    switch (thread->thread.data->pd_status) {
    case RUNNING:
      output = @'running';
      break;
    case SUSPENDED:
      output = @'suspended';
      break;
    case WAITING:
      output = @'waiting';
      break;
    case STOPPED:
      output = @'stopped';
      break;
    case DEAD:
      output = @'dead';
      break;
    default:
      FEerror("Unexpected type for thread ~A", 1, thread);
    }
  else
    output = @'dead';
  @(return output)
@)


/***************
 *
 * CONTINUATIONS
 *
 ***************/

@(defun make_continuation (thread)
  cl_object x;
@
  if (type_of(thread) != t_thread)
    FEwrong_type_argument(@'thread', thread);

  if (thread->thread.cont)
    FEerror("A continuation for thread ~A already exists.", 1, thread);

  if (thread->thread.data == NULL ||
      thread->thread.data->pd_status == DEAD) {
    FEerror("Thread ~A is DEAD.", 1, thread);
  }

  x = cl_alloc_object(t_cont);

  x->cn.cn_thread = thread;
  x->cn.cn_resumed = FALSE;
  x->cn.cn_timed_out = FALSE;

  thread->thread.cont = x;
  @(return x)
@)

/* Returns the thread associated to a continuation */
@(defun thread_of (cont)
@
  if (type_of(cont) != t_cont)
    FEwrong_type_argument(@'cont', cont);
  @(return cont->cn.cn_thread)
@)

/* Returns the continuation associated to a thread, if it exists */
@(defun continuation_of (thread)
@
  if (type_of(thread) != t_thread)
    FEwrong_type_argument(@'thread', thread);
  @(return (thread->thread.cont? thread->thread.cont : Cnil))
@)

@(defun resume (cont &rest args)
  int i;
  cl_object *thread_Values;
@
  if (Null(cont))
    @(return Cnil)

  if (type_of(cont) != t_cont)
    FEwrong_type_argument(@'cont', cont);

  if (cont->cn.cn_resumed)
    FEerror("The continuation has already been resumed.", 0);
  
  if (cont->cn.cn_timed_out)
    FEerror("The continuation has been timed out.", 0);

  if (cont->cn.cn_thread->thread.data == NULL)
    FEerror("The continuation cannot be resumed. Its thread is DEAD.", 0);

  if (cont->cn.cn_thread->thread.data->pd_status != SUSPENDED &&
      cont->cn.cn_thread->thread.data->pd_status != DELAYED)
    FEerror("The continuation cannot be resumed. Its thread isn't suspended", 0);

  /* Push the arguments on the value stack of thread */

  thread_Values = cont->cn.cn_thread->thread.data->pd_lpd->lwp_Values;

  for (i = 1; i < narg; i++)
    *(thread_Values++) = va_arg(args, cl_object);
  cont->cn.cn_thread->thread.data->pd_lpd->lwp_nValues = narg-1;

  cont->cn.cn_resumed = TRUE;
  cont->cn.cn_thread->thread.cont = OBJNULL;

  /* If you are waiting on a slice expiring I reset your slice    */
  cont->cn.cn_thread->thread.data->pd_slice = 0;

  resume(cont->cn.cn_thread->thread.data);

  @(return cont->cn.cn_thread)
@)


/***************
 *
 * SCHEDULING
 *
 ***************/

@(defun disable_scheduler ()
@
  scheduler_level++;
  @(return Cnil)
@)

@(defun enable_scheduler ()
@
  enable_scheduler();
  @(return Cnil)
@)

enable_scheduler()
{
   scheduler_level = 0;

   if (scheduler_interrupted) {
     scheduler_interrupted = FALSE;
     thread_switch();
   }
}

@(defun suspend ()
@
  if (timer_active) {
       running_head->pd_status = SUSPENDED;
       running_processes--;
       thread_switch();
       /* When resumed it will be provided with the Values to return */
#error "This is very very wrong!"
       RETURN(running_head->pd_lpd->lwp_nValues);
     }
  else 
    FEerror("No other active thread.", 0);
@)

@(defun delay (interval)
  int z;
@
  z = fixnnint(interval);
  
  if (timer_active) {
    running_head->pd_status = DELAYED;
    running_processes--;
       
    /* Translate seconds in number of scheduler slices    */
    running_head->pd_slice = z * 10 + absolute_time;

    thread_switch();
       
    /* When resumed it will be provided with the Values to return */
#error "This is very very wrong!"
    RETURN(running_head->pd_lpd->lwp_nValues);
  }
  else
    sleep(z);
@)

@(defun thread_wait (fun &rest args)
@
  start_critical_section();
  running_head->pd_status = WAITING;
  running_processes--;
  end_critical_section();

  for (;;) {
    if (apply(narg-1, fun, &va_arg(args, cl_object)) != Cnil)
      break;
    else if (timer_active) {
      /* the time slice has not been used */
      absolute_time--;
      thread_switch();
    } else
      FEerror("The condition will never be satisfied for lack of active processes", 0);
  }
  running_head->pd_status = RUNNING;
  running_processes++;
  end_critical_section();
  returnn();
@)
  
@(defun thread_wait_with_timeout (timeout fun &rest args)
@
  /* We have to translate seconds in scheduler call number */
  start_critical_section();
  running_head->pd_slice = fixnnint(timeout) * 10 + absolute_time;
  
  running_head->pd_status = WAITING;
  running_processes--;
  end_critical_section();
  
  for (;;) {
    
    if (absolute_time > running_head->pd_slice) {
      /* the time slice has expired */
      VALUES(0) = Cnil;
      NValues = 1;
      break;
    }

    if (apply(narg-1, fun, va_arg(&args, cl_object)) != Cnil)
      break;
    else {
      /* the time slice has not been used */
      absolute_time--;
      thread_switch();
    }
  }
  
  start_critical_section();
  running_head->pd_slice = 0;
  running_head->pd_status = RUNNING;
  running_processes++;
  end_critical_section();
  returnn();
@)

enable_lwp()
{
  signal(SIGALRM, scheduler);
}

init_lwp()
{ pd *temp_pd;

  temp_pd = &main_pd;
  PUSH(temp_pd);

  main_thread = cl_alloc_object(t_thread);
  main_pd.pd_thread = main_thread;
  main_thread->thread.entry = Cnil;
  main_thread->thread.size = sizeof (pd);
  main_thread->thread.data = &main_pd;
  main_thread->thread.cont = OBJNULL;
  /* Backpointer to thread */
  main_pd.pd_status = RUNNING;
  main_pd.pd_lpd = &main_lpd;
  main_lpd.lwp_thread = main_thread;
  register_root(&main_thread);
}
