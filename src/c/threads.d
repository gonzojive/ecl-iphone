/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    threads.d -- Posix threads with support from GCC.
*/
/*
    Copyright (c) 2003, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/
/*
 * IMPORTANT!!!! IF YOU EDIT THIS FILE, CHANGE ALSO threads_win32.d
 */

#include <pthread.h>
#include <errno.h>
#include <math.h>
#include <time.h>
#include <signal.h>
#define GC_THREADS
#include <ecl/ecl.h>
#include <ecl/internal.h>
#ifdef HAVE_GETTIMEOFDAY
# include <sys/time.h>
#endif
#ifdef HAVE_SCHED_YIELD
# include <sched.h>
#endif

#if defined(_MSVC) || defined(mingw32)
#define ECL_WINDOWS_THREADS
/*
 * We have to put this explicit definition here because Boehm GC
 * is designed to produce a DLL and we rather want a static
 * reference
 */
#include <windows.h>
#include <gc.h>
extern HANDLE WINAPI GC_CreateThread(
    LPSECURITY_ATTRIBUTES lpThreadAttributes, 
    DWORD dwStackSize, LPTHREAD_START_ROUTINE lpStartAddress, 
    LPVOID lpParameter, DWORD dwCreationFlags, LPDWORD lpThreadId );
#ifndef WITH___THREAD
DWORD cl_env_key;
#endif
static DWORD main_thread;
#else
#ifndef WITH___THREAD
static pthread_key_t cl_env_key;
#endif
static pthread_t main_thread;
#endif

extern void ecl_init_env(struct cl_env_struct *env);

#if !defined(WITH___THREAD)
cl_env_ptr
ecl_process_env(void)
{
#ifdef ECL_WINDOWS_THREADS
	return TlsGetValue(cl_env_key);
#else
	struct cl_env_struct *rv = pthread_getspecific(cl_env_key);
        if (rv)
		return rv;
	printf("Getting pthread key %i returned %i\n", (int)cl_env_key, (int)rv);
	FElibc_error("pthread_getspecific() failed.", 0);
	return NULL;
#endif
}
#endif

static void
ecl_set_process_env(cl_env_ptr env)
{
#ifdef WITH___THREAD
	cl_env_p = env;
#else
# ifdef ECL_WINDOWS_THREADS
	TlsSetValue(cl_env_key, env);
# else
	printf("Setting pthread key %i to %i\n", (int)cl_env_key, (int)env);
	if (pthread_setspecific(cl_env_key, env))
		FElibc_error("pthread_setcspecific() failed.", 0);

	struct cl_env_struct *rv = pthread_getspecific(cl_env_key);
        if (rv != env || 1)
	{
		printf("Getting pthread key %i returned %i immediately afterwards. should match.\n", (int)cl_env_key, (int)rv);
	}


# endif
#endif
}

cl_object
mp_current_process(void)
{
	return ecl_process_env()->own_process;
}

/*----------------------------------------------------------------------
 * THREAD OBJECT
 */

static void
assert_type_process(cl_object o)
{
	if (type_of(o) != t_process)
		FEwrong_type_argument(@'mp::process', o);
}

static void
thread_cleanup(void *env)
{
	/* This routine performs some cleanup before a thread is completely
	 * killed. For instance, it has to remove the associated process
	 * object from the list, an it has to dealloc some memory.
	 *
	 * NOTE: thread_cleanup() does not provide enough "protection". In
	 * order to ensure that all UNWIND-PROTECT forms are properly
	 * executed, never use pthread_cancel() to kill a process, but
	 * rather use the lisp functions mp_interrupt_process() and
	 * mp_process_kill().
	 */
	THREAD_OP_LOCK();
	cl_core.processes = ecl_remove_eq(mp_current_process(),
					  cl_core.processes);
	THREAD_OP_UNLOCK();
}

static void *
thread_entry_point(cl_object process)
{
	cl_env_ptr env = process->process.env;

	/* 1) Setup the environment for the execution of the thread */
	pthread_cleanup_push(thread_cleanup, (void *)env);
	ecl_init_env(env);
	init_big_registers(env);
	ecl_set_process_env(env);
	ecl_enable_interrupts_env(env);

	/* 2) Execute the code. The CATCH_ALL point is the destination
	*     provides us with an elegant way to exit the thread: we just
	*     do an unwind up to frs_top.
	*/
	process->process.active = 1;
	CL_CATCH_ALL_BEGIN(env) {
		ecl_bds_bind(env, @'mp::*current-process*', process);
		cl_apply(2, process->process.function, process->process.args);
		ecl_bds_unwind1(env);
	} CL_CATCH_ALL_END;
	process->process.active = 0;

	/* 3) If everything went right, we should be exiting the thread
	 *    through this point. thread_cleanup is automatically invoked.
	 */
#ifdef ECL_WINDOWS_THREADS
	thread_cleanup(env);
	return 1;
#else
	pthread_cleanup_pop(1);
	return NULL;
#endif
}

static cl_object
alloc_process(cl_object name)
{
	cl_object process = ecl_alloc_object(t_process);
	process->process.active = 0;
	process->process.name = name;
	process->process.function = Cnil;
	process->process.args = Cnil;
	process->process.interrupt = Cnil;
	process->process.env = _ecl_alloc_env();
	process->process.env->own_process = process;
	return process;
}

static void
initialize_process_bindings(cl_object process, cl_object initial_bindings)
{
	const cl_env_ptr this_env = ecl_process_env();
	cl_object hash;
	/* FIXME! Here we should either use INITIAL-BINDINGS or copy lexical
	 * bindings */
	if (initial_bindings != OBJNULL) {
		hash = cl__make_hash_table(@'eq', MAKE_FIXNUM(1024),
					   ecl_make_singlefloat(1.5),
					   ecl_make_singlefloat(0.7),
					   Cnil); /* no need for locking */
	} else {
		hash = si_copy_hash_table(this_env->bindings_hash);
	}
	process->process.env->bindings_hash = hash;
}

void
ecl_import_current_thread(cl_object name, cl_object bindings)
{
	cl_object process = alloc_process(name);
	cl_env_ptr env = process->process.env;
	initialize_process_bindings(process, bindings);
	ecl_init_env(env);
	init_big_registers(env);
	ecl_set_process_env(env);
	ecl_enable_interrupts_env(env);
}

void
ecl_release_current_thread(void)
{
	thread_cleanup(&cl_env);
}

@(defun mp::make-process (&key name ((:initial-bindings initial_bindings) Ct))
	cl_object process;
@
	process = alloc_process(name);
	initialize_process_bindings(process, initial_bindings);
	@(return process)
@)

cl_object
mp_process_preset(cl_narg narg, cl_object process, cl_object function, ...)
{
	cl_va_list args;
	cl_va_start(args, function, narg, 2);
	if (narg < 2)
		FEwrong_num_arguments(@'mp::process-preset');
	assert_type_process(process);
	process->process.function = function;
	process->process.args = cl_grab_rest_args(args);
	@(return process)
}

cl_object
mp_interrupt_process(cl_object process, cl_object function)
{
	if (mp_process_active_p(process) == Cnil)
		FEerror("Cannot interrupt the inactive process ~A", 1, process);
#ifdef ECL_WINDOWS_THREADS
	{
	CONTEXT context;
	HANDLE thread = process->process.thread;
	if (SuspendThread(thread) == (DWORD)-1)
		FEwin32_error("Cannot suspend process ~A", 1, process);
	context.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
	if (!GetThreadContext(thread, &context))
		FEwin32_error("Cannot get context for process ~A", 1, process);
	context.Eip = process_interrupt_handler;
	if (!SetThreadContext(thread, &context))
		FEwin32_error("Cannot set context for process ~A", 1, process);
	process->process.interrupt = function;
	if (ResumeThread(thread) == (DWORD)-1)
		FEwin32_error("Cannot resume process ~A", 1, process);
	}
#else
	process->process.interrupt = function;
	if ( pthread_kill(process->process.thread, SIGUSR1) )
		FElibc_error("pthread_kill() failed.", 0);
#endif
	@(return Ct)
}

cl_object
mp_process_kill(cl_object process)
{
	return mp_interrupt_process(process, @'mp::exit-process');
}

cl_object
mp_process_yield(void)
{
#ifdef HAVE_SCHED_YIELD
	sched_yield();
#else
# if defined(_MSVC) || defined(mingw32)
	Sleep(0);
# else
	sleep(0); /* Use sleep(0) to yield to a >= priority thread */
# endif
#endif
	@(return)
}

cl_object
mp_process_enable(cl_object process)
{
	cl_object output;
#ifdef ECL_WINDOWS_THREADS
	HANDLE code;
	DWORD threadId;

	if (mp_process_active_p(process) != Cnil)
		FEerror("Cannot enable the running process ~A.", 1, process);
	THREAD_OP_LOCK();
	code = (HANDLE)CreateThread(NULL, 0, thread_entry_point, process, 0, &threadId);
	if (code) {
		/* If everything went ok, add the thread to the list. */
		cl_core.processes = CONS(process, cl_core.processes);
		output = process;
	} else {
		output = Cnil;
	}
	process->process.thread = code;
	THREAD_OP_UNLOCK();
#else
	pthread_t *posix_thread;
	int code;

	if (mp_process_active_p(process) != Cnil)
		FEerror("Cannot enable the running process ~A.", 1, process);
	THREAD_OP_LOCK();
	code = pthread_create(&process->process.thread, NULL, thread_entry_point, process);
	if (code) {
		output = Cnil;
	} else {
		/* If everything went ok, add the thread to the list. */
		cl_core.processes = CONS(process, cl_core.processes);
		output = process;
	} /* FIXME: how to do FElibc_error() without leaving a lock? */
	THREAD_OP_UNLOCK();
#endif
	@(return output)
}

cl_object
mp_exit_process(void)
{
#ifdef ECL_WINDOWS_THREADS
	int same = GetCurrentThreadId() == main_thread;
#else
	int same = pthread_equal(pthread_self(), main_thread);
#endif
	if (same) {
		/* This is the main thread. Quitting it means exiting the
		   program. */
		si_quit(0);
	} else {
		/* We simply undo the whole of the frame stack. This brings up
		   back to the thread entry point, going through all possible
		   UNWIND-PROTECT.
		*/
		const cl_env_ptr env = ecl_process_env();
		ecl_unwind(env, env->frs_org);
	}
}

cl_object
mp_all_processes(void)
{
	/* No race condition here because this list is never destructively
	 * modified. When we add or remove processes, we create new lists. */
	@(return cl_copy_list(cl_core.processes))
}

cl_object
mp_process_name(cl_object process)
{
	assert_type_process(process);
	@(return process->process.name)
}

cl_object
mp_process_active_p(cl_object process)
{
	assert_type_process(process);
	@(return (process->process.active? Ct : Cnil))
}

cl_object
mp_process_whostate(cl_object process)
{
	assert_type_process(process);
	@(return (cl_core.null_string))
}

cl_object
mp_process_run_function(cl_narg narg, cl_object name, cl_object function, ...)
{
	cl_object process;
	cl_va_list args;
	cl_va_start(args, function, narg, 2);
	if (narg < 2)
		FEwrong_num_arguments(@'mp::process-run-function');
	if (CONSP(name)) {
		process = cl_apply(2, @'mp::make-process', name);
	} else {
		process = mp_make_process(2, @':name', name);
	}
	cl_apply(4, @'mp::process-preset', process, function,
		 cl_grab_rest_args(args));
	return mp_process_enable(process);
}

/*----------------------------------------------------------------------
 * LOCKS or MUTEX
 */

@(defun mp::make-lock (&key name ((:recursive recursive) Ct))
	pthread_mutexattr_t attr;
	cl_object output;
@
	output = ecl_alloc_object(t_lock);
#ifdef ECL_WINDOWS_THREADS
	output->lock.name = name;
	output->lock.mutex = CreateMutex(NULL, FALSE, NULL);
	output->lock.holder = Cnil;
	output->lock.counter = 0;
	output->lock.recursive = (recursive != Cnil);
#else
	pthread_mutexattr_init(&attr);
	output->lock.name = name;
	output->lock.holder = Cnil;
	output->lock.counter = 0;
	if (recursive == Cnil) {
		pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK_NP);
		output->lock.recursive = 0;
	} else {
		pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE_NP);
		output->lock.recursive = 1;
	}
	pthread_mutex_init(&output->lock.mutex, &attr);
	pthread_mutexattr_destroy(&attr);
#endif
	si_set_finalizer(output, Ct);
	@(return output)
@)

cl_object
mp_recursive_lock_p(cl_object lock)
{
	if (type_of(lock) != t_lock)
		FEwrong_type_argument(@'mp::lock', lock);
	@(return (lock->lock.recursive? Ct : Cnil))
}

cl_object
mp_lock_name(cl_object lock)
{
	if (type_of(lock) != t_lock)
		FEwrong_type_argument(@'mp::lock', lock);
	@(return lock->lock.name)
}

cl_object
mp_lock_holder(cl_object lock)
{
	if (type_of(lock) != t_lock)
		FEwrong_type_argument(@'mp::lock', lock);
	@(return lock->lock.holder)
}

cl_object
mp_giveup_lock(cl_object lock)
{
	cl_object own_process = mp_current_process();
	int code;
	if (type_of(lock) != t_lock)
		FEwrong_type_argument(@'mp::lock', lock);
	if (lock->lock.holder != own_process) {
		FEerror("Attempt to give up a lock ~S that is not owned by ~S.", 2,
			lock, own_process);
	}
	if (--lock->lock.counter == 0) {
		lock->lock.holder = Cnil;
	}
#ifdef ECL_WINDOWS_THREADS
        if (ReleaseMutex(lock->lock.mutex) == 0)
		FEwin32_error("Unable to release Win32 Mutex", 0);
#else
	pthread_mutex_unlock(&lock->lock.mutex);
#endif
	@(return Ct)
}

@(defun mp::get-lock (lock &optional (wait Ct))
	cl_object output;
	int rc;
@
	if (type_of(lock) != t_lock)
		FEwrong_type_argument(@'mp::lock', lock);
	/* In Windows, all locks are recursive. We simulate the other case. */
	/* We will complain always if recursive=0 and try to lock recursively. */
	if (!lock->lock.recursive && (lock->lock.holder == the_env->own_process)) {
		FEerror("A recursive attempt was made to hold lock ~S", 1, lock);
	}
#ifdef ECL_WINDOWS_THREADS
	switch (WaitForSingleObject(lock->lock.mutex, (wait==Ct?INFINITE:0))) {
		case WAIT_OBJECT_0:
                        lock->lock.holder = env->own_process;
			lock->lock.counter++;
			output = Ct;
			break;
		case WAIT_TIMEOUT:
			output = Cnil;
			break;
		case WAIT_ABANDONED:
			ecl_internal_error("");
			break;
		case WAIT_FAILED:
			FEwin32_error("Unable to lock Win32 Mutex", 0);
			break;
	}
#else
	if (wait == Ct) {
		rc = pthread_mutex_lock(&lock->lock.mutex);
	} else {
		rc = pthread_mutex_trylock(&lock->lock.mutex);
	}
	if (rc == 0) {
		lock->lock.holder = the_env->own_process;
		lock->lock.counter++;
		output = Ct;
	} else {
		output = Cnil;
	}
#endif
	@(return output)
@)

/*----------------------------------------------------------------------
 * CONDITION VARIABLES
 */

cl_object
mp_make_condition_variable(void)
{
#ifdef ECL_WINDOWS_THREADS
	FEerror("Condition variables are not supported under Windows.", 0);
	@(return Cnil)
#else
	pthread_condattr_t attr;
	cl_object output;

	pthread_condattr_init(&attr);
	output = ecl_alloc_object(t_condition_variable);
	pthread_cond_init(&output->condition_variable.cv, &attr);
	pthread_condattr_destroy(&attr);
	si_set_finalizer(output, Ct);
	@(return output)
#endif
}

cl_object
mp_condition_variable_wait(cl_object cv, cl_object lock)
{
#ifdef ECL_WINDOWS_THREADS
	FEerror("Condition variables are not supported under Windows.", 0);
#else
	if (type_of(cv) != t_condition_variable)
		FEwrong_type_argument(@'mp::condition-variable', cv);
	if (type_of(lock) != t_lock)
		FEwrong_type_argument(@'mp::lock', lock);
	if (pthread_cond_wait(&cv->condition_variable.cv,
	                      &lock->lock.mutex) == 0)
		lock->lock.holder = mp_current_process();
#endif
	@(return Ct)
}

cl_object
mp_condition_variable_timedwait(cl_object cv, cl_object lock, cl_object seconds)
{
#ifdef ECL_WINDOWS_THREADS
	FEerror("Condition variables are not supported under Windows.", 0);
#else
	int rc;
	double r;
	struct timespec   ts;
	struct timeval    tp;

	if (type_of(cv) != t_condition_variable)
		FEwrong_type_argument(@'mp::condition-variable', cv);
	if (type_of(lock) != t_lock)
		FEwrong_type_argument(@'mp::lock', lock);
	/* INV: ecl_minusp() makes sure `seconds' is real */
	if (ecl_minusp(seconds))
		cl_error(9, @'simple-type-error', @':format-control',
			 make_constant_base_string("Not a non-negative number ~S"),
			 @':format-arguments', cl_list(1, seconds),
			 @':expected-type', @'real', @':datum', seconds);
	gettimeofday(&tp, NULL);
	/* Convert from timeval to timespec */
	ts.tv_sec  = tp.tv_sec;
	ts.tv_nsec = tp.tv_usec * 1000;

	/* Add `seconds' delta */
	r = ecl_to_double(seconds);
	ts.tv_sec += (time_t)floor(r);
	ts.tv_nsec += (long)((r - floor(r)) * 1e9);
	if (ts.tv_nsec >= 1e9) {
		ts.tv_nsec -= 1e9;
		ts.tv_sec++;
	}
	if (pthread_cond_timedwait(&cv->condition_variable.cv,
	                           &lock->lock.mutex, &ts) == 0) {
		lock->lock.holder = mp_current_process();
		@(return Ct)
	} else {
		@(return Cnil)
	}
#endif
}

cl_object
mp_condition_variable_signal(cl_object cv)
{
#ifdef ECL_WINDOWS_THREADS
	FEerror("Condition variables are not supported under Windows.", 0);
#else
	if (type_of(cv) != t_condition_variable)
		FEwrong_type_argument(@'mp::condition-variable', cv);
	pthread_cond_signal(&cv->condition_variable.cv);
#endif
	@(return Ct)
}

cl_object
mp_condition_variable_broadcast(cl_object cv)
{
#ifdef ECL_WINDOWS_THREADS
	FEerror("Condition variables are not supported under Windows.", 0);
#else
	if (type_of(cv) != t_condition_variable)
		FEwrong_type_argument(@'mp::condition-variable', cv);
	pthread_cond_broadcast(&cv->condition_variable.cv);
#endif
	@(return Ct)
}

/*----------------------------------------------------------------------
 * INITIALIZATION
 */

void
init_threads(cl_env_ptr env)
{
	cl_object process;
	pthread_mutexattr_t attr;

	cl_core.processes = OBJNULL;
#ifdef ECL_WINDOWS_THREADS
	cl_core.global_lock = CreateMutex(NULL, FALSE, NULL);
#else
	pthread_mutexattr_init(&attr);
	pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK_NP);
	pthread_mutex_init(&cl_core.global_lock, &attr);
	pthread_mutexattr_destroy(&attr);
#endif

	/* We have to set the environment before any allocation takes place,
	 * so that the interrupt handling code works. */
#if !defined(WITH___THREAD)
# if defined(ECL_WINDOWS_THREADS)
	cl_env_key = TlsAlloc();
# else
	pthread_key_create(&cl_env_key, NULL);
# endif
#endif
	ecl_set_process_env(env);

	process = ecl_alloc_object(t_process);
	process->process.active = 1;
	process->process.name = @'si::top-level';
	process->process.function = Cnil;
	process->process.args = Cnil;
	process->process.thread = pthread_self();
	process->process.env = env;

	env->own_process = process;

	cl_core.processes = ecl_list1(process);

#ifdef ECL_WINDOWS_THREADS
	main_thread = GetCurrentThreadId();
#else
	main_thread = pthread_self();
#endif
}
