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

#include <pthread.h>
#include "ecl.h"

pthread_mutex_t ecl_threads_mutex = PTHREAD_MUTEX_INITIALIZER;

static pthread_key_t cl_env_key;

static pthread_t main_thread;

extern void ecl_init_env(struct cl_env_struct *env);

struct cl_env_struct *
ecl_process_env(void)
{
	return pthread_getspecific(cl_env_key);
}

cl_object
mp_current_process(void)
{
	return cl_env.own_process;
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
	cl_object *p, l, process  = cl_env.own_process;

	pthread_mutex_lock(&ecl_threads_mutex);
	p = &cl_core.processes;
	for (l = *p; l != Cnil; ) {
		if (CAR(l) == process) {
			*p = CDR(l);
			break;
		}
		p = &CDR(l);
		l = *p;
	}
	cl_dealloc(process->process.thread, sizeof(pthread_t));
	process->process.thread = NULL;
	pthread_mutex_unlock(&ecl_threads_mutex);
}

static void *
thread_entry_point(cl_object process)
{
	/* 1) Setup the environment for the execution of the thread */
	pthread_cleanup_push(thread_cleanup, (void *)process->process.env);
	pthread_setspecific(cl_env_key, process->process.env);
	ecl_init_env(process->process.env);

	/* 2) Execute the code */
	CL_CATCH_ALL_BEGIN {
		bds_bind(@'mp::*current-process*', process);
		cl_apply(2, process->process.function, process->process.args);
		bds_unwind1();
	} CL_CATCH_ALL_END;

	/* 3) Remove the thread. thread_cleanup is automatically invoked. */
	pthread_cleanup_pop(1);
	return NULL;
}

@(defun mp::make-process (&key name ((:initial-bindings initial_bindings) Ct))
@
	cl_object process;
	cl_object hash;

	process = cl_alloc_object(t_process);
	process->process.name = name;
	process->process.function = Cnil;
	process->process.args = Cnil;
	process->process.thread = NULL;
	process->process.env = cl_alloc(sizeof(*process->process.env));
	/* FIXME! Here we should either use INITIAL-BINDINGS or copy lexical
	 * bindings */
	if (initial_bindings != OBJNULL) {
		hash = cl__make_hash_table(@'eq', MAKE_FIXNUM(1024),
					   make_shortfloat(1.5),
					   make_shortfloat(0.7));
	} else {
		hash = si_copy_hash_table(cl_env.bindings_hash);
	}
	process->process.env->bindings_hash = hash;
	process->process.env->own_process = process;
	@(return process)
@)

cl_object
mp_process_preset(int narg, cl_object process, cl_object function, ...)
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
mp_process_kill(cl_object process)
{
	cl_object output = Cnil;
	assert_type_process(process);
	if (process->process.thread) {
		if (pthread_cancel(*((pthread_t*)process->process.thread)) == 0)
			output = Ct;
	}
	@(return output)
}

cl_object
mp_process_enable(cl_object process)
{
	pthread_t *posix_thread;
	int code;

	assert_type_process(process);
	if (process->process.thread != NULL)
		FEerror("Cannot enable the running process ~A.", 1, process);
	posix_thread = cl_alloc_atomic(sizeof(*posix_thread));
	process->process.thread = posix_thread;
	pthread_mutex_lock(&ecl_threads_mutex);
	code = pthread_create(posix_thread, NULL, thread_entry_point, process);
	if (!code) {
		/* If everything went ok, add the thread to the list. */
		cl_core.processes = CONS(process, cl_core.processes);
	}
	pthread_mutex_unlock(&ecl_threads_mutex);
	@(return (code? Cnil : process))
}

cl_object
mp_exit_process(void)
{
	if (pthread_equal(pthread_self(), main_thread)) {
		/* This is the main thread. Quitting it means exiting the
		   program. */
		cl_quit(0);
	} else {
		cl_object tag = cl_env.bindings_hash;
		/* We simply throw with a catch value that nobody can have. This
		   brings up back to the thread entry point, going through all
		   possible UNWIND-PROTECT.
		*/
		NVALUES=0;
		VALUES(0)=Cnil;
		cl_throw(tag);
	}
}

cl_object
mp_all_processes(void)
{
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
	@(return ((process->process.thread == NULL)? Cnil : Ct))
}

cl_object
mp_process_whostate(cl_object process)
{
	assert_type_process(process);
	@(return (cl_core.null_string))
}

cl_object
mp_process_run_function(int narg, cl_object name, cl_object function, ...)
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

@(defun mp::make-lock (&key name)
@
	cl_object output = cl_alloc_object(t_lock);
	output->lock.name = name;
	output->lock.mutex = cl_alloc(sizeof(pthread_mutex_t));
	pthread_mutex_init(output->lock.mutex, NULL);
	@(return output)
@)

cl_object
mp_giveup_lock(cl_object lock)
{
	if (type_of(lock) != t_lock)
		FEwrong_type_argument(@'mp::lock', lock);
	pthread_mutex_unlock(lock->lock.mutex);
	@(return Ct)
}

@(defun mp::get-lock (lock &optional (wait Ct))
	cl_object output;
@
	if (type_of(lock) != t_lock)
		FEwrong_type_argument(@'mp::lock', lock);
	if (wait == Ct) {
		pthread_mutex_lock(lock->lock.mutex);
		output = Ct;
	} else if (pthread_mutex_trylock(lock->lock.mutex) == 0) {
		output = Ct;
	} else {
		output = Cnil;
	}
	@(return output)
@)

void
init_threads()
{
	cl_object process;
	struct cl_env_struct *env;

	cl_core.processes = OBJNULL;
	pthread_mutex_init(&ecl_threads_mutex, NULL);

	process = cl_alloc_object(t_process);
	process->process.name = @'si::top-level';
	process->process.function = Cnil;
	process->process.args = Cnil;
	process->process.thread = NULL;
	process->process.thread = cl_alloc(sizeof(pthread_t));
	*((pthread_t *)process->process.thread) = pthread_self();
	process->process.env = env = cl_alloc(sizeof(*env));

	pthread_key_create(&cl_env_key, NULL);
	pthread_setspecific(cl_env_key, env);
	env->own_process = process;

	ECL_SET(@'mp::*current-process*', process);
	cl_core.processes = CONS(process, Cnil);

	main_thread = pthread_self();
}
