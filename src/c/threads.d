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

static struct cl_env_struct cl_envs_array[128];
static pthread_key_t cl_env_key;

extern void ecl_intt_env(struct cl_env_struct *env);

struct cl_env_struct *
ecl_thread_env(void)
{
	return pthread_getspecific(cl_env_key);
}

/*----------------------------------------------------------------------
 * THREAD OBJECT
 */

static void
thread_cleanup(void *env)
{
	cl_object *p, l, process;

	pthread_mutex_lock(&ecl_threads_mutex);
	p = &cl_core.processes;
	for (l = *p; l != Cnil; ) {
		process = CAR(l);
		if (process->process.env == env) {
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
	bds_bind(@'si::*current-process*', process);
	cl_apply(2, process->process.function, process->process.args);
	bds_unwind1();

	/* 3) Remove the thread. thread_cleanup is automatically invoked. */
	pthread_cleanup_pop(1);
	return NULL;
}

cl_object
si_make_process(int narg, cl_object name, cl_object function, ...)
{
	cl_object process;
	cl_va_list args;
	cl_va_start(args, function, narg, 2);

	process = cl_alloc_object(t_process);
	process->process.name = name;
	process->process.function = function;
	process->process.args = cl_grab_rest_args(args);
	process->process.thread = NULL;
	process->process.env = cl_alloc(sizeof(*process->process.env));
	process->process.env->bindings_hash =
		si_copy_hash_table(cl_env.bindings_hash);
	return si_restart_process(process);
}

cl_object
si_kill_process(cl_object process)
{
	cl_object output = Cnil;
	if (type_of(process) != t_process)
		FEwrong_type_argument(@'si::process', process);
	if (process->process.thread) {
		if (pthread_cancel(*((pthread_t*)process->process.thread)) == 0)
			output = Ct;
	}
	@(return output)
}

cl_object
si_exit_process(void)
{
	pthread_exit(NULL);
}

cl_object
si_restart_process(cl_object process)
{
	pthread_t *posix_thread;
	int code;

	if (type_of(process) != t_process)
		FEwrong_type_argument(@'si::process', process);
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
si_all_processes(void)
{
	@(return cl_copy_list(cl_core.processes))
}

cl_object
si_process_name(cl_object process)
{
	if (type_of(process) != t_process)
		FEwrong_type_argument(@'si::process', process);
	@(return process->process.name)
}

cl_object
si_process_active_p(cl_object process)
{
	/* FIXME! */
	if (type_of(process) != t_process)
		FEwrong_type_argument(@'si::process', process);
	@(return (process->process.thread != NULL? Ct : Cnil))
}

cl_object
si_process_whostate(cl_object process)
{
	/* FIXME! */
	if (type_of(process) != t_process)
		FEwrong_type_argument(@'si::process', process);
	@(return (cl_core.null_string))
}


/*----------------------------------------------------------------------
 * LOCKS or MUTEX
 */

@(defun si::make-lock (&key name)
@
	cl_object output = cl_alloc_object(t_lock);
	output->lock.name = name;
	output->lock.mutex = cl_alloc(sizeof(pthread_mutex_t));
	pthread_mutex_init(output->lock.mutex, NULL);
	@(return output)
@)

cl_object
si_giveup_lock(cl_object lock)
{
	if (type_of(lock) != t_lock)
		FEwrong_type_argument(@'si::lock', lock);
	pthread_mutex_unlock(lock->lock.mutex);
	@(return Ct)
}

@(defun si::get-lock (lock &optional (wait Ct))
	cl_object output;
@
	if (type_of(lock) != t_lock)
		FEwrong_type_argument(@'si::lock', lock);
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
	cl_core.processes = OBJNULL;
	pthread_mutex_init(&ecl_threads_mutex, NULL);
	pthread_key_create(&cl_env_key, NULL);
	pthread_setspecific(cl_env_key, cl_alloc(sizeof(struct cl_env_struct)));
}
