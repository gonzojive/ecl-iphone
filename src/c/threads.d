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

extern void ecl_int_env(struct cl_env_struct *env);

struct cl_env_struct *
ecl_thread_env(void)
{
	return pthread_getspecific(cl_env_key);
}

static void
thread_cleanup(void *env)
{
	cl_object *p, l;

	pthread_mutex_lock(&ecl_threads_mutex);
	p = &cl_core.threads;
	for (l = *p; l != Cnil; ) {
		cl_object thread = CAR(l);
		if (thread->thread.env == env) {
			*p = CDR(l);
			break;
		}
		p = &CDR(l);
		l = *p;
	}
	pthread_mutex_unlock(&ecl_threads_mutex);
}

static void *
thread_entry_point(cl_object thread)
{
	/* 1) Setup the environment for the execution of the thread */
	pthread_cleanup_push(thread_cleanup, (void *)thread->thread.env);
	pthread_setspecific(cl_env_key, thread->thread.env);
	ecl_init_env(thread->thread.env);

	/* 2) Execute the code */
	cl_apply(2, thread->thread.function, thread->thread.args);

	/* 3) Remove the thread. thread_cleanup is automatically invoked. */
	pthread_cleanup_pop(1);
	return NULL;
}

cl_object
si_thread_launch(int narg, cl_object function, ...)
{
	cl_object thread;
	pthread_t *posix_thread;
	int code;
	cl_va_list args;
	cl_va_start(args, function, narg, 1);

	posix_thread = cl_alloc_atomic(sizeof(*posix_thread));
	thread = cl_alloc_object(t_thread);
	thread->thread.function = function;
	thread->thread.args = cl_grab_rest_args(args);
	thread->thread.pthread = posix_thread;
	thread->thread.env = cl_alloc(sizeof(*thread->thread.env));
	pthread_mutex_lock(&ecl_threads_mutex);
	code = pthread_create(posix_thread, NULL, thread_entry_point, thread);
	if (!code) {
		/* If everything went ok, add the thread to the list. */
		cl_core.threads = CONS(thread, cl_core.threads);
	}
	pthread_mutex_unlock(&ecl_threads_mutex);
	@(return (code? Cnil : thread))
}

cl_object
si_thread_list(void)
{
	@(return cl_copy_list(cl_core.threads))
}

cl_object
si_thread_kill(cl_object thread)
{
	cl_object output = Cnil;
	if (type_of(thread) != t_thread)
		FEwrong_type_argument(@'si::thread', thread);
	if (thread->thread.pthread) {
		if (pthread_cancel(*((pthread_t*)thread->thread.pthread)) == 0)
			output = Ct;
	}
	@(return output)
}

cl_object
si_thread_exit(void)
{
	pthread_exit(NULL);
}

void
init_threads()
{
	cl_core.threads = OBJNULL;
	pthread_mutex_init(&ecl_threads_mutex, NULL);
	pthread_key_create(&cl_env_key, NULL);
	pthread_setspecific(cl_env_key, cl_alloc(sizeof(struct cl_env_struct)));
}
