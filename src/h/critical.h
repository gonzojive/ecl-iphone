/*
    critical.h  -- Mutual exclusion.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

/*
 * SCHEDULER STOPPING
 */
#ifdef THREADS
#define BUSY 1
#define CRITICAL 5
#define ACTIVE 1
#define INACTIVE 0
#define SCHEDULER_INT 1
#define ERROR_INT 2

extern bool scheduler_interrupted;
extern int critical_level;

#define start_critical_section() {critical_level++;}

#define end_critical_section() {  critical_level--; \
				  if (scheduler_interrupted) \
				    if (critical_level == 0) \
				      interruption_handler(); }
#endif /* THREADS */

/*
 * EMPTY MACROS FOR SINGLE-THREADED CODE
 */
#ifndef THREADS
#define start_critical_section()
#define end_critical_section()
#endif /* !THREADS */
