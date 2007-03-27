/*
  libcore: library for basic data structures and algorithms.
  Copyright (C) 2005  IPD Goos, Universit"at Karlsruhe, Germany

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/



#ifndef _LC_TIMING_H
#define _LC_TIMING_H

#include <libcore/lc_printf.h>

typedef struct _lc_timer_t lc_timer_t;

/**
 * Switch to real-time scheduling.
 * This shall make measurements more precise.
 * @note Does not work for all operating systems.
 * @note You could need special user privileges.
 * @return 0 on success, else UNIX error code.
 */
int lc_timer_enter_high_priority(void);

/**
 * Leave the high priority mode.
 * @see lc_timer_enter_high_priority()
 * @return 0 on success, else UNIX error code.
 */
int lc_timer_leave_high_priority(void);

/**
 * Get the amount of bytes allocated on the heap.
 * @return The number of bytes allocated on the heap.
 */
size_t lc_get_heap_used_bytes(void);

/**
 * Register a new timer.
 * If the timer was registered before, the registered timer is returned.
 * @param name  The name of the timer.
 * @param desc  The description of the timer.
 * @return The timer.
 */
lc_timer_t *lc_timer_register(const char *name, const char *desc);

/**
 * Start a timer.
 * @param timer The timer. 
 */
void lc_timer_start(lc_timer_t *timer);

/**
 * Reset a timer and start it.
 * @param timer The timer.
 */
void lc_timer_reset_and_start(lc_timer_t *timer);

/**
 * Reset a timer.
 * @param timer The timer.
 */
void lc_timer_reset(lc_timer_t *timer);

/**
 * Stop a timer.
 * Stopping a stopped timer has no effect.
 * @param timer The timer.
 */
void lc_timer_stop(lc_timer_t *timer);

/**
 * Push a timer of the timer stack. This automatically
 * stop the previous timer on tos and start the new one.
 *
 * @param timer   The timer to push on stack.
 * @return non-zero on succes, zero if the timer is already on the stack.
 */
int lc_timer_push(lc_timer_t *timer);

/**
 * Pop the current timer. This automatically stops it and
 * start the timer that is now on the stack.
 * @return the popped timer
 */
lc_timer_t *lc_timer_pop(void);

/**
 * Get the number of milliseconds, the timer has elapsed.
 * @param timer The timer.
 * @return The number of milliseconds the timer is (was) running.
 */
unsigned long lc_timer_elapsed_msec(const lc_timer_t *timer);

/**
 * Get the number of microseconds, the timer has elapsed.
 * @param timer The timer.
 * @return The number of milliseconds the timer is (was) running.
 */
unsigned long lc_timer_elapsed_usec(const lc_timer_t *timer);

/**
 * Get name of given timer.
 * @param timer The timer.
 * @return The name of the timer.
 */
const char *lc_timer_get_name(const lc_timer_t *timer);

/**
 * Get description of given timer.
 * @param timer The timer.
 * @return The description of the timer.
 */
const char *lc_timer_get_description(const lc_timer_t *timer);

/**
 * Get the lc_printf environment for timers.
 */
const lc_arg_env_t *lc_timer_get_arg_env(void); 
	
#endif /* _LC_TIMING_H */
