/*
  Copyright 2012-2016 David Robillard <http://drobilla.net>

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

/**
   @defgroup worker Worker

   Support for non-realtime plugin operations, see
   <http://lv2plug.in/ns/ext/worker> for details.

   @{
*/

#ifndef LV2_WORKER_H
#define LV2_WORKER_H

#include "lv2/core/lv2.h"

#include <stdint.h>

#define LV2_WORKER_URI    "http://lv2plug.in/ns/ext/worker"  ///< http://lv2plug.in/ns/ext/worker
#define LV2_WORKER_PREFIX LV2_WORKER_URI "#"                 ///< http://lv2plug.in/ns/ext/worker#

#define LV2_WORKER__interface LV2_WORKER_PREFIX "interface"  ///< http://lv2plug.in/ns/ext/worker#interface
#define LV2_WORKER__schedule  LV2_WORKER_PREFIX "schedule"   ///< http://lv2plug.in/ns/ext/worker#schedule

#ifdef __cplusplus
extern "C" {
#endif

/**
   Status code for worker functions.
*/
typedef enum {
	LV2_WORKER_SUCCESS       = 0,  /**< Completed successfully. */
	LV2_WORKER_ERR_UNKNOWN   = 1,  /**< Unknown error. */
	LV2_WORKER_ERR_NO_SPACE  = 2   /**< Failed due to lack of space. */
} LV2_Worker_Status;

/** Opaque handle for LV2_Worker_Interface::work(). */
typedef void* LV2_Worker_Respond_Handle;

/**
   A function to respond to run() from the worker method.

   The `data` MUST be safe for the host to copy and later pass to
   work_response(), and the host MUST guarantee that it will be eventually
   passed to work_response() if this function returns LV2_WORKER_SUCCESS.
*/
typedef LV2_Worker_Status (*LV2_Worker_Respond_Function)(
	LV2_Worker_Respond_Handle handle,
	uint32_t                  size,
	const void*               data);

/**
   Plugin Worker Interface.

   This is the interface provided by the plugin to implement a worker method.
   The plugin's extension_data() method should return an LV2_Worker_Interface
   when called with LV2_WORKER__interface as its argument.
*/
typedef struct _LV2_Worker_Interface {
	/**
	   The worker method.  This is called by the host in a non-realtime context
	   as requested, possibly with an arbitrary message to handle.

	   A response can be sent to run() using `respond`.  The plugin MUST NOT
	   make any assumptions about which thread calls this method, except that
	   there are no real-time requirements and only one call may be executed at
	   a time.  That is, the host MAY call this method from any non-real-time
	   thread, but MUST NOT make concurrent calls to this method from several
	   threads.

	   @param instance The LV2 instance this is a method on.
	   @param respond  A function for sending a response to run().
	   @param handle   Must be passed to `respond` if it is called.
	   @param size     The size of `data`.
	   @param data     Data from run(), or NULL.
	*/
	LV2_Worker_Status (*work)(LV2_Handle                  instance,
	                          LV2_Worker_Respond_Function respond,
	                          LV2_Worker_Respond_Handle   handle,
	                          uint32_t                    size,
	                          const void*                 data);

	/**
	   Handle a response from the worker.  This is called by the host in the
	   run() context when a response from the worker is ready.

	   @param instance The LV2 instance this is a method on.
	   @param size     The size of `body`.
	   @param body     Message body, or NULL.
	*/
	LV2_Worker_Status (*work_response)(LV2_Handle  instance,
	                                   uint32_t    size,
	                                   const void* body);

	/**
	   Called when all responses for this cycle have been delivered.

	   Since work_response() may be called after run() finished, this provides
	   a hook for code that must run after the cycle is completed.

	   This field may be NULL if the plugin has no use for it.  Otherwise, the
	   host MUST call it after every run(), regardless of whether or not any
	   responses were sent that cycle.
	*/
	LV2_Worker_Status (*end_run)(LV2_Handle instance);
} LV2_Worker_Interface;

/** Opaque handle for LV2_Worker_Schedule. */
typedef void* LV2_Worker_Schedule_Handle;

/**
   Schedule Worker Host Feature.

   The host passes this feature to provide a schedule_work() function, which
   the plugin can use to schedule a worker call from run().
*/
typedef struct _LV2_Worker_Schedule {
	/**
	   Opaque host data.
	*/
	LV2_Worker_Schedule_Handle handle;

	/**
	   Request from run() that the host call the worker.

	   This function is in the audio threading class.  It should be called from
	   run() to request that the host call the work() method in a non-realtime
	   context with the given arguments.

	   This function is always safe to call from run(), but it is not
	   guaranteed that the worker is actually called from a different thread.
	   In particular, when free-wheeling (e.g. for offline rendering), the
	   worker may be executed immediately.  This allows single-threaded
	   processing with sample accuracy and avoids timing problems when run() is
	   executing much faster or slower than real-time.

	   Plugins SHOULD be written in such a way that if the worker runs
	   immediately, and responses from the worker are delivered immediately,
	   the effect of the work takes place immediately with sample accuracy.

	   The `data` MUST be safe for the host to copy and later pass to work(),
	   and the host MUST guarantee that it will be eventually passed to work()
	   if this function returns LV2_WORKER_SUCCESS.

	   @param handle The handle field of this struct.
	   @param size   The size of `data`.
	   @param data   Message to pass to work(), or NULL.
	*/
	LV2_Worker_Status (*schedule_work)(LV2_Worker_Schedule_Handle handle,
	                                   uint32_t                   size,
	                                   const void*                data);
} LV2_Worker_Schedule;

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* LV2_WORKER_H */

/**
   @}
*/
