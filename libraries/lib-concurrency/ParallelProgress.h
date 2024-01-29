/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file ParallelProgress.h
 
 Paul Licameli
 
 **********************************************************************/
#ifndef __AUDACITY_PARALLEL_PROGRESS__
#define __AUDACITY_PARALLEL_PROGRESS__

#include "NonInterfering.h"
#include <atomic>

namespace Parallel {

namespace detail { struct AbortException{}; }

//! A callback supplied to run methods of a task so it can "yield" while it
//! reports progress, also detecting total progress
/*!
 Throws an exception when a thread team is shutting down.  Do not handle this
 exception.

 @tparam Increment an Integral type
 */
template<typename Increment> class Progress {
public:
   using increment_type = Increment;

   //! Report progress; poll total progress; possibly non-returning
   /*!
    This may throw an exception internally to stop a thread when it has been
    detected that other threads in the team have stopped.

    TODO C++20: make Progress an awaitable that can suspend instead of throwing,
    and let tasks be coroutines
    */
   increment_type operator()(increment_type inc = {}) {
      if (abandoned.load(std::memory_order_relaxed))
         throw detail::AbortException{};
      if (inc == increment_type{ 0 }) {
         return total.load(std::memory_order_acquire);
      }
      else {
         const auto old_total = total.fetch_add(inc, std::memory_order_acq_rel);
         return old_total + inc;
      }
   }

   //! Cause shut-down of tasks
   void abandon() { abandoned.store(true, std::memory_order_relaxed); }
private:
   NonInterfering<std::atomic<increment_type>> total{ Increment{} };
   NonInterfering<std::atomic<bool>> abandoned{ false };
};

}

#endif
