/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file ParallelReduce.h
 
 Paul Licameli
 
 **********************************************************************/
#ifndef __AUDACITY_PARALLEL_REDUCE__
#define __AUDACITY_PARALLEL_REDUCE__

#include <cassert>
#include <exception>
#include <functional>
#include <future>
#include <iterator>
#include <thread>
#include <type_traits>
#include <utility>
#include <vector>

#include "ParallelProgress.h"
#include "Splittable.h"

namespace Parallel {

//! Compute the result of a task that admits data-parallel decomposition of
//! a domain described by one iterator range
/*!
 The simple subdivision assumes that the time spent in Task::run is proportional
 to the size of the iterator range.

 Copies of Task for non-empty, disjoint sub-ranges may be created and each of
 them run on its own thread.  The calling thread always runs the original task
 object on the leftmost range.

 @tparam Task satisfies the @ref Splittable concept.

 It splitting constructor, `Task(Task &orig, iterator_type iter)` is called zero
 or more times, only before `run`.

 It also must define:
 
 - `void run(Progress<Increment> &progress)` which typically iterates the
 range and is called exactly once per task
 - `void merge(Task &&task, Progress<Increment> &progress)`, which is called
 exactly once for each task made by the split constructor (absent exceptions),
 and only after `run` is called on `this` and on `task`

 @tparam Increment defaults to the difference type of the Splittable iterators

 @param task If `(std::distance(task.begin(), task.end()))` is zero or less, no
 other threads are recruited and execution is serial.

 @param n_threads maximum number of threads to use in the computation, including
 the calling thread; supply 1 to do all work serially

 @throws if any exceptions escape the split constructor or any member function
 called on any task, then one of the exceptions (or a copy thereof) propagates
 on the thread calling this function, but only after all running tasks finish
 (thus avoiding dangling references); also possibly `std::bad_alloc`.

 After `run` throws on one task, any other task will prompty shut down after it
 next tests `progress`.

 @pre `n_threads >= 1`
 */
template<
   typename Task,
   typename Increment
      = typename std::iterator_traits<typename Task::iterator_type>
         ::difference_type
>
void OneDimensionalReduce(Task &task,
   unsigned n_threads = std::thread::hardware_concurrency());

enum ReportChoice{ NoReport, Report };
enum MergeChoice{ NoMerge, Merge };

//! Adapts its base as a task type that checks and reports progress
/*!
 Generates a `run` that iterates the task's range, calling operator() on
 each element, and yielding after each.

 Also, generates a `merge` that yields, and if doMerge, that function calls a
 one-argument `merge` in the base class.

 If doReport, then these base class functions are expected to return an
 increment value that is passed to the yielder; also the base must define
 `report_progress` which accepts the total progress value and is called only on
 the original task.
 */
template<typename Task, typename Increment,
   MergeChoice doMerge, ReportChoice doReport>
struct TaskAdapter : Task {
   using Task::Task;
   TaskAdapter(
      TaskAdapter &orig, typename Task::iterator_type iter)
      : Task{ orig, iter }
      , is_first{ false }
   {}
   void run(Progress<Increment> &progress)
   {
      std::for_each(this->begin(), this->end(), [this, &progress](auto &&value){
         if constexpr (doReport) {
            auto total =
               progress((*this)(std::forward<decltype(value)>(value)));
            if (is_first)
               this->report_progress(total);
         }
         else {
            (*this)(std::forward<decltype(value)>(value));
            progress(Increment{});
         }
      });
   }
   void merge(TaskAdapter &&other, Progress<Increment> &progress) {
      if constexpr (doMerge) {
         if constexpr (doReport) {
            auto total = progress(this->Task::merge(std::move(other)));
            if (is_first)
               this->report_progress(total);
         }
         else {
            this->Task::merge(std::move(other));
            progress(Increment{});
         }
      }
      else
         progress(Increment{});
   }
private:
   bool is_first{ true };
};

namespace detail {
template<typename Iterator, typename Callable>
struct ForEach_Task : SplittableBase<Iterator> {
   using iterator_type = Iterator;

   ForEach_Task(Iterator first, Iterator last, Callable callable)
      : SplittableBase<Iterator>{ first, last }
      , callable{ std::move(callable) }
   {}
   ForEach_Task(ForEach_Task &task, Iterator iterator)
      : SplittableBase<Iterator>{ task, iterator }
      , callable{ task.callable }
   {}

   template<typename Value> void operator ()(Value &&value) {
      return callable(std::forward<Value>(value)); }

   Callable callable;
};
}

//! A convenient type for a body of a simple parallelized for_each
template<typename Iterator, typename Callable> struct ForEach
: TaskAdapter<detail::ForEach_Task<Iterator, Callable>,
   typename std::iterator_traits<Iterator>::difference_type,
   NoMerge, NoReport
>{
   using TaskAdapter<detail::ForEach_Task<Iterator, Callable>,
      typename std::iterator_traits<Iterator>::difference_type,
      NoMerge, NoReport
   >::TaskAdapter;
};

// Deduction guide
template<typename Iterator, typename Callable>
ForEach(Iterator, Iterator, Callable) -> ForEach<Iterator, Callable>;

template<typename Task, typename Increment>
void OneDimensionalReduce(Task &task, unsigned n_threads)
{
   assert(n_threads >= 1);
   // TODO: This function might be modified instead to allocate tasks to threads
   // from a pool, and the Task class might define other policies for
   // subdivision of its range
   n_threads = std::max(1u, n_threads);
   using Iterator = typename Task::iterator_type;
   const Iterator begin = task.begin(),
      end = task.end();
   using Distance = typename std::iterator_traits<Iterator>::difference_type;
   const auto count = std::max(std::distance(begin, end), Distance{});

   // Split the task
   std::vector<Task> tasks;
   tasks.reserve(n_threads - 1);
   {
      Task *prevTask = &task;
      auto first = begin;
      const auto step = count / n_threads;
      const auto remainder = count % n_threads;
      Distance distance{},
         fraction{};
      for (size_t ii = 1; ii < n_threads; ++ii) {
         // Don't evaluate (ii * count) / n_threads because there might be
         // overflow
         auto newDistance = distance + step;
         fraction += remainder;
         if (fraction >= n_threads) {
            fraction -= n_threads;
            ++newDistance;
         }
         if (newDistance != distance) {
            const auto oldFirst = first;
            std::advance(first, newDistance - distance);
            newDistance = distance;
            if (first == end)
               break;
            // Invoke the split constructor, which mutates *prevTask
            tasks.emplace_back(*prevTask, first);
            // Test postconditions expected of the split constructor
            assert(prevTask->begin() == oldFirst);
            assert(prevTask->end() == first);
            prevTask = &tasks.back();
            assert(prevTask->begin() == first);
            assert(prevTask->end() == end);
         }
      }
   }
   n_threads = tasks.size() + 1;

   // We will need futures as communication channels among tasks
   using Packaged = std::packaged_task<void(Task&, size_t)>;
   using Future = std::future<void>;
   using Channel = std::pair<Packaged, Future>;
   std::vector<Channel> channels;
   channels.reserve(tasks.size());

   // The runner captures a reference to the array of not-yet-constructed
   // channels
   Progress<Increment> progress;
   const auto runner =
   [n_threads, &tasks, &channels, &progress](Task &task, size_t index){
      // Typically the task will iterate over its sub-range
      std::exception_ptr exc;
      bool aborting = false;
      try {
         task.run(progress);
      }
      catch(const detail::AbortException &) {
         // Some other thread aborted; don't skip the merging below, which must
         // propagate the original exception.  Don't propagate AbortException.
         aborting = true;
      }
      catch(...) {
         aborting = true;
         // Cause other threads to go through the previous catch when they test
         // progress
         progress.abandon();
         exc = std::current_exception();
      }
      // Merge results of some other tasks with higher index, in a "butterfly"
      // (or "hypercube") pattern
      for (size_t step = 1; !(index & step); step <<= 1) {
         auto neighborIndex = index + step;
         if (neighborIndex >= n_threads)
            break;
         --neighborIndex;
         // Synchronize on the completion of the other task before merging
         // (or, propagate an exception)
         try {
            channels[neighborIndex].second.get();
         }
         catch(...) {
            aborting = true;
            // Remember only one exception
            exc = std::current_exception();
         }
         if (!aborting)
            try {
               task.merge(std::move(tasks[neighborIndex]), progress);
            }
            catch(...) {
               aborting = true;
               // Remember only one exception
               exc = std::current_exception();
            }
      }
      if (exc)
         rethrow_exception(exc);
   };

   // Make the channels, but don't start threads before all futures exist!
   for (auto &task : tasks) {
      auto pt = Packaged{ std::cref(runner) };
      auto future = pt.get_future();
      channels.emplace_back(move(pt), move(future));
   }

   // Now it's safe to run the new tasks concurrently, for all indices more than
   // 0
   size_t index = 0;
   for (auto &task : tasks) {
      std::thread{ ref(channels[index].first), std::ref(task), index + 1 }
         .detach();
      ++index;
   }

   // Run the leftmost sub-range (for index 0) in this thread and merge all
   runner(task, 0);
}

}

#endif
