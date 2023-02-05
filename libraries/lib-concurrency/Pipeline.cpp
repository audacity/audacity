/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file Pipeline.cpp
 
 Paul Licameli
 
 **********************************************************************/
#include "Pipeline.h"

#include <algorithm>
#include <thread>
// C++20 use std::counting_semaphore not wxSemaphore
#include <wx/log.h>
#include <wx/thread.h>
#include "MemoryX.h"

namespace TaskParallel {
namespace detail {
struct BadSemaphore{};

struct TypeErasedPipeline::Channel {
   // might generalize to variable numbers of slots in future
   static constexpr size_t NSlots = 2;

   Channel() = default;
   bool Initialized() const
   {
      return all_of(begin(slots), end(slots), [](auto &slot){
         return slot.first.has_value() &&
            slot.first->has_value() &&
            slot.second.has_value();
      });
   }
   bool Reset()
   {
      for (auto &slot : slots)
         slot.Assign({ {}, {} });

      static_assert(!wxSEMA_NO_ERROR); // wxSemaphore's good return is false
      auto error = wxSEMA_NO_ERROR;
      // Drain full to 0 (or until a real error)
      do
         error = full.semaphore.TryWait();
      while (!error);
      if (error != wxSEMA_BUSY)
         return false;
      // Likewise empty, but then raise it back to known value of 2
      do
         error = empty.semaphore.TryWait();
      while (!error);
      if (error != wxSEMA_BUSY
          || empty.semaphore.Post() || empty.semaphore.Post())
         return false;

      empty.index = 0;
      full.index = 0;
      return true;
   }

   struct Port {
      Port(int count)
         : semaphore{ count }, index{ 0 }
      {}

      unsigned char Next()
      {
         return (index = (index + 1) % NSlots);
      }

      wxSemaphore semaphore;
      unsigned char index;
   };

   //! Upstream writer task waits on empty, downstream reader on full
   NonInterfering<Port> empty{ 2 }, full{ 0 };
   //! Ports govern race free alternation of usage of slots by producer and
   //! consumer, which allows read and write to overlap in time
   NonInterfering<std::pair<std::optional<Data>, Status>> slots[NSlots];
};

TypeErasedPipeline::TypeErasedPipeline(
   Source source, std::vector<Stage> stages, Sink sink, const wxString &name
)  : mSource{ move(source) }
   , mStages{ move(stages) }
   , mSink{ move(sink) }
   , mChannels(1 + mStages.size())
   , mName{ name.empty() ? wxString{ "<unnamed>" } : name }
   //! Check for failure of wxSemaphore constructors
   , mOk{ all_of(mChannels.begin(), mChannels.end(), [](auto &channel) {
      return channel.empty.semaphore.IsOk() && channel.full.semaphore.IsOk();
   } ) }
{
}

TypeErasedPipeline::~TypeErasedPipeline() = default;

void TypeErasedPipeline::Initialize(
   size_t index, const Initializer &initializer)
{
   assert(index <= NStages()); // pre
   assert(mChannels.size() == 1 + NStages()); // constructor
   if (index < mChannels.size()) {
      //! Call initializer more than once
      for (auto &slot : mChannels[index].slots)
         slot.Assign(initializer());
   }
}

bool TypeErasedPipeline::Initialized() const
{
   return all_of(mChannels.begin(), mChannels.end(),
      std::mem_fn(&Channel::Initialized));
}

static inline auto MakeTimeUpdater(std::chrono::nanoseconds &accumulator) {
   using namespace std::chrono;
   return finally([&accumulator, pollStart = high_resolution_clock::now()] {
      accumulator += high_resolution_clock::now() - pollStart;
   });
}

static inline double NsToNumber(std::chrono::nanoseconds ns) {
   using namespace std::chrono;
   return duration_cast<duration<double>>(ns).count();
}

auto TypeErasedPipeline::TimedRun(TaskAssignment assignment)
   -> std::vector<Status>
{
   const auto nStages = mStages.size();
   const auto nResults = 2 + nStages;
   std::vector<Timing> timings(nResults);
   std::vector<std::exception_ptr> exceptions(nResults);

   std::chrono::nanoseconds total{};
   auto result = (MakeTimeUpdater(total), Run(assignment, exceptions, timings));
 
   // Report time even in case of exceptional termination, before rethrowing
   wxLogInfo("Timing summary for pipeline '%s':", mName);
   for (size_t ii = 0; ii < nResults; ++ii) {
      const auto &timing = timings[ii];
      auto stageName =
         (ii == 0) ? wxString{ "Source" }
         : (ii == nResults - 1) ? wxString{ "Sink" }
         : wxString::Format("Stage %d", static_cast<int>(ii));
      const auto inner = NsToNumber(timing.functionTime),
         stageTotal = NsToNumber(timing.allTime),
         wait = stageTotal - inner;
      wxLogInfo(
         "\t%s called %dx: %fs plus %fs wait",
         stageName, timing.passes, inner, wait);
   }
   wxLogInfo("\tTotal: %fs", NsToNumber(total));

   // Propagate exceptions except BadSemaphore
   // TODO: use std::nested_exception if there is more than one?
   bool semaphoreError = false;
   for (auto &pException : exceptions) {
      if (pException) {
         try {
            rethrow_exception(pException);
         }
         catch (const BadSemaphore &) {
            semaphoreError = true;
         }
      }
   }

   if (semaphoreError)
      return {};

   return result;
}

auto TypeErasedPipeline::Run(TaskAssignment assignment,
   std::vector<std::exception_ptr> &exceptions, std::vector<Timing> &timings)
      -> std::vector<Status>
{
   if (!mOk || !Initialized())
      return {};

   Finally Do{ [this] {
      // Revert pipeline object to uninitialized state
      for (auto & channel : mChannels) {
         mOk = mOk && channel.Reset();
      }
   } };

   const auto nStages = mStages.size();
   const auto nResults = 2 + nStages;
   std::vector<Status> statuses(nResults);

   auto result = [&](size_t ii) -> Result {
      return { statuses[ii], exceptions[ii], timings[ii] };
   };

   std::vector<std::thread> threads;
   threads.reserve(nResults);
   if (assignment != TaskAssignment::Source)
      threads.emplace_back(SourceRunner, result(0),
         cref(mSource), std::ref(mChannels[0]));
   for (size_t ii = 0; ii < nStages; ++ii)
      threads.emplace_back(StageRunner, result(ii + 1),
         std::ref(mChannels[ii]), cref(mStages[ii]),
         std::ref(mChannels[ii + 1]));
   if (assignment != TaskAssignment::Sink)
      threads.emplace_back(SinkRunner, result(nResults - 1),
         std::ref(mChannels.back()), cref(mSink));

   // May do one of the tasks on this calling thread
   switch (assignment) {
   case TaskAssignment::Source:
      SourceRunner(result(0), mSource, mChannels[0]);
      break;
   case TaskAssignment::Sink:
      SinkRunner(result(nResults - 1), mChannels.back(), mSink);
      break;
   default:
      break;
   }

   // Finish all work
   for (auto &thread : threads)
      thread.join();

   return statuses;
}

bool TypeErasedPipeline::SourceRunner(Result result,
   const Source &source, Channel &output) noexcept
{
   bool terminated = false;
   auto timeUpdater = MakeTimeUpdater(result.timing.allTime);
   static_assert(!wxSEMA_NO_ERROR); // wxSemaphore's good return is false
   auto errorFree = wxSEMA_NO_ERROR;
   auto errorOut = wxSEMA_NO_ERROR;
   auto pOutSlot = output.slots + output.empty.index;
   Status *pLastStatus = &output.slots[1 - output.empty.index].second;
   try {
      // Wait for vacancy from downstream; check that the buffer wasn't
      // destroyed (no downstream task caused early termination); and that
      // source doesn't destroy it (indicating normal termination)
      // (The argument to source only allows recycling of storage; contents
      // of buffers are not significant)
      while ( !(errorFree = output.empty.semaphore.Wait()) &&
         pOutSlot->first.has_value()
      ) {
         auto &outSlot = *pOutSlot;
         Callable::OverloadSet visitor{
            [&](Done){
               // If called from StageRunner, report that the stage completed
               // normally
               terminated = true;
               // Send one empty message to downstream informing it of normal
               // termination
               outSlot = { {}, {} };
            },
            [&](std::pair<Data, Status> &&output){
               // Note that the status and data types need to be
               // nothrow_move_assignable
               outSlot = { std::move(output.first), std::move(output.second) };
               pLastStatus = &outSlot.second;
            },
            [](More){
               // Sources and flushing Stages are not allowed to ask more
               // from upstream
               assert(false);
            },
         };
         Variant::Visit(visitor,
            (++result.timing.passes,
            MakeTimeUpdater(result.timing.functionTime),
            source(&*outSlot.first)));
         // Switch buffers in the channel; maybe next loop pass can progress
         // with no wait if downstream finished fast with the result of a
         // previous pass
         pOutSlot = output.slots + output.empty.Next();
         // Type-erasing layer will guarantee this:
         assert(pLastStatus->has_value());
         if ((errorOut = output.full.semaphore.Post()))
            break;
         if (terminated)
            break;
      }
      result.status = std::move(*pLastStatus);
   }
   catch (...) {
      // Abnormal termination in case source threw an exception, which is
      // stored for rethrow; also give downstream the stop signal (nullopt data)
      result.exception = std::current_exception();
      if (!errorOut) {
         *pOutSlot = { {}, {} };
         errorOut = output.full.semaphore.Post();
      }
   }
   if (!result.exception && (errorFree || errorOut))
      result.exception = std::make_exception_ptr(BadSemaphore{});
   return terminated;
}

void TypeErasedPipeline::StageRunner(Result result,
   Channel &input, const Stage &stage, Channel &output) noexcept
{
   // This function is like a combination of SourceRunner and SinkRunner.
   // See their comments.
   // But there is also the complication that it can request more iterations
   // of the upstream before passing data downstream.
   auto timeUpdater = MakeTimeUpdater(result.timing.allTime);
   static_assert(!wxSEMA_NO_ERROR); // wxSemaphore's good return is false
   auto errorIn = wxSEMA_NO_ERROR;
   auto errorFree = wxSEMA_NO_ERROR;
   auto errorDone = wxSEMA_NO_ERROR;
   auto errorOut = wxSEMA_NO_ERROR;
   auto pInSlot = input.slots + input.full.index;
   auto pOutSlot = output.slots + output.empty.index;
   Status *pLastStatus = &output.slots[1 - output.empty.index].second;
   try {
      bool needMore = false;
      while ( !(errorIn = input.full.semaphore.Wait())
      && (needMore || !(errorFree = output.empty.semaphore.Wait()))
      && pInSlot->first.has_value()
      && pOutSlot->first.has_value()
      ) {
         auto &outSlot = *pOutSlot;
         Callable::OverloadSet visitor{
            [&](Done){
               // Requirement on stages: don't indicate end-of-stream before the
               // upstream does
               assert(false);
               outSlot = { {}, {} };
               needMore = false;
            },
            [&](std::pair<Data, Status> &&output){
               // Note that the status and data types need to be
               // nothrow_move_assignable
               outSlot = { std::move(output.first), std::move(output.second) };
               pLastStatus = &outSlot.second;
               needMore = false;
            },
            [&](More){ needMore = true; },
         };
         Variant::Visit(visitor,
            (++result.timing.passes,
            MakeTimeUpdater(result.timing.functionTime),
            stage(&*pInSlot->first, &*outSlot.first)));
         pInSlot = input.slots + input.full.Next();
         if ((errorDone = input.empty.semaphore.Post()))
            break;
         if (needMore)
            continue;

         // Switch buffers in the channel; maybe next loop pass can progress
         // with no wait if downstream finished fast with the result of a
         // previous pass
         pOutSlot = output.slots + output.empty.Next();

         if ((errorOut = output.full.semaphore.Post()))
            break;
      }
      bool terminated = false;
      if (pOutSlot->first.has_value()) {
         if (!(errorIn || errorFree || errorDone || errorOut)) {
            // deduced from negation of while condition conjoined with
            // the break conditions:
            assert(!pInSlot->first.has_value());
            // Normal termination of the upstream.  Stage can now act like a
            // source for one or more iterations.
            using namespace std::placeholders;
            terminated =
               SourceRunner(result, std::bind(stage, nullptr, _1), output);
         }
      }
      else
         result.status = std::move(*pLastStatus);
      if (!terminated) {
         // If downstream abnormally destroyed the output buffer already,
         // we must communicate that to upstream
         if (!errorDone) {
            *pInSlot = { {}, {} };
            errorDone = input.empty.semaphore.Post();
         }
      }
   }
   catch (...) {
      // Communicate broken pipe to upstream
      if (!errorDone) {
         *pInSlot = { {}, {} };
         errorDone = input.empty.semaphore.Post();
      }
      // And to downstream
      if (!errorOut) {
         *pOutSlot = { {}, {} };
         errorOut = output.full.semaphore.Post();
      }
      result.exception = std::current_exception();
   }
   if (!result.exception && (errorFree || errorIn || errorOut || errorDone ))
      result.exception = std::make_exception_ptr(BadSemaphore{});
}

void TypeErasedPipeline::SinkRunner(
   Result result, Channel &input, const Sink &sink) noexcept
{
   auto timeUpdater = MakeTimeUpdater(result.timing.allTime);
   static_assert(!wxSEMA_NO_ERROR); // wxSemaphore's good return is false
   auto errorIn = wxSEMA_NO_ERROR;
   auto errorDone = wxSEMA_NO_ERROR;
   auto pInSlot = input.slots + input.full.index;
   try {
      // Wait for data from upstream; check that the buffer isn't destroyed
      while ( !(errorIn = input.full.semaphore.Wait()) &&
         pInSlot->first.has_value()
      ) {
         // Consume values, then tell upstream it can refill the buffer
         result.status = (++result.timing.passes,
            MakeTimeUpdater(result.timing.functionTime),
            sink(&*pInSlot->first));
         if ((errorDone = input.empty.semaphore.Post()))
             break;
         // Switch buffers in the channel; maybe next loop pass can progress
         // with no wait if upstream refilled fast after a previous pass of the
         // sink
         pInSlot = input.slots + input.full.Next();
      }
      if (!(errorIn || errorDone)) {
         // Normal termination after upstream runs out.
         // Last call to sink passing nullptr lets it do a final flushing action
         result.status = (++result.timing.passes,
            MakeTimeUpdater(result.timing.functionTime),
            sink(nullptr));
      }
   }
   catch (...) {
      // Abnormal termination in case sink threw an exception, which is
      // stored for rethrow; also give upstream a stop signal (nullopt data)
      // by destroying the buffer
      if (!errorDone) {
         *pInSlot = { {}, {} };
         errorDone = input.empty.semaphore.Post();
      }
      result.exception = std::current_exception();
   }
   if (!result.exception && (errorIn || errorDone))
      result.exception = std::make_exception_ptr(BadSemaphore{});
}

} // namespace detail
} // namespace TaskParallel
