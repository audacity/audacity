/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file Pipeline.h
 
 Paul Licameli
 
 **********************************************************************/
#ifndef __AUDACITY_PIPELINE__
#define __AUDACITY_PIPELINE__

#include <any>
#include <cassert>
#include <chrono>
#include <exception>
#include <functional>
#include <utility>
#include <variant>
#include <vector>
#include <wx/string.h>

namespace TaskParallel {

//! Specifies which if any of a Pipeline's tasks should execute on the calling
//! thread of Pipeline::Run()
enum class TaskAssignment : char {
   None = 0,
   Source,
   Sink,
};

//! Default type for stages that return only data and don't record status
struct EmptyStatus{};

//! Variant alternative indicating that a stage has no more data to produce
struct Done{};

//! Variant alternative indicating that a stage requests more data from upstream
//! before producing
struct More{};

template<typename Data, typename Status = EmptyStatus>
struct DataAndStatus : std::pair<Data, Status> {
   DataAndStatus(Data &&data, Status &&status = {})
      : std::pair<Data, Status>{ std::move(data), std::move(status) }
   {}
};

template<typename Data, typename Status = EmptyStatus>
using StageResult = std::variant<Done, DataAndStatus<Data, Status>, More>;

namespace detail {

//! type-erased value communicated from source or stage, to stage or sink; may
//! have different types at different joints of the pipeline
using Data = std::any;
//! type-erased status code of the last call to a source or stage or sink; may
//! have different types for different steps
using Status = std::any;
using Initializer = std::function<std::pair<Data, Status>()>;
using Input =  Data *;
using Output = StageResult<Data, Status>;
//! Source can recycle a data buffer of the same type as its output Data
using Source = std::function<Output(Data*)>;
//! Stage can recycle like Source; Input is null when Stage is being flushed
using Stage =  std::function<Output(Input, Data*)>;
//! Input is null when Sink is being flushed
using Sink =   std::function<Status(Input)>;

//! Defines the machinery of Pipeline non-inline
class CONCURRENCY_API TypeErasedPipeline
{
   struct Timing {
      std::chrono::nanoseconds allTime{};
      std::chrono::nanoseconds functionTime{};
      unsigned passes{};
   };
public:
   TypeErasedPipeline(Source source, std::vector<Stage> stages, Sink sink,
      const wxString &name);
   ~TypeErasedPipeline();
   size_t NStages() const { return mStages.size(); }

   TypeErasedPipeline(const TypeErasedPipeline&) = delete;
   TypeErasedPipeline& operator =(const TypeErasedPipeline&) = delete;

   /*!
    @pre `index <= NStages()`
    */
   void Initialize(size_t index, const Initializer &initializer);
   bool Initialized() const;

   std::vector<Status> TimedRun(TaskAssignment assignment);
   std::vector<Status> Run(TaskAssignment assignment,
      std::vector<std::exception_ptr> &exceptions,
      std::vector<Timing> &timings);

private:
   struct Channel;
   struct Result{
      Status &status; std::exception_ptr &exception; Timing &timing; };
   static bool SourceRunner(Result result,
      const Source &source, Channel &output) noexcept;
   static void StageRunner(Result result,
      Channel &input, const Stage &stage, Channel &output) noexcept;
   static void SinkRunner(Result result,
      Channel &input, const Sink &sink) noexcept;

   const Source mSource;
   const std::vector<Stage> mStages;
   const Sink mSink;
   std::vector<Channel> mChannels;
   const wxString mName;
   bool mOk;
};

} // namespace detail

} // namespace TaskParallel

#endif
