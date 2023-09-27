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
#include <optional>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>
#include <wx/string.h>
#include "Callable.h"
#include "TypedAny.h"
#include "TypeList.h"
#include "Variant.h"

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
struct Data : audacity::TypedAny<Data>{ using TypedAny::TypedAny; };
//! type-erased status code of the last call to a source or stage or sink; may
//! have different types for different steps
struct Status : audacity::TypedAny<Status>{ using TypedAny::TypedAny; };
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

//! Some deduction helpers

// Standard in C++20
template<typename T> struct type_identity { using type = T; };

template<typename T> struct is_std_function : std::false_type{};
template<typename T> struct is_std_function<std::function<T>>
   : std::true_type{};
template<typename T> constexpr bool is_std_function_v =
   is_std_function<T>::value;

template<typename Tuple, typename Type> using push_front_t =
   decltype(std::tuple_cat(
      std::declval<std::tuple<Type>>(), std::declval<Tuple>()
   ));
template<typename Tuple, typename Type> using push_back_t =
   decltype(std::tuple_cat(
      std::declval<Tuple>(), std::declval<std::tuple<Type>>()
   ));

// Destructures the type returned by a Source or Stage
template<typename> struct data_type;
template<typename Data, typename Status>
struct data_type<StageResult<Data, Status>> {
   using type = Data;
};
template<typename T> using data_type_t = typename data_type<T>::type;

// Destructures the type returned by a Source or Stage
template<typename> struct status_type;
template<typename Data, typename Status>
struct status_type<StageResult<Data, Status>> {
   using type = Status;
};
template<typename T> using status_type_t = typename status_type<T>::type;

// Deduces the type returned by Source
template<typename Source> using FirstResult_t =
   std::invoke_result_t<Source, nullptr_t>;

// Deduces the type returned by Stage
template <typename Input, typename Stage> using NextResult_t =
   std::invoke_result_t<Stage, Input*, nullptr_t>;

// Deduces the status returned by Sink, which may be void
template <typename Input, typename Sink> using LastReturn_t =
   std::invoke_result_t<Sink, Input *>;

using namespace TypeList;

// Deduces the sequence of input, output, and status types in a pipeline
struct StageTypes {
   template<typename Input> struct Init {
      using input = Input;
      using inputs = Nil;
      using outputs = Nil;
      using statuses = Nil;
      using initValues = Nil;
   };
   template <typename Stage, typename Acc>
   struct Op {
      using Input = typename Acc::input;
      using Next = NextResult_t<Input, Stage>;
      using Data = data_type_t<Next>;
      using Status = status_type_t<Next>;
      struct type {
         using input = Data;
         using inputs = Cons_t<Input, typename Acc::inputs>;
         using outputs = Cons_t<Data, typename Acc::outputs>;
         using statuses = Cons_t<Status, typename Acc::statuses>;
         using initValues =
            Cons_t<std::pair<Data, Status>, typename Acc::initValues>;
      };
   };
   template <typename Stage, typename Acc>
   using Op_t = typename Op<Stage, Acc>::type;
};
template<typename Input, typename Stages> using StageTypes_t = RightFold_t<
   StageTypes::Op_t, Stages, StageTypes::Init<Input>>;

//! Adapt function as a type-erased Initializer
/*!
 @tparam InitValues is a std::pair specialization that must be supplied,
 cannot be deduced

 The given function may return a pair of data and a status code,
 or return only the data.  The function needs only to return values convertible
 to the appropriate types, not necessarily of those exact types.
 */
template<typename InitValues, typename TypedInitializer>
static auto AdaptInitializer(TypedInitializer initializer)
{
   using DataType = typename InitValues::first_type;
   using StatusType = typename InitValues::second_type;
   auto defaultFunction = []{
      return std::pair{ Data{ DataType{} }, Status{ StatusType{} } };
   };

   if constexpr (std::is_null_pointer_v<TypedInitializer>)
      // Adapt nullptr
      return defaultFunction;
   else {
      if constexpr (is_std_function_v<TypedInitializer>) {
         if (!initializer)
            // Adapt an empty std::function
            return defaultFunction;
      }
      // Adapt other cases, including lambdas
      if constexpr(std::is_same_v<StatusType, EmptyStatus>)
         return [initializer = std::move(initializer)] {
            DataType data = initializer();
            return std::pair{ Data{ std::move(data) }, Status{ EmptyStatus{} } };
         };
      else
         return [initializer = std::move(initializer)] {
            InitValues pair = initializer();
            auto &[data, status] = pair;
            return std::pair{
               Data{ std::move(data) }, Status{ std::move(status) } };
         };
   }
}

//! Adapt function as a type-erased Source
template<typename TypedSource> Source AdaptSource(TypedSource source)
{
   return [source = std::move(source)] (Data *p) -> Output {
      using TypedOutput = FirstResult_t<TypedSource>;
      using TypedData = data_type_t<TypedOutput>;
      using TypedStatus = status_type_t<TypedOutput>;
      assert(p); // TypeErasedPipeline guarantees this
      Callable::OverloadSet visitor{
         [](Done) -> Output { return Done{}; },
         [](std::pair<TypedData, TypedStatus> &&output) -> Output {
            return DataAndStatus{
               Data{ std::move(output.first) },
               Status{ std::move(output.second) } }; },
         [](More) -> Output { return More{}; },
      };
      return Variant::Visit(visitor, source(p->cast<TypedData>()));
   };
}

//! Adapt function as a type-erased Stage
/*!
 @tparam TypedInput must be supplied, cannot be deduced
 */
template<typename TypedInput, typename TypedStage>
Stage AdaptStage(TypedStage stage)
{
   return [stage = std::move(stage)] (Input input, Data *p) -> Output {
      using TypedOutput = NextResult_t<TypedInput, TypedStage>;
      using TypedData = data_type_t<TypedOutput>;
      using TypedStatus = status_type_t<TypedOutput>;
      assert(p); // TypeErasedPipeline guarantees this
      Callable::OverloadSet visitor{
         [](Done) -> Output { return Done{}; },
         [](std::pair<TypedData, TypedStatus> &&output) -> Output {
            return DataAndStatus{
               Data{ std::move(output.first) },
               Status{ std::move(output.second) } }; },
         [](More) -> Output { return More{}; },
      };
      const auto pTypedInput =
         input ? input->cast<TypedInput>() : nullptr;
      const auto pTypedData = p->cast<TypedData>();
      return Variant::Visit(visitor, stage(pTypedInput, pTypedData));
   };
}

//! Adapt function as a type-erased Sink
/*!
 @tparam TypedInput must be supplied, cannot be deduced
*/
template<typename TypedInput, typename TypedSink>
auto AdaptSink(TypedSink sink) -> Sink
{
   return [sink = std::move(sink)] (Input input) -> Status {
      using Answer = LastReturn_t<TypedInput, TypedSink>;
      const auto pTypedInput = input ? input->cast<TypedInput>() : nullptr;
      if constexpr (std::is_void_v<Answer>) {
         sink(pTypedInput);
         return Status{ EmptyStatus{} };
      }
      else
         return Status{ sink(pTypedInput) };
   };
}

} // namespace detail

//! Performs pipelined computations specified by function arguments given to the
//! constructor, each running in a loop in its own thread; checks function
//! signatures for compatibility
template<typename Source, typename Sink, typename... Stages>
class Pipeline : detail::TypeErasedPipeline
{
   using First = detail::FirstResult_t<Source>;
   using Data0 = detail::data_type_t<First>;
   using Status0 = detail::status_type_t<First>;
   using InitValues0 = std::pair<Data0, Status0>;
   using Types = detail::StageTypes_t<Data0, TypeList::List<Stages...>>;
   using Inputs = typename Types::inputs;
   using Outputs = typename Types::outputs;
   using Statuses = typename Types::statuses;
   using Initvalues = typename Types::initValues;
   using LastOutput = typename std::conditional_t<
      sizeof...(Stages) == 0,
      detail::type_identity<Data0>,
      TypeList::Last<Outputs>
   >::type;
   using LastReturn = detail::LastReturn_t<LastOutput, Sink>;
   using LastStatus = std::conditional_t<
      std::is_void_v<LastReturn>, EmptyStatus, LastReturn>;
   using AlmostAllStatuses = TypeList::Cons_t<Status0, Statuses>;
   using AllStatuses = TypeList::PushBack_t<AlmostAllStatuses, LastStatus>;

   using StatusVector = std::vector<detail::Status>;
   using StatusIter = StatusVector::const_iterator;
   struct StatusCollector {
      struct Init {
         auto operator ()(StatusIter it) { return std::tuple{}; } };
      template<typename Status, typename Acc> struct Op {
         auto operator ()(StatusIter it) {
            auto rest = Acc{}(it + 1);
            if constexpr (std::is_same_v<Status, EmptyStatus>)
               return rest;
            else
               // Assume `it` is in bounds;
               // assume the std::any there has the right type
               return std::tuple_cat(
                  std::tuple{ std::move(*it->cast<Status>()) },
                  move(rest));
         }
      };
      template<typename Statuses>
      static auto Collect(const StatusVector &statuses) {
         using namespace TypeList;
         using Collector = RightFold_t<Op, Statuses, Init>;
         return Collector{}(statuses.begin());
      }
   };
   using Result = decltype(StatusCollector::template Collect<AllStatuses>(
      std::declval<const StatusVector &>()));
   static constexpr auto Indices = std::index_sequence_for<Stages...>{};

public:
   //! Construct a pipeline from functions, often lambdas.
   /*!
    @param source returns StageResult<D, S> for some types D and S
    If the result is Done{}, source is not called again in the current run.
    Result MUST NOT be More{}.
    Its argument is a non-null pointer to D whose contents are no longer needed
    and which may be moved to produce the new output.
    D and S must be nothrow_move_assignable.

    @param stages each returns like source, takes a second argument like
    source's only argument, and takes a first argument, which is a pointer to
    a value of the D type produced by the previous stage or the source.
    When the argument is null, the upstream is finished, and the stage is called
    repeatedly with null until it returns Done{}.
    If the first argument is null, the result MUST NOT be More{}.
    If the first argument is not null, the result MUST NOT be Done{}.
    Return is interpreted as for source; More{} status is a request for the next
    input before production of output.

    @param sink takes a first argument like each of stages, and may return void
    or a status code of a nothrow_move_assignable type.

    @param name used only in logging messages about timing
    */
   Pipeline(Source source, std::tuple<Stages...> stages, Sink sink,
      const wxString &name = {})
      : detail::TypeErasedPipeline{
         detail::AdaptSource(std::move(source)),
         TypeList::Apply_t<StageAdapter, Inputs>{}(move(stages), Indices),
         detail::AdaptSink<LastOutput>(std::move(sink)), name
      }
   {}

   //! Run the pipeline.
   //! Requires all value and status types to be default-constructible.
   std::optional<Result> Run(TaskAssignment assignment = TaskAssignment::None)
   {
      // Just pass the correct number of nullptrs to the other override
      return Run(assignment, nullptr, Null<Stages>{}...);
   }

   //! Run the pipeline.
   /*
    Leaves the Pipeline in a reusable state after return or exception.

    @param assignment may specify source, sink, or neither to run on the same
    thread as the call to Run.  (This might be the UI thread and the function
    might be able to run only on that thread.)

    @param initializer0 returns values convertible to a pair of data and status
    types of source, or can be nullptr
    @param initializers return values convertible to pairs of data and status
    types of corresponding stages, or any one can be a nullptr

    @throws ex If any of the functions given to the constructor throws an
    exception, rethrows the one from the function earliest in the sequence of
    constructor arguments.
    (TODO: use std::throw_with_nested to rethrow all?)

    @return nullopt if system errors prevented the pipeline from running to
    completion, otherwise contains the tuple of all the status codes returned by
    the last calls to each of the functions.  This deduced tuple type may
    have fewer members than there are functions if some functions return no
    status values.
    */
   template<typename Initializer0, typename... Initializers>
   std::optional<Result> Run(TaskAssignment assignment,
      const Initializer0 &initializer0, const Initializers &...initializers)
   {
      Initialize(0, detail::AdaptInitializer<InitValues0>(initializer0));
      TypeList::Apply_t<StageInitializer, Initvalues>{}(
         *this, Indices, initializers...);
      // Successful run will return a vector of at least two elements
      if (auto answer = TypeErasedPipeline::TimedRun(assignment)
          ; answer.empty())
         return {};
      else
         return { StatusCollector::template Collect<AllStatuses>(answer) };
   }

private:
   // a type alias that ignores its parameter
   template<typename Stage> using Null = nullptr_t;
   template<typename... Inputs> struct StageAdapter {
      template<size_t... Indices> std::vector<detail::Stage> operator()(
         std::tuple<Stages...> &&stages, std::index_sequence<Indices...>)
      {
         return {
            detail::AdaptStage<Inputs>(std::get<Indices>(std::move(stages)))...
         };
      };
   };
   template<typename... InitValues> struct StageInitializer {
      template<size_t... Indices, typename... Initializers> void operator ()(
         TypeErasedPipeline &base, std::index_sequence<Indices...>,
         const Initializers &...initializers)
      {
         (..., base.Initialize(1 + Indices,
            detail::AdaptInitializer<InitValues>(initializers)));
      }
   };
};
} // namespace TaskParallel

#endif
