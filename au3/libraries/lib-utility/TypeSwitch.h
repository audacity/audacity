/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file TypeSwitch.h
  @brief Dispatch to one of a set of functions by the run-time type of an object

  Paul Licameli

**********************************************************************/
#ifndef __AUDACITY_TYPE_SWITCH__
#define __AUDACITY_TYPE_SWITCH__

#include "Callable.h"
#include "Tuple.h"
#include "TypeList.h"
#include <cassert>

namespace TypeSwitch {
using namespace TypeList;

namespace detail {

template<typename... Functions> using FunctionTupleType =
   std::tuple<const Functions &...>;
template<typename Functions> using FunctionTuple =
   Apply_t<FunctionTupleType, Functions>;

//! Metafunction implementing TypeSwitch
/*!
 @tparam ArgumentTypes nonempty; more derived types later
 @tparam Funs those applicable to more derived types are earlier
 */
template<typename R, typename ArgumentTypes, typename Funs, typename... Args>
struct Executor {
   static_assert(!Null_v<ArgumentTypes>);
   using ArgumentType = Head_t<ArgumentTypes>;

   struct NoOp {
      struct type {
         using Functions = FunctionTuple<Nil>;

         //! Constant used in a compile-time check
         enum : unsigned { SetUsed = 0 };
         //! No functions matched, so do nothing
         R operator ()(ArgumentType &, const Functions &, Args&&...) const {
            if constexpr (std::is_void_v<R>)
               return;
            else
               return R{};
         }
      };
   };

   template<typename Fs, typename Wrapped> struct Combine {
      using Functions = FunctionTuple<Fs>;
      struct Transparent {
         //! No BaseClass of ArgumentType is acceptable to the first function
         struct type {
            using Next = typename Wrapped::type;

            //! Constant used in a compile-time check
            enum : unsigned { SetUsed = Next::SetUsed << 1 };

            //! Ignore the first, inapplicable function and try others.
            R operator ()(
               ArgumentType &object, const Functions &functions, Args&&... args)
            const {
               return Next{}(object, Tuple::ForwardNext(functions),
                  std::forward<Args>(args)...);
            }
         };
      };

      template<typename BaseClass, typename NextBase> struct CombineOp {
         static_assert(
            std::is_const_v<BaseClass> == std::is_const_v<ArgumentType>);
         //! Whether upcast of ArgumentType* to BaseClass* works
         using Compatible = std::is_base_of<BaseClass, ArgumentType>;

         //! Generates operator () that calls one function only, shadowing
         //! those taking less derived base classes
         struct Opaque {
            //! Constant used in a compile-time check
            enum : unsigned { SetUsed = 1u };

            //! Ignore the remaining functions and call the first only.
            R operator ()(
               BaseClass &object, const Functions &functions, Args&&... args)
            const {
               return std::get<0>(functions)(
                  object, std::forward<Args>(args)...);
            }
         };

         //! Generates operator () that calls a function that accepts a next
         //! function
         struct Wrapper {
            using Next = typename Wrapped::type;
            //! Constant used in a compile-time check
            enum : unsigned { SetUsed = (Next::SetUsed << 1) | 1u };

            //! Call the first function, which may request dispatch to the
            //! further functions
            R operator ()(
               BaseClass &object, const Functions &functions, Args&&... args)
            const {
               // The first function in the tuple is curried!
               // Its first argument is the call-through and its second
               // is the object
               const auto next = [&](){ return
                  Next{}(object, Tuple::ForwardNext(functions),
                     std::forward<Args>(args)...); };
               return std::get<0>(functions)(next)(
                  object, std::forward<Args>(args)...);
            }
         };

         using F = Head_t<Fs>;

         // whether Function looks like a generic callable
         struct Dummy { R operator ()() const {
            if constexpr(std::is_void_v<R>) return; else return R{};
         } };
         using curried = std::is_invocable<F, Dummy&&>;

         // Case 1: Compatible, and invocable on the next function, giving
         // another function, that accepts BaseClass:
         struct Case1_;
         using Case1 = std::conjunction<Compatible, curried, Case1_>;
         struct Case1_ {
            static constexpr bool value = std::is_invocable_v<
               std::invoke_result_t<F, Dummy &&>, BaseClass&, Args&&...>;
            using type = Wrapper;
         };

         // Case 2: Invocable directly on the object
         struct Case2 : std::conjunction<
            Compatible, std::negation<curried>,
            std::is_invocable<F, BaseClass&, Args&&...>
         > {
            using type = Opaque;
         };
         struct Default : std::true_type {
            using type = typename NextBase::type;
         };
         using type = typename std::disjunction<Case1, Case2, Default>::type;
      };
      using type =
         typename LeftFold_t<CombineOp, ArgumentTypes, Transparent>::type;
   };

   using type = typename RightFoldList_t<Combine, Funs, NoOp>::type;
};

//! Synthesize a function appropriate for ArgumentType
/*!
 @tparam ArgumentTypes nonempty; more derived types later
 @tparam Functions those applicable to more derived types are earlier
 */
template<
   typename R, typename ArgumentTypes, typename Functions, typename... Args>
using Executor_t =
   typename Executor<R, ArgumentTypes, Functions, Args...>::type;

// Executors for more derived classes are later in the given type list
template<typename R, typename Exec, typename ObjectTypes>
struct Invoker {
private:
   struct Base {
      template<typename Object, typename Functions, typename... Args>
      R operator ()(Object &object, const Functions &functions, Args&&...)
      const {
         // This should never be reached at run-time, because an Executor
         // generated for (const) Object should have been the catch-all.
         assert(false);
         if constexpr (std::is_void_v<R>)
            return;
         else
            return R{};
      }
   };
   template<typename ObjectType, typename Recur> struct Op {
      template<typename Object, typename Functions, typename... Args>
      R operator ()(Object &object, const Functions &functions, Args&&... args)
      const {
         // Dynamic type test of object
         if (const auto pObject = dynamic_cast<ObjectType*>(&object))
            // Dispatch to an Executor that knows which of functions applies
            return Exec{}(*pObject, functions, std::forward<Args>(args)...);
         else
            // Recur, with fewer candidate Executors and all of functions
            return Recur{}(object, functions, std::forward<Args>(args)...);
      }
   };
public:
   template<typename Object, typename Functions, typename... Args>
   R operator ()(Object &object, const Functions &functions, Args&&... args)
   const {
      const auto fn = LeftFold_t<Op, ObjectTypes, Base>{};
      return fn(object, functions, std::forward<Args>(args)...);
   }
};

template<typename ...Executors> struct UsedCases {
   constexpr auto operator ()() {
      return std::integral_constant<unsigned, (Executors::SetUsed | ...)>{};
   };
};

template<size_t... Is, typename TupleLike> auto MakeFunctionTuple(
   std::index_sequence<Is...>, const TupleLike &functions) {
   return std::forward_as_tuple(std::get<Is>(functions)...);
}

template<
   typename R,
   typename ObjectTypes,
   typename Functions,
   typename... Args
>
struct TypeSwitcher {
   static_assert(!Null_v<ObjectTypes>);
   using Object = Head_t<ObjectTypes>;
   // Generate Executor classes, for each of ObjectTypes,
   // each zero-sized and with an operator () that calls the correct
   // one of functions, assuming the object is of the corresponding type
   template<typename Tail> using Executor_ =
      Executor_t<R, Tail, Functions, Args...>;
   using Executors = MapList_t<Fn<Executor_>, ObjectTypes>;

   // Compile time reachability check of the given functions
   enum { All = Length_v<Functions>, AllBits = (1u << All) - 1u };
   static_assert(std::is_same_v<
         std::integral_constant<unsigned, AllBits>,
         decltype(Apply_t<UsedCases, Executors>{}())
      >,
      "Uncallable case in TypeSwitch");

   using Exec = Apply_t<Callable::OverloadSet, Executors>;
   template<typename TupleLike> R operator ()(
      Object &object, const TupleLike &functions,
      Args&&... args) const {
      // Do dynamic dispatch to one of the Executors
      return Invoker<R, Exec, ObjectTypes>{}(object,
         MakeFunctionTuple(std::make_index_sequence<All>{}, functions),
         std::forward<Args>(args)...);
   }
};

template<typename TypeList> constexpr bool RootTypeCheck_v =
   Every_v<Bind1st<std::is_base_of, Head_t<TypeList>>, Tail_t<TypeList>>;

template<typename TypeList> using InheritanceCheck =
   NotAny<Bind2nd<std::is_base_of, Head_t<TypeList>>, Tail_t<TypeList>>;

}

template<typename TypeList> constexpr bool TypeListCheck_v =
   std::is_polymorphic_v<Head_t<TypeList>> &&
   detail::RootTypeCheck_v<TypeList> &&
   Every_v<Fn<detail::InheritanceCheck>, NonEmptyTails_t<TypeList>> &&
   (Every_v<Fn<std::is_const>, TypeList> ||
    NotAny_v<Fn<std::is_const>, TypeList>);

/*!
 A variadic function taking any number of function objects, each taking
 - a reference to the first of Types or a subclass of it, plus extra aruments,
   or
 - a first, callable next-function argument, and returning a function, which
   takes a reference to the first of Types or a subclass, plus extra arguments.
   (Typically, a generic lambda returning a lambda.  That is, it's curried.)

 In the first case, the function object returns R or a type convertible to R.

 In the second case, the next-function takes no arguments and returns
 likewise; the partial application of the curried function is then like
 the first case.

 Calls the first in the sequence that accepts the actual type of the object
 (after any needed partial application).

 If none accepts, do nothing and return R{} (or void when R is void).

 If one of the curried functions invokes the no-argument function passed into
 it, the inner call invokes the next applicable function.

 There is a compile-time check that all of the given functions are reachable
 (that is, all have correct signatures, and no function earlier in the list
 shadows a later function, by accepting a less derived class, without partial
 application).

 A single Callable::OverloadSet might be used instead of this dispatch, when the
 next-functions are not used, but that would lose the advantage of the compile
 time checks.

 @tparam R returned by this function; each given function must return a type
    convertible to R
 @tparam Types a non-empty TypeList; the first is polymorphic and a proper base
    of all other types; no type is a subclass of any type left of it;
    and either all are const-qualified, or none are
 @tparam TupleLike deduced from argument
 @tparam Args... extra arguments, passed to the functions
 @param object determines the function to call by its run-time type
 @param functions typically lambdas; see above
 */
template<
   typename R = void,
   typename Types,
   class TupleLike,
   typename... Args
>
auto Dispatch(Head_t<Types> &object, const TupleLike &functions,
   Args&&... args)
   -> std::enable_if_t<TypeListCheck_v<Types>, R>
{
   // Generate a function that dispatches dynamically on track type
   return detail::TypeSwitcher<R, Types, Bind_t<TupleLike>, Args...>{}(
      object, functions, std::forward<Args>(args)...);
}

//! @copydoc Dispatch
/*!
 Variadic overload of Dispatch; doesn't accept extra arguments
 */
template<
   typename R = void,
   typename Types,
   typename ...Functions
>
auto VDispatch(Head_t<Types> &object, const Functions &...functions)
{
   return Dispatch<R, Types>(object, std::forward_as_tuple(functions...));
}

}

#endif
