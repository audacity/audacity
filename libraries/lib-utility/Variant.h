/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file Variant.h

 @brief emulates std::visit for one visitor
 
 Paul Licameli split from MemoryX.h
 
 **********************************************************************/
#ifndef __AUDACITY_VARIANT__
#define __AUDACITY_VARIANT__

#include <functional>
#include <type_traits>
#include <utility>
#include <variant>
#include <stdexcept>

namespace Variant {

namespace detail {
//! Help to define Visit() below
template <typename Visitor, typename Variant>
struct VisitHelperReturn {
   using Var = std::remove_reference_t<Variant>;
   using Alt = std::variant_alternative_t<0, Var>;
   using QAlt = std::conditional_t<
      std::is_const_v<Var>, const Alt, Alt >;
   using Arg = std::conditional_t<std::is_lvalue_reference_v<Variant>,
      std::add_lvalue_reference_t<QAlt>, std::add_rvalue_reference_t<QAlt>
   >;
   // All this just so that the noreturn function below has an appropriate type
   using type = decltype( std::invoke(
     std::forward<Visitor>( std::declval<Visitor>() ),
     std::declval<Arg>() ) );
};

//! Help to define Visit() below
template <typename Visitor, typename Variant>
[[noreturn]] auto VisitHelper(Visitor &&, Variant &&)
   -> typename VisitHelperReturn<Visitor, Variant>::type
{
   // Fall through here when the variant holds no value
   // Should really throw std::bad_variant_access but that may not be available
   throw std::invalid_argument{"Bad variant"};
}

//! Help to define Visit() below
template <size_t Index, size_t... Indices, typename Visitor, typename Variant>
auto VisitHelper(Visitor &&vis, Variant &&var)
{
   // Invoke vis at most once after no-throw testing for presence of
   // alternatives in the variant
   if (const auto pValue = std::get_if<Index>(&var)) {
      if constexpr (std::is_lvalue_reference_v<Variant>)
         return std::invoke( std::forward<Visitor>(vis), (*pValue) );
      else
         return std::invoke( std::forward<Visitor>(vis), std::move(*pValue) );
   }
   // Recur down the index value pack
   return VisitHelper<Indices...>(
      std::forward<Visitor>(vis), std::forward<Variant>(var));
}

//! Help to define Visit() below
template <size_t... Indices, typename Visitor, typename Variant>
auto VisitHelper(std::index_sequence<Indices...>, Visitor &&vis, Variant &&var)
{
   // Non-template parameters were deduced and are passed on as non-deduced
   return VisitHelper<Indices...>(
      std::forward<Visitor>(vis), std::forward<Variant>(var) );
}
}

//! Mimic some of std::visit, for the case of one visitor only
/*! This is necessary because of limitations of the macOS implementation of
 some of the C++17 standard library without a minimum version of 10.13, and
 so let's use this even when not needed on the other platforms, instead of having
 too much conditional compilation
 */
template <typename Visitor, typename Variant>
auto Visit(Visitor &&vis, Variant &&var)
{
   constexpr auto size = std::variant_size_v<std::remove_reference_t<Variant>>;
   return detail::VisitHelper( std::make_index_sequence<size>{},
      std::forward<Visitor>(vis), std::forward<Variant>(var) );
}

namespace detail {
//! Standard in C++20
template<typename T> struct type_identity{ using type = T; };

template<typename T> struct to_std_function{
   using type = decltype(std::function{ std::declval<T>() });
};

//! Capture pointer to member
template<typename M, typename C> struct MemberInvoker {
   using Member = M C::*;
   template<typename T> using Result = std::invoke_result_t<Member, T>;
   template<typename, typename = void> struct CanAccept : std::false_type {};
   template<typename T> struct CanAccept<T, std::void_t<Result<T>>>
      : std::true_type {};

   explicit MemberInvoker(Member member) : member{ member } {}
   //! Cover all cases of std::invoke, with perfect forwarding, and
   //! sfinae eliminates the overloads for argument types for which it
   //! is inapplicable
   template<typename Obj> auto operator() (Obj&& obj) const
      -> std::enable_if_t<CanAccept<Obj&&>::value, Result<Obj&&>>
   {
      return std::invoke(member, std::forward<Obj>(obj));
   }
   Member member;
};

//! Capture any invocable as a class, using std::function only when needed
template<typename Invocable> struct InvocableBase {
   using type = typename std::conditional_t<std::is_class_v<Invocable>,
      type_identity<Invocable>,
      to_std_function<Invocable>
   >::type;
};
template<typename Invocable> using InvocableBase_t =
   typename InvocableBase<Invocable>::type;
//! partial specialization for pointers to members because CTAD fails for
//! std::function
template<typename M, typename C> struct InvocableBase<M C::*> {
   using type = MemberInvoker<M, C>;
};

template<typename... Invocables> struct VisitorBase
   : detail::InvocableBase_t<Invocables>...
{
   //! Variadic constructor allowing arguments with different value categories
   template<typename... Is>
   VisitorBase(Is&&... invocables)
      : detail::InvocableBase_t<Invocables>{ std::forward<Is>(invocables) }...
   {}
   using detail::InvocableBase_t<Invocables>::operator() ...;
};
}

//! Construct from multiple invocables to get overloaded operator ()
template<typename... Invocables> struct Visitor
   : detail::VisitorBase<std::remove_reference_t<Invocables>...>
{
   using
      detail::VisitorBase<std::remove_reference_t<Invocables>...>::VisitorBase;
};
template<typename... Is> Visitor(Is&&... invocables) -> Visitor<Is&&...>;

}

#endif
