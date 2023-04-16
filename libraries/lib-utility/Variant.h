/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file Variant.h

 @brief emulates std::visit for one visitor
 
 Paul Licameli split from MemoryX.h
 
 **********************************************************************/
#ifndef __AUDACITY_VARIANT__
#define __AUDACITY_VARIANT__

#include <array>
#include <cassert>
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
   using type =
      decltype(std::invoke(std::declval<Visitor&&>(), std::declval<Arg>()));
};

//! Help to define Visit() below
template <typename Visitor, typename Variant>
[[noreturn]] auto VisitHelperBad(Visitor &&, Variant &&)
   -> typename VisitHelperReturn<Visitor&&, Variant&&>::type
{
   // Fall through here when the variant holds no value
   // Should really throw std::bad_variant_access but that may not be available
   throw std::invalid_argument{"Bad variant"};
}

//! Help to define Visit() below
template <size_t Index, typename Visitor, typename Variant>
decltype(auto) VisitHelperFunction(Visitor &&vis, Variant &&var)
{
   // Invoke vis at most once
   const auto pValue = std::get_if<Index>(&var);
   // Trust VisitHelper to dispatch correctly
   assert(pValue);
   if constexpr (std::is_lvalue_reference_v<Variant>)
      return std::invoke(std::forward<Visitor>(vis), (*pValue));
   else
      return std::invoke(std::forward<Visitor>(vis), std::move(*pValue));
}

//! Help to define Visit() below
template <size_t Index, typename Visitor, typename Variant>
auto TypeCheckedVisitHelperFunction(Visitor &&vis, Variant &&var)
   -> typename VisitHelperReturn<Visitor&&, Variant&&>::type
{
   static_assert(std::is_same_v<
      typename VisitHelperReturn<Visitor&&, Variant&&>::type,
      std::invoke_result_t<
         decltype(VisitHelperFunction<Index, Visitor&&, Variant&&>),
         Visitor &&, Variant &&>
   >,
   "Visitor must return the same type for all alternatives of the Variant.");
   return VisitHelperFunction<Index>(
      std::forward<Visitor>(vis), std::forward<Variant>(var));
}

//! Help to define Visit() below
template <size_t... Indices, typename Visitor, typename Variant>
decltype(auto)
VisitHelper(std::index_sequence<Indices...>, Visitor &&vis, Variant &&var)
{
   constexpr auto size = sizeof...(Indices);
   using Return = typename VisitHelperReturn<Visitor&&, Variant&&>::type;
   using Function = Return(*)(Visitor&&, Variant&&);
   static constexpr std::array<Function, size + 1> jumpTable{
      TypeCheckedVisitHelperFunction<Indices>...,
      VisitHelperBad
   };
   auto function = jumpTable[std::min(var.index(), size)];
   return function(std::forward<Visitor>(vis), std::forward<Variant>(var));
}

//! Standard in C++20
template<typename T> struct type_identity{ using type = T; };

//! Unevaluated
auto deduce_variant(...) -> void;
template<typename... Types>
auto deduce_variant(std::variant<Types...>& v)
   -> type_identity<std::remove_reference_t<decltype(v)>>;
template<typename... Types>
auto deduce_variant(std::variant<Types...>&& v)
   -> type_identity<std::remove_reference_t<decltype(v)>>;
template<typename... Types>
auto deduce_variant(const std::variant<Types...>& v)
   -> type_identity<std::remove_reference_t<decltype(v)>>;
template<typename... Types>
auto deduce_variant(const std::variant<Types...>&& v)
   -> type_identity<std::remove_reference_t<decltype(v)>>;

template<typename T> using deduced_variant =
   typename decltype(deduce_variant(std::declval<T>()))::type;

template<typename ForwardType, typename Variant>
decltype(auto) forward_variant(Variant &var) {
   return std::forward<ForwardType>(var);
   
}
template<typename ForwardType, typename Variant>
decltype(auto) forward_variant(const Variant &var) {
   return std::forward<ForwardType>(var);
}
}

//! Mimic some of std::visit, for the case of one visitor only
/*! This is necessary because of limitations of the macOS implementation of
 some of the C++17 standard library without a minimum version of 10.13, and
 so let's use this even when not needed on the other platforms, instead of
 having too much conditional compilation
 */
template <typename Visitor, typename Variant,
   typename VariantBase = detail::deduced_variant<Variant &&>>
decltype(auto) Visit(Visitor &&vis, Variant &&var)
{
   using ForwardType = std::conditional_t<std::is_lvalue_reference_v<Variant>,
      std::add_lvalue_reference_t<VariantBase>, VariantBase>;
   constexpr auto size = std::variant_size_v<VariantBase>;
   return detail::VisitHelper(std::make_index_sequence<size>{},
      std::forward<Visitor>(vis), detail::forward_variant<ForwardType>(var));
}

namespace detail {
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

   explicit constexpr MemberInvoker(Member member) : member{ member } {}
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
   constexpr VisitorBase(Is&&... invocables)
      : detail::InvocableBase_t<Invocables>{ std::forward<Is>(invocables) }...
   {}
   constexpr VisitorBase(const VisitorBase&) = default;
   constexpr VisitorBase(VisitorBase&&) = default;
   using detail::InvocableBase_t<Invocables>::operator() ...;
};
}

//! Construct from multiple invocables to get overloaded operator ().
//! It is a literal type when all its base classes are
template<typename... Invocables> struct Visitor
   : detail::VisitorBase<std::remove_reference_t<Invocables>...>
{
   using
      detail::VisitorBase<std::remove_reference_t<Invocables>...>::VisitorBase;
};
template<typename... Is> Visitor(Is&&... invocables) -> Visitor<Is&&...>;

}

#endif
