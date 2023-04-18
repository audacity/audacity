/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file Callable.h

 @brief Functions and classes that generate callable objects
 
 Paul Licameli
 
 **********************************************************************/
#ifndef __AUDACITY_CALLABLE__
#define __AUDACITY_CALLABLE__

#include <functional>
#include <type_traits>
#include <utility>

namespace Callable {

//! standard in C++20; add a level of indirection to a type
template<typename T> struct type_identity { using type = T; };

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

template<typename... Invocables> struct OverloadSetBase
   : detail::InvocableBase_t<Invocables>...
{
   //! Variadic constructor allowing arguments with different value categories
   template<typename... Is>
   constexpr OverloadSetBase(Is&&... invocables)
      : detail::InvocableBase_t<Invocables>{ std::forward<Is>(invocables) }...
   {}
   constexpr OverloadSetBase(const OverloadSetBase&) = default;
   constexpr OverloadSetBase(OverloadSetBase&&) = default;
   using detail::InvocableBase_t<Invocables>::operator() ...;
};
}

//! Construct from multiple invocables to get overloaded operator ().
//! It is a literal type when all its base classes are
template<typename... Invocables> struct OverloadSet
   : detail::OverloadSetBase<std::remove_reference_t<Invocables>...>
{
   using detail::OverloadSetBase<std::remove_reference_t<Invocables>...>
      ::OverloadSetBase;
};
template<typename... Is> OverloadSet(Is&&... invocables)
   -> OverloadSet<Is&&...>;

}

#endif
