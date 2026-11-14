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
#include <memory>
#include <type_traits>
#include <utility>

namespace Callable {
//! standard in C++20; add a level of indirection to a type
template<typename T> struct type_identity {
    using type = T;
};

namespace detail {
template<typename> struct FunctionInvoker;
template<typename R, typename ... Args> struct FunctionInvoker<R(Args...)> {
    using type = R(Args...);
    type* const f;
    R operator()(Args... args) const { return f(std::forward<Args>(args)...); }
};

//! Capture pointer to member
template<typename M, typename C> struct MemberInvoker {
    using Member = M C::*;
    template<typename T> using Result = std::invoke_result_t<Member, T>;
    template<typename, typename = void> struct CanAccept : std::false_type {};
    template<typename T> struct CanAccept<T, std::void_t<Result<T> > >: std::true_type {};

    explicit constexpr MemberInvoker(Member member)
        : member{member} {}
    //! Cover all cases of std::invoke, with perfect forwarding, and
    //! sfinae eliminates the overloads for argument types for which it
    //! is inapplicable
    template<typename Obj> auto operator()(Obj&& obj) const
    -> std::enable_if_t<CanAccept<Obj &&>::value, Result<Obj &&> >
    {
        return std::invoke(member, std::forward<Obj>(obj));
    }

    Member member;
};

//! Capture any invocable as a class, using std::function only when needed
template<typename Invocable> struct InvocableBase {
    using type = typename std::conditional_t<std::is_class_v<Invocable>,
                                             Invocable,
                                             FunctionInvoker<std::remove_pointer_t<Invocable> >
                                             >;
};
template<typename Invocable> using InvocableBase_t
    =typename InvocableBase<Invocable>::type;
//! partial specialization for pointers to members
template<typename M, typename C> struct InvocableBase<M C::*> {
    using type = MemberInvoker<M, C>;
};

template<typename ... Invocables> struct OverloadSetBase : detail::InvocableBase_t<Invocables>...
{
    constexpr OverloadSetBase() = default;
    //! Variadic constructor allowing arguments with different value categories
    template<typename ... Is>
    constexpr OverloadSetBase(Is&&... invocables)
        : detail::InvocableBase_t<Invocables>{std::forward<Is>(invocables)}...
    {}
    constexpr OverloadSetBase(const OverloadSetBase&) = default;
    constexpr OverloadSetBase(OverloadSetBase&&) = default;
    using detail::InvocableBase_t<Invocables>::operator() ...;
};
}

//! Construct from multiple invocables to get overloaded operator ().
//! It is a literal type when all its base classes are
template<typename ... Invocables> struct OverloadSet : detail::OverloadSetBase<std::remove_reference_t<Invocables>...>
{
    using detail::OverloadSetBase<std::remove_reference_t<Invocables>...>
    ::OverloadSetBase;
};
template<typename ... Is> OverloadSet(Is && ... invocables)
->OverloadSet<Is && ...>;

//! Generates functions useable as non-type template parameters
template<typename Type, typename ... Arguments> struct UniquePtrFactory {
    [[nodiscard]]
    static auto Function(Arguments... arguments) -> std::unique_ptr<Type>
    {
        return std::make_unique<Type>(std::forward < Arguments && > (arguments)...);
    }
};

//! Generates functions useable as non-type template parameters
template<typename Type, typename ... Arguments> struct SharedPtrFactory {
    [[nodiscard]]
    static auto Function(Arguments... arguments) -> std::shared_ptr<Type>
    {
        return std::make_shared<Type>(std::forward < Arguments && > (arguments)...);
    }
};

//! Generates functions useable as non-type template parameters
template<auto Value, typename ... Arguments> struct Constantly {
    static decltype(Value) Function(Arguments ...) {
        return Value;
    }
};

//! Generate variadic factory functions
/*!
 @tparam FixedArgs can be useful to enable initializer-list syntax for an
 argument, where otherwise type deduction would fail
 */
template<typename T, typename ... FixedArgs> constexpr auto UniqueMaker()
{
    return [](FixedArgs... fixedArgs, auto&&... args) {
        return std::make_unique<T>(
            std::forward<FixedArgs>(fixedArgs)...,
            std::forward<decltype(args)>(args)...);
    };
}
}

#endif
