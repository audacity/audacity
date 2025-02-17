/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file TypeEnumerator.h

  @brief Make a list of all distinct types so far mentioned in calls of a
  certain macro

  Paul Licameli

**********************************************************************/
#ifndef __AUDACITY_TYPE_ENUMERATOR__
#define __AUDACITY_TYPE_ENUMERATOR__

#include "TypeList.h"

namespace TypeEnumerator {
namespace detail {
//! Standard in C++20
template<typename T> struct type_identity {
    using type = T;
};

//! A type excluded from any enumeration
struct Unenumerated : type_identity<Unenumerated> {};

//! What type is associated with `U`, at the point of instantiation of the
//! template specialization?  (Location is a struct type local to a function)
template<typename Tag, typename Location, unsigned U> using EnumeratedType
    =typename decltype(enumerateTypes(
                           Tag {}, Location {}, std::integral_constant<unsigned, U> {}))::type;

//! Embedded `value` member counts enumerated types so far declared in the
//! translation unit, where instantiated for `Location`
/*!
 @tparam Tag distinguishes different enumerations
 @tparam Location a distinct subclass of Tag for one point of instantiation of
 the template
 */
template<typename Tag, typename Location> class CountTypes
{
    template<unsigned U> struct Stop {
        static constexpr unsigned value = U;
    };
    template<unsigned U> struct Count : std::conditional_t<
            std::is_same_v<Unenumerated, EnumeratedType<Tag, Location, U> >,
            Stop<U>,
            Count<U + 1>
            > {};
public:
    static constexpr unsigned value = Count<0>::value;
};

//! Implements the ENUMERATE_TYPE macro
template<typename Tag, typename T>
struct TypeCounter {
    // Generate a type for the location of the macro call
    struct Location : Tag {};
    // Count types for this new location, stopping one later than for the
    // last specialization
    static constexpr unsigned value = CountTypes<Tag, Location>::value;
    // value is then used to make a new association for the next TypeCounter
    // with a previously unseen T
};
}

/*!
 Inject an unevaluated function, whose overloads will help to define a
 metafunction from an initial segment of the unsigned integers to distinct
 types.
 `Tag` will name an empty structure type to distinguish this enumeration.
 The macro call must occur at file scope, not within any other namespace.
 */
#define BEGIN_TYPE_ENUMERATION(Tag) namespace { \
    struct Tag {}; \
    auto enumerateTypes(Tag, Tag, ...)->TypeEnumerator::detail::Unenumerated; }

//! This macro must occur at file scope, not within any other namespace
#define ENUMERATE_TYPE(Tag, T) namespace { auto enumerateTypes( \
                                               Tag, Tag, std::integral_constant<unsigned, \
                                                                                TypeEnumerator::detail::TypeCounter<Tag, T>::value>) \
                                           ->TypeEnumerator::detail::type_identity<T>; }

//! Embedded `type` member is the list of enumerated types so far declared in
//! the translation unit at first instantiation for `Location`
/*!
 @tparam Tag distinguishes enumerations
 @tparam Location a structure type inheriting Tag
 */
template<typename Tag, typename Location> class CollectTypes
{
    static_assert(std::is_base_of_v<Tag, Location>);
    template<typename ... Types> struct Stop {
        using type = TypeList::List<Types...>;
    };
    // This works by mutual recursion of Accumulate and AccumulateType
    template<unsigned U, typename ... Types> struct Accumulate;
    template<unsigned U, typename Type, typename ... Types> struct AccumulateType : std::conditional_t<std::is_same_v<detail::Unenumerated,
                                                                                                                      Type>,
                                                                                                       Stop<Types...>,
                                                                                                       Accumulate<U + 1, Types..., Type>
                                                                                                       >
    {};
    template<unsigned U, typename ... Types> struct Accumulate : AccumulateType<U, detail::EnumeratedType<Tag, Location, U>, Types...>
    {};
public:
    using type = typename Accumulate<0>::type;
};
}

#endif
