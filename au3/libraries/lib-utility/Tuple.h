/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file Tuple.h
  @brief Extraction of sub-tuples of std::tuple (or other tuple-like classes)

  Paul Licameli

**********************************************************************/
#ifndef __AUDACITY_TUPLE__
#define __AUDACITY_TUPLE__

#include <cstddef>
#include <tuple>
#include <type_traits>
#include <utility>

namespace Tuple {
//! Type test metapredicate, for specializations of std::tuple only
template<typename T> struct is_tuple : std::false_type {};
template<typename ... Types> struct is_tuple<std::tuple<Types...> >: std::true_type {};
template<typename T> static constexpr auto is_tuple_v = is_tuple<T>::value;

//! Type test metapredicate, for more general tuple-like types
template<typename T, typename = void> struct is_tuple_like : std::false_type {};
template<typename T> struct is_tuple_like<T,
                                          // std::tuple_size<T> is only forward-declared except where customized for
                                          // T = std::tuple<U...> or other (possibly user) types
                                          std::void_t<decltype(std::tuple_size<T> {})>
                                          >  : std::true_type {};
template<typename T> static constexpr auto is_tuple_like_v
    =is_tuple_like<T>::value;

template<typename Tuple> constexpr bool Empty_v
    =(0 == size_t(std::tuple_size_v<Tuple>));

template<typename Tuple> [[nodiscard]] constexpr bool Empty(const Tuple& tuple)
{
    return Empty_v<Tuple>;
}

// helpers
namespace detail {
template<typename> struct increment_each;
template<size_t... Indices> struct increment_each<
    std::index_sequence<Indices...>
    >  : std::index_sequence<Indices + 1 ...> {};

template<typename, typename> struct each_less;
template<size_t... Indices1, size_t... Indices2>
struct each_less<
    std::index_sequence<Indices1...>, std::index_sequence<Indices2...>
    > {
    static constexpr auto value = ((Indices1 < Indices2) && ...);
};

constexpr size_t npos(-1);

template<size_t... Indices> struct increasing;
template<> struct increasing<> {
    static constexpr auto value = true;
};
template<size_t Index> struct increasing<Index> {
    static constexpr auto value = true;
};
template<size_t Index, size_t... Indices> struct increasing<Index, Indices...> {
    static constexpr auto value = each_less<
        std::index_sequence<Index, Indices...>,
        std::index_sequence<Indices..., npos>
        >::value;
};
}

//! Return a tuple of values initialized from a subsequence of a tuple
template<size_t... Indices> constexpr auto Project = [](auto&& tuple)
{
    using Tuple = decltype(tuple);
    constexpr auto size = std::tuple_size_v<std::remove_reference_t<Tuple> >;
    static_assert(((Indices < size) && ...), "Indices must be in range");
    // Increasing indices will also be unique, preventing the possibility
    // of moving-from a tuple member twice
    // TODO weaker test just for uniqueness
    static_assert(detail::increasing<Indices...>::value,
                  "Indices must be strictly increasing");
    return std::make_tuple(std::get<Indices>(std::forward<Tuple>(tuple))...);
};

//! Destructures a std::index_sequence argument
template<size_t... Indices, typename Tuple> auto Projection(
    const std::index_sequence<Indices...>&, Tuple&& tuple)
{
    return Project<Indices...>(std::forward<Tuple>(tuple));
}

//! Forwarding of a subsequence of a tuple
template<size_t... Indices> constexpr auto ForwardProject = [](auto&& tuple)
{
    using Tuple = decltype(tuple);
    constexpr auto size = std::tuple_size_v<std::remove_reference_t<Tuple> >;
    static_assert(((Indices < size) && ...), "Indices must be in range");
    // Increasing indices will also be unique, preventing the possibility
    // of (later) moving-from a duplicated rvalue reference in the tuple
    // TODO weaker test just for uniqueness
    static_assert(detail::increasing<Indices...>::value,
                  "Indices must be strictly increasing");
    return
        std::forward_as_tuple(std::get<Indices>(std::forward<Tuple>(tuple))...);
};

//! Destructures a std::index_sequence argument
template<size_t... Indices, typename Tuple> constexpr auto ForwardingProjection(
    const std::index_sequence<Indices...>&, Tuple&& tuple)
{
    return ForwardProject<Indices...>(std::forward<Tuple>(tuple));
}

//! Projection of all of a tuple
template<typename Tuple> auto All(Tuple&& tuple)
{
    constexpr auto Length = std::tuple_size_v<std::decay_t<Tuple> >;
    return Projection(
        std::make_index_sequence<Length> {}, std::forward<Tuple>(tuple));
}

//! Type of projection of all of a tuple
template<typename Tuple> using All_t = decltype(All(std::declval<Tuple>()));

//! Forwarding of all of a tuple
template<typename Tuple> auto ForwardAll(Tuple&& tuple)
{
    constexpr auto Length = std::tuple_size_v<std::decay_t<Tuple> >;
    return ForwardingProjection(
        std::make_index_sequence<Length> {}, std::forward<Tuple>(tuple));
}

//! Type of forwarding references to all members of a tuple
template<typename Tuple> using ForwardAll_t
    =decltype(ForwardAll(std::declval<Tuple>()));

//! Projection of the tail of a tuple
template<typename Tuple,
         typename Decayed = std::decay_t<Tuple>,
         auto Length = std::tuple_size_v<Decayed>,
         typename sfinae = std::enable_if_t<(Length > 0)>
         >
auto Next(Tuple&& tuple)
{
    return Projection(
        detail::increment_each<std::make_index_sequence<Length - 1> > {},
        std::forward<Tuple>(tuple));
}

//! Type of projection of the tail of a tuple
template<typename Tuple> using Next_t = decltype(Next(std::declval<Tuple>()));

//! Forwarding of the tail of a tuple
template<typename Tuple,
         typename Decayed = std::decay_t<Tuple>,
         auto Length = std::tuple_size_v<Decayed>,
         typename sfinae = std::enable_if_t<(Length > 0)>
         >
auto ForwardNext(Tuple&& tuple)
{
    return ForwardingProjection(
        detail::increment_each<std::make_index_sequence<Length - 1> > {},
        std::forward<Tuple>(tuple));
}

//! Type of forwarding references to tail members of a tuple
template<typename Tuple> using ForwardNext_t
    =decltype(ForwardNext(std::declval<Tuple>()));
}

#endif
