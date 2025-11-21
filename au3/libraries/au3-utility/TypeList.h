/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file TypeList.h
  @brief Metaprogramming utilities for manipulating lists of types

  See examples of use in TypeList.cpp

  Paul Licameli

**********************************************************************/
#ifndef __AUDACITY_TYPELIST__
#define __AUDACITY_TYPELIST__

#include <cstddef>
#include <tuple>
#include <type_traits>
#include <utility>

//! Utilities for compile-time type manipulation.  Some terminology as in Lisp
namespace TypeList {
/*
 Some of these utilities use "metafunctions." These are classes with a nested
 class template or type alias template, named `typemap`, which takes one type
 parameter.

 The template `Fn` takes such class templates or type alias templates and adapts
 them to this convention.  It can also compose such templates.

 Some utilities require a predicate, which means a metafunction that produces
 types that have an embedded static constexpr member called `value` and
 convertible to bool.

 Many utilities define a nested type `type` and an associated type alias ending
 in `_t` that forces template expansion of that type.

 Sometimes specialization of the template causes no compilation error while any
 use of the `type` in the specialization (outside of a sfinae context) would.
 It can be useful for such a template to be an alternative in
    `typename std::conditional_t<test, Alt1, Alt2>::type`
 where the `test` chooses the other alternative.  Think of this as a
 metaprogramming equivalent of null pointer checking.

 The lists are not lazy.  The size and member types of a TypeList specialization
 are always defined.

 (That is, tere is no attempt here to implement meta-iterators of lists of
 undetermined length, that determine their tail types only when required by
 template substitution.)

 However there is short-circuit evaluation of alternatives in And and Or, which
 can make compound predicates out of simpler ones.
  */

//! standard in C++20; add a level of indirection to a type
template<typename T> struct type_identity {
    using type = T;
};

//! Primary template for a list of arbitrary types
template<typename ... Types> struct List;
//! Empty specialization
template<> struct List<> {
    static constexpr size_t length = 0;
    static constexpr bool null = true;
};
//! Non-empty specialization also defines first and rest
template<typename Type, typename ... Types> struct List<Type, Types...> {
    static constexpr size_t length = sizeof...(Types);
    static constexpr bool null = false;
    using head = Type;
    using tail = List<Types...>;
};

using Nil = List<>;

template<typename TypeList> struct Null;
template<typename TypeList> constexpr auto Null_v = Null<TypeList>::value;
template<typename ... Types> struct Null<List<Types...> >: std::bool_constant<sizeof...(Types) == 0> {};

template<typename TypeList> struct Length;
template<typename TypeList> using Length_t = typename Length<TypeList>::type;
template<typename TypeList> constexpr auto Length_v = Length_t<TypeList>::value;
template<typename ... Types> struct Length<List<Types...> >: std::integral_constant<size_t, sizeof...(Types)> {};

//! Delays expansion of nested alias `head`; so Head<Nil> is legal
template<typename TypeList> struct Head;
/*!
 Could simply define
 `template<typename TypeList> using Head_t = typename TypeList::head;`
 But instead it is defined in terms of Head
 */
template<typename TypeList> using Head_t = typename Head<TypeList>::type;
template<typename ... Types> struct Head<List<Types...> > {
    using type = typename List<Types...>::head;
};

//! Delays expansion of nested alias rest; so Tail<Nil> is legal
template<typename TypeList> struct Tail;
/*!
 Could simply define
 `template<typename TypeList> using Tail_t = typename TypeList::tail;`
 But instead it is defined in terms of Tail
 */
template<typename TypeList> using Tail_t = typename Tail<TypeList>::type;
template<typename ... Types> struct Tail<List<Types...> > {
    using type = typename List<Types...>::tail;
};

//! Synonym for Tail
template<typename TypeList> using Rest = Tail<TypeList>;
template<typename TypeList> using Rest_t = Tail_t<TypeList>;

template<size_t N, typename TypeList> struct Nth {
private:
    template<size_t I, typename TL> struct Countdown {
        using type = typename Countdown<I - 1, Tail_t<TL> >::type;
    };
    template<typename TL> struct Countdown<0, TL> {
        using type = Head_t<TL>;
    };
public:
    using type = typename Countdown<N, TypeList>::type;
};
template<size_t N, typename TypeList> using Nth_t
    =typename Nth<N, TypeList>::type;

//! Nth is zero-based but "First, Second," etc. are not.
//! Sorry, but that's Lisp convention.
//! (And C++ too: get<I> as applied to pairs, but "first", "second")
template<typename TypeList> using First = Nth<0, TypeList>;
template<typename TypeList> using First_t = typename First<TypeList>::type;

template<typename TypeList> using Second = Nth<1, TypeList>;
template<typename TypeList> using Second_t = typename Second<TypeList>::type;

//! Build a type list from a type and another type list
template<typename Type, typename TypeList> struct Cons;
template<typename Type, typename TypeList> using Cons_t
    =typename Cons<Type, TypeList>::type;
template<typename Type, typename ... Types> struct Cons<Type, List<Types...> > {
    using type = List<Type, Types...>;
};

//! An alternative more suggestive of C++ than Lisp conventions
template<typename TypeList, typename Type> struct PushFront : Cons<Type, TypeList> {};
template<typename TypeList, typename Type> using PushFront_t
    =typename PushFront<TypeList, Type>::type;

//! PushBack -- no Common Lisp equivalent name
template<typename TypeList, typename Type> struct PushBack;
template<typename TypeList, typename Type> using PushBack_t
    =typename PushBack<TypeList, Type>::type;
template<typename Type, typename ... Types> struct
PushBack<List<Types...>, Type> {
    using type = List<Types..., Type>;
};

//! List of the tails of the given list (by decreasing length, excluding Nil,
//! otherwise including itself)
template<typename TypeList> struct NonEmptyTails {
private:
    struct Next {
        using type
            =Cons_t<TypeList, typename NonEmptyTails<Tail_t<TypeList> >::type>;
    };
public:
    using type = typename std::conditional_t<
        Null_v<TypeList>, type_identity<Nil>, Next
        >::type;
};
template<typename TypeList> using NonEmptyTails_t
    =typename NonEmptyTails<TypeList>::type;

//! List of the tails of the given list (by decreasing length, including itself
//! and Nil)
template<typename TypeList> struct Tails {
    using type = PushBack_t<NonEmptyTails_t<TypeList>, Nil>;
};
template<typename TypeList> using Tails_t = typename Tails<TypeList>::type;

template<typename TypeList> struct Last;
template<typename TypeList> using Last_t = typename Last<TypeList>::type;
template<typename Type> struct Last<List<Type> > {
    using type = Type;
};
template<typename Type, typename ... Types> struct Last<List<Type, Types...> > {
    using type = Last_t<List<Types...> >;
};

template<typename TypeList> struct ButLast;
template<typename TypeList> using ButLast_t = typename ButLast<TypeList>::type;
template<typename Type> struct ButLast<List<Type> > {
    using type = Nil;
};
template<typename Type, typename ... Types>
struct ButLast<List<Type, Types...> > {
    using type = Cons_t<Type, ButLast_t<List<Types...> > >;
};

//! Compute the composition of any number of templates, applying the rightmost
//! first, and adapting the result to the metafunction convention
template<template<typename> class ... Template> struct Fn;
template<> struct Fn<> {
    template<typename T> using typemap = T;
};
template<template<typename> class Template,
         template<typename> class ... Templates>
struct Fn<Template, Templates...> {
    template<typename T> using typemap
        =Template<typename Fn<Templates...>::template typemap<T> >;
};

//! Compute the composition of any number of unary metafunctions, applying
//! the rightmost first
template<typename ... Metafunctions> struct Compose;
template<> struct Compose<> {
    template<typename T> using typemap = T;
};
template<typename Metafunction, typename ... Metafunctions>
struct Compose<Metafunction, Metafunctions...> {
    template<typename T> using typemap = typename Metafunction::template typemap<
        typename Compose<Metafunctions...>::template typemap<T> >;
};

using Identity = Compose<>;

//! Given a binary template and a fixed argument, make a metafunction
template<template<typename, typename> class BinaryTemplate, typename First>
struct Bind1st {
    template<typename T> using typemap = BinaryTemplate<First, T>;
};

//! Given a binary template and a fixed argument, make a metafunction
template<template<typename, typename> class BinaryTemplate, typename Second>
struct Bind2nd {
    template<typename T> using typemap = BinaryTemplate<T, Second>;
};

//! Bind the types in TypeList to the parameters of a variadic Template
template<template < typename ...> class Template, typename TypeList> struct Apply;
template<template < typename ...> class Template, typename TypeList>
using Apply_t = typename Apply<Template, TypeList>::type;
template<template < typename ...> class Template, typename ... Types>
struct Apply<Template, List<Types...> > {
    using type = Template<Types...>;
};

//! Destructure any tuple-like type into a TypeList
template<typename TupleLike> struct Bind {
private:
    template<typename> struct Impl;
    template<size_t... Is> struct Impl<std::index_sequence<Is...> > {
        using type = List<std::tuple_element_t<Is, TupleLike>...>;
    };
    enum : size_t {
        size = std::tuple_size_v<TupleLike>
    };
public:
    using type = typename Impl<std::make_index_sequence<size> >::type;
};
template<typename TupleLike> using Bind_t = typename Bind<TupleLike>::type;

//! Transform a list of types by the given metafunction
template<typename Metafunction, typename TypeList> struct Map {
private:
    template<typename ... Types> struct Impl {
        using type = List<typename Metafunction::template typemap<Types>...>;
    };
public:
    using type = typename Apply_t<Impl, TypeList>::type;
};
template<typename Metafunction, typename TypeList> using Map_t
    =typename Map<Metafunction, TypeList>::type;

//! Transform the list of nonempty tails of a list of types by the given
//! metafunction
template<typename Metafunction, typename TypeList> struct MapList {
    using type = Map_t<Metafunction, NonEmptyTails_t<TypeList> >;
};
template<typename Metafunction, typename TypeList> using MapList_t
    = typename MapList<Metafunction, TypeList>::type;

template<typename ... Lists> struct Append;
template<typename ... Lists> using Append_t = typename Append<Lists...>::type;
template<> struct Append<> {
    using type = Nil;
};
template<typename TypeList, typename ... TypeLists>
struct Append<TypeList, TypeLists...> {
private:
    template<typename ... FirstTypes> struct CaptureFirst {
        template<typename ... RestTypes> struct CaptureRest {
            using type = List<FirstTypes..., RestTypes...>;
        };
        using type = typename Apply_t<CaptureRest, Append_t<TypeLists...> >::type;
    };
public:
    using type = typename Apply_t<CaptureFirst, TypeList>::type;
};

//! Left fold reduction of a list of types by a binary template
template<template<typename Type, typename Accumulator> class Op,
         typename TypeList, typename Initial
         >
struct LeftFold {
private:
    template<typename ... Types> struct Impl {
        template<typename Acc, typename ...> struct Accumulate;
        template<typename Acc> struct Accumulate<Acc> {
            using type = Acc;
        };
        template<typename Acc, typename T, typename ... Ts>
        struct Accumulate<Acc, T, Ts...> {
            using type = typename Accumulate<Op<T, Acc>, Ts...>::type;
        };
        using type = typename Accumulate<Initial, Types...>::type;
    };
public:
    using type = typename Apply_t<Impl, TypeList>::type;
};
template<template<typename Type, typename Accumulator> class Op,
         typename TypeList, typename Initial
         >
using LeftFold_t = typename LeftFold<Op, TypeList, Initial>::type;

//! Like LeftFold, but passing nonempty prefixes, not elements, to Op
template<template<typename Prefix, typename Accumulator> class Op,
         typename TypeList, typename Initial
         >
struct LeftFoldList {
private:
    template<typename T, typename Pair> struct Op1 {
        using NewPrefix = PushBack_t<Second_t<Pair>, T>;
        using type = List<Op<NewPrefix, First_t<Pair> >, NewPrefix>;
    };
    template<typename T, typename Pair> using Op1_t
        =typename Op1<T, Pair>::type;
public:
    using type = First_t<LeftFold_t<Op1_t, TypeList, List<Initial, Nil> > >;
};
template<template<typename Prefix, typename Accumulator> class Op,
         typename TypeList, typename Initial
         >
using LeftFoldList_t = typename LeftFoldList<Op, TypeList, Initial>::type;

//! Right fold reduction of a list of types by a binary template
template<template<typename Type, typename Accumulator> class Op,
         typename TypeList, typename Initial
         >
struct RightFold {
private:
    template<typename Acc, typename TL> struct Accumulate {
        using type = Op<Head_t<TL>, typename Accumulate<Acc, Tail_t<TL> >::type>;
    };
    template<typename Acc> struct Accumulate<Acc, Nil> {
        using type = Acc;
    };
public:
    using type = typename Accumulate<Initial, TypeList>::type;
};
template<template<typename Type, typename Accumulator> class Op,
         typename TypeList, typename Initial
         >
using RightFold_t = typename RightFold<Op, TypeList, Initial>::type;

//! Like RightFold, but passing nonempty tails, not elements, to Op
template<template<typename Tail, typename Accumulator> class Op,
         typename TypeList, typename Initial
         >
struct RightFoldList {
private:
    template<typename Acc, typename TL> struct Accumulate {
        using type = Op<TL, typename Accumulate<Acc, Tail_t<TL> >::type>;
    };
    template<typename Acc> struct Accumulate<Acc, Nil> {
        using type = Acc;
    };
public:
    using type = typename Accumulate<Initial, TypeList>::type;
};
template<template<typename Tail, typename Accumulator> class Op,
         typename TypeList, typename Initial
         >
using RightFoldList_t = typename RightFoldList<Op, TypeList, Initial>::type;

template<typename TypeList> struct Reverse : LeftFold<Cons_t, TypeList, Nil> {};
template<typename TypeList> using Reverse_t = typename Reverse<TypeList>::type;

//! Apply a metafunction to a type
template<typename Predicate, typename T> struct Call {
    using type = typename Predicate::template typemap<T>;
};
template<typename Predicate, typename T> using Call_t
    =typename Call<Predicate, T>::type;

//! Conditionally add const to a type
template<bool Const> using MaybeConst
    =std::conditional_t<Const, Fn<std::add_const_t>, Identity>;

//! Conditionally map const over a type list
template<bool Const, typename TypeList> using MapConst
    =std::conditional_t<Const,
                        Map<Fn<std::add_const_t>, TypeList>,
                        type_identity<TypeList>
                        >;
template<bool Const, typename TypeList> using MapConst_t
    =typename MapConst<Const, TypeList>::type;

//! Apply a metapredicate to a type
template<typename Predicate, typename T> using Is = Call<Predicate, T>;
template<typename Predicate, typename T> using Is_t
    =typename Is<Predicate, T>::type;
template<typename Predicate, typename T> static constexpr bool Is_v
    =bool(Is_t<Predicate, T>::value);

template<typename Predicate> using Not = Compose<Fn<std::negation>, Predicate>;

//! Logical disjunction of metapredicates with short-circuit expansion,
//! meaning substitution failure in predicates after the first that
//! gives a true type, is not an error
template<typename ... Predicates> struct Or;
template<> struct Or<> {
    template<typename T> using typemap = std::false_type;
};
template<typename Predicate, typename ... Predicates>
struct Or<Predicate, Predicates...> {
private:
    template<typename T> struct Rest {
        static constexpr bool value = Is_v<Or<Predicates...>, T>;
    };
public:
    template<typename T> using typemap = typename std::disjunction<
        typename Predicate::template typemap<T>, Rest<T>
        >;
};

//! Logical conjunction of metapredicates with short-circuit expansion,
//! meaning substitution failure in predicates after the first that
//! gives a false type, is not an error
template<typename ... Predicates> struct And;
template<> struct And<> {
    template<typename T> using typemap = std::true_type;
};
template<typename Predicate, typename ... Predicates>
struct And<Predicate, Predicates...> {
private:
    template<typename T> struct Rest {
        static constexpr bool value = Is_v<And<Predicates...>, T>;
    };
public:
    template<typename T> using typemap = typename std::conjunction<
        typename Predicate::template typemap<T>, Rest<T>
        >;
};

//! Derived from the Predicate, applied to the first of the types (often boolean
//! constant types), for which the value is false; or std::true_type
template<typename Predicate, typename TypeList> struct Every : Apply_t<std::conjunction, Map_t<Predicate, TypeList> > {};
//! The constant value in the corresponding type
template<typename Predicate, typename TypeList> constexpr auto Every_v
    =Every<Predicate, TypeList>::value;

//! Derived from the Predicate, applied to the first of the types (often boolean
//! constant types), for which the value is true; or std::false_type
template<typename Predicate, typename TypeList> struct Some : Apply_t<std::disjunction, Map_t<Predicate, TypeList> > {};
//! The constant value in the corresponding type
template<typename Predicate, typename TypeList> constexpr auto Some_v
    =Some<Predicate, TypeList>::value;

//! Derived from the negation of Predicate, applied to the first of the types
//! (often boolean constant types), for which the value is true; or
//! std::false_type
template<typename Predicate, typename TypeList> struct NotEvery : Some<Not<Predicate>, TypeList> {};
//! The constant value in the corresponding type
template<typename Predicate, typename TypeList> constexpr auto NotEvery_v
    =NotEvery<Predicate, TypeList>::value;

//! Derived from the negation of Predicate, applied to the first of the types
//! (often boolean constant types), for which the value is false; or
//! std::true_type
template<typename Predicate, typename TypeList> struct NotAny : Every<Not<Predicate>, TypeList> {};
//! The constant value in the corresponding type
template<typename Predicate, typename TypeList> constexpr auto NotAny_v
    =NotAny<Predicate, TypeList>::value;

//! Just a membership test, not like CommonLisp member, which returns the
//! element
template<typename Item, typename TypeList> struct In : Some<Bind1st<std::is_same, Item>, TypeList> {};
template<typename Item, typename TypeList> constexpr auto In_v
    =In<Item, TypeList>::value;

//! Whether the given type is derived from any member of the list
template<typename Item, typename TypeList> struct HasBaseIn : Some<Bind2nd<std::is_base_of, Item>, TypeList> {};
template<typename Item, typename TypeList> constexpr auto HasBaseIn_v
    =HasBaseIn<Item, TypeList>::value;

//! Find a list of two lists of types, the first containing those satisfying
//! the Predicate, the second containing the others
template<typename Predicate, typename TypeList> struct StablePartition {
private:
    template<typename Type, typename Acc> using Op = std::conditional_t<
        Is_v<Predicate, Type>,
        List<Cons_t<Type, First_t<Acc> >, Second_t<Acc> >,
        List<First_t<Acc>, Cons_t<Type, Second_t<Acc> > >
        >;
public:
    using type = RightFold_t<Op, TypeList, List<Nil, Nil> >;
};
template<typename Predicate, typename TypeList> using StablePartition_t
    =typename StablePartition<Predicate, TypeList>::type;

//! Select only the subsequence of the type list satisfying the predicate
template<typename Predicate, typename TypeList> struct Filter {
    using type = First_t<StablePartition_t<Predicate, TypeList> >;
};
template<typename Predicate, typename TypeList> using Filter_t
    =typename Filter<Predicate, TypeList>::type;
}

#endif
