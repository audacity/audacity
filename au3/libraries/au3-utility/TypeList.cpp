/**********************************************************************

  Audacity: A Digital Audio Editor

  @file TypeList.cpp
  @brief Examples as a compile-time unit test of metaprogramming utilities

  Paul Licameli

**********************************************************************/
#include "TypeList.h"
#include <array>

// Enough examples to test compilation of all the templates
namespace {
using namespace TypeList;
using namespace std;

using Example = List<int, double>;

static_assert(Null_v<Nil>);
static_assert(!Null_v<Example>);

static_assert(0 == Length_v<Nil>);
static_assert(2 == Length_v<Example>);

static_assert(is_same_v<int, Head_t<Example> >);

// These compile even though Head_t<Nil> does not
using Unused = Head<Nil>;
static_assert(is_same_v<int, Head_t<conditional_t<true, Example, Nil> > >);
template<typename T> struct ShortCircuiting : Head<conditional_t<Null_v<T>, Example, T> > {};
static_assert(is_same_v<int, ShortCircuiting<Nil>::type>);
static_assert(is_same_v<double, ShortCircuiting<List<double> >::type>);

static_assert(is_same_v<List<double>, Tail_t<Example> >);

static_assert(is_same_v<int, First_t<Example> >);
static_assert(is_same_v<double, Second_t<Example> >);

static_assert(is_same_v<Example, Cons_t<int, Cons_t<double, Nil> > >);
static_assert(is_same_v<Example, PushFront_t<PushFront_t<Nil, double>, int> >);
static_assert(is_same_v<Example, PushBack_t<PushBack_t<Nil, int>, double> >);

static_assert(is_same_v<Nil, NonEmptyTails_t<Nil> >);
static_assert(is_same_v<List<Example, List<double> >, NonEmptyTails_t<Example> >);

static_assert(is_same_v<List<Nil>, Tails_t<Nil> >);
static_assert(is_same_v<List<Example, List<double>, Nil>, Tails_t<Example> >);

static_assert(is_same_v<double, Last_t<Example> >);
static_assert(is_same_v<List<int>, ButLast_t<Example> >);

static_assert(is_same_v<Example, Map_t<Identity, Example> >);
static_assert(is_same_v<List<const int, const double>,
                        Map_t<Fn<add_const_t>, Example> >);
static_assert(is_same_v<List<const int&, const double&>,
                        Map_t<Fn<add_lvalue_reference_t, add_const_t>, Example> >);
static_assert(is_same_v<List<const int*&, const double*&>,
                        Map_t<
                            Fn<add_lvalue_reference_t, add_pointer_t, add_const_t>,
                            Example> >);
static_assert(is_same_v<List<int* const&, double* const&>,
                        Map_t<
                            Compose<
                                Fn<add_lvalue_reference_t>,
                                Fn<add_const_t, add_pointer_t> >,
                            Example> >);

static_assert(is_same_v<Nil, MapList_t<Fn<Length_t>, Nil> >);
static_assert(is_same_v<
                  List<integral_constant<size_t, 2>, integral_constant<size_t, 1> >,
                  MapList_t<Fn<Length_t>, Example> >);

static_assert(is_same_v<tuple<int, double>, Apply_t<tuple, Example> >);
static_assert(is_same_v<tuple<const int, const double>,
                        Apply_t<tuple, Map_t<Fn<add_const_t>, Example> > >);

static_assert(is_same_v<Example, Bind_t<tuple<int, double> > >);
static_assert(is_same_v<Example, Bind_t<pair<int, double> > >);
static_assert(is_same_v<List<int, int>, Bind_t<array<int, 2> > >);

static_assert(is_same_v<Nil, Append_t<> >);
static_assert(is_same_v<Example, Append_t<Example> >);
static_assert(is_same_v<List<int, double, char>,
                        Append_t<Example, List<char> > >);

static_assert(is_same_v<List<Example, List<int> >,
                        LeftFoldList_t<Cons_t, Example, Nil> >);

static_assert(is_same_v<List<Example, List<double> >,
                        RightFoldList_t<Cons_t, Example, Nil> >);

static_assert(is_same_v<List<double, int>, Reverse_t<Example> >);

static_assert(is_same_v<const int, Call_t<MaybeConst<true>, int> >);
static_assert(is_same_v<int, Call_t<MaybeConst<false>, int> >);

static_assert(is_same_v<List<const int, const double>,
                        MapConst_t<true, Example> >);
static_assert(is_same_v<Example, MapConst_t<false, Example> >);

static_assert(Is_v<Fn<Null>, Nil>);
static_assert(!Is_v<Fn<Null>, Example>);
static_assert(!Is_v<Not<Fn<Null> >, Nil>);
static_assert(Is_v<Not<Fn<Null> >, Example>);

using HeadIsInt = Compose<Bind1st<is_same, int>, Fn<Head_t> >;
static_assert(Is_v<HeadIsInt, Example>);
// static_assert(!Is_v<HeadIsInt, Nil>); // won't compile

//! Demonstrate short-circuiting behavior of Or and And
using StartsWithInt = And<Not<Fn<Null> >, HeadIsInt>;
static_assert(!Is_v<StartsWithInt, Nil>);
static_assert(Is_v<StartsWithInt, Example>);

using NullOrStartsWithInt = Or<Fn<Null>, HeadIsInt>;
static_assert(Is_v<NullOrStartsWithInt, Nil>);
static_assert(Is_v<NullOrStartsWithInt, Example>);

static_assert(Every_v<Fn<is_arithmetic>, Example>);
static_assert(is_base_of_v<true_type, Every<Fn<is_arithmetic>, Example> >);
static_assert(!Every_v<Fn<is_integral>, Example>);
static_assert(is_base_of_v<is_integral<double>,
                           Every<Fn<is_integral>, Example> >);

static_assert(Some_v<Fn<is_integral>, Example>);
static_assert(is_base_of_v<is_integral<int>,
                           Some<Fn<is_integral>, Example> >);
static_assert(!Some_v<Fn<is_void>, Example>);
static_assert(is_base_of_v<false_type, Some<Fn<is_void>, Example> >);

static_assert(NotEvery_v<Fn<is_floating_point>, Example>);
static_assert(NotAny_v<Fn<is_void>, Example>);

static_assert(!In_v<void, Example>);
static_assert(In_v<int, Example>);

struct B {};
struct D : B {};

static_assert(HasBaseIn_v<D, List<B> >);
static_assert(HasBaseIn_v<D, List<D> >);
static_assert(HasBaseIn_v<B, List<B> >);
static_assert(!HasBaseIn_v<B, List<D> >);
static_assert(HasBaseIn_v<B, List<B, D> >);

static_assert(is_same_v<List<Nil, Example>,
                        StablePartition_t<Fn<is_void>, Example> >);
static_assert(is_same_v<List<List<int>, List<double> >,
                        StablePartition_t<Fn<is_integral>, Example> >);
static_assert(is_same_v<List<Example, Nil>,
                        StablePartition_t<Fn<is_arithmetic>, Example> >);

static_assert(is_same_v<Nil, Filter_t<Fn<is_void>, Example> >);
static_assert(is_same_v<List<int>, Filter_t<Fn<is_integral>, Example> >);
static_assert(is_same_v<Example, Filter_t<Fn<is_arithmetic>, Example> >);

using Example2 = Cons_t<unsigned, Example>;
static_assert(is_same_v<List<unsigned>,
                        Filter_t<And<Not<Fn<is_signed> >, Fn<is_integral> >, Example2> >);
static_assert(is_same_v<List<unsigned, double>,
                        Filter_t<Or<Not<Fn<is_signed> >, Fn<is_floating_point> >, Example2> >);

// To do: more cases
}
