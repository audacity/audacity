/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TupleTest.cpp

  Paul Licameli

**********************************************************************/
#include <catch2/catch.hpp>

#include "Tuple.h"
#include "TypeList.h"
#include <array>
#include <memory>

using namespace Tuple;
using namespace TypeList;
using namespace std;

namespace {
struct X {};
using Example = tuple<unsigned char, int, double, unique_ptr<X> >;
using ExampleList = List<unsigned char, int, double, unique_ptr<X> >;
}

TEST_CASE("Tuple")
{
    static_assert(is_same_v<Example, Apply_t<tuple, ExampleList> >);

    static_assert(is_tuple_v<Example>);
    static_assert(!is_tuple_v<std::pair<int, double> >);
    static_assert(!is_tuple_v<std::array<int, 2> >);
    static_assert(!is_tuple_v<ExampleList>);

    static_assert(is_tuple_like_v<Example>);
    static_assert(is_tuple_like_v<std::pair<int, double> >);
    static_assert(is_tuple_like_v<std::array<int, 2> >);
    static_assert(!is_tuple_like_v<ExampleList>);

    static_assert(Empty_v<tuple<> >);
    REQUIRE(Empty(tuple {}));

    Example ex1{ '\0', 1, 2.0, make_unique<X>() };
    static_assert(!Empty_v<Example>);
    REQUIRE(!Empty(ex1));

    const auto& cex1 = ex1;

    SECTION("Projections")
    {
        auto p0 = Project<>(ex1);
        static_assert(is_same_v<decltype(p0), tuple<> >);

        auto p1 = Project<2>(cex1);
        static_assert(is_same_v<decltype(p1), tuple<double> >);
        REQUIRE(get<0>(p1) == 2.0);

        auto p2 = Project<0, 2>(move(cex1));
        static_assert(is_same_v<decltype(p2), tuple<unsigned char, double> >);
        REQUIRE(get<0>(p2) == '\0');

        auto p3 = Project<0, 1, 2>(ex1);
        static_assert(is_same_v<decltype(p3), tuple<unsigned char, int, double> >);
        REQUIRE(get<1>(ex1) == 1);
        REQUIRE(get<1>(p3) == 1);
        // Did not forward references
        ++get<1>(ex1);
        REQUIRE(get<1>(ex1) == 2);
        REQUIRE(get<1>(p3) == 1);
        ++get<1>(p3);
        REQUIRE(get<1>(ex1) == 2);
        REQUIRE(get<1>(p3) == 2);

        const auto pX = get<3>(ex1).get();
        REQUIRE(pX);
        auto p4 = Project<0, 1, 2, 3>(move(ex1));
        static_assert(is_same_v<decltype(p4), Example>);
        REQUIRE(get<3>(p4).get() == pX);
        // The unique pointer moved when making p4
        REQUIRE(!get<3>(ex1).get());
    }

    SECTION("Forwarding Projections")
    {
        auto p0 = ForwardProject<>(ex1);
        static_assert(is_same_v<decltype(p0), tuple<> >);

        auto p1 = ForwardProject<2>(cex1);
        static_assert(is_same_v<decltype(p1), tuple<const double&> >);
        REQUIRE(get<0>(p1) == 2.0);

        auto p2 = ForwardProject<0, 2>(move(cex1));
        static_assert(is_same_v<decltype(p2),
                                tuple<const unsigned char &&, const double &&> >);
        REQUIRE(get<0>(p2) == '\0');

        auto p3 = ForwardProject<0, 1, 2>(ex1);
        static_assert(is_same_v<decltype(p3),
                                tuple<unsigned char&, int&, double&> >);
        REQUIRE(get<1>(ex1) == 1);
        REQUIRE(get<1>(p3) == 1);
        // Modify the original and through the reference
        ++get<1>(ex1);
        REQUIRE(get<1>(ex1) == 2);
        REQUIRE(get<1>(p3) == 2);
        ++get<1>(p3);
        REQUIRE(get<1>(ex1) == 3);
        REQUIRE(get<1>(p3) == 3);

        const auto pX = get<3>(ex1).get();
        REQUIRE(pX);
        auto p4 = ForwardProject<0, 1, 2, 3>(move(ex1));
        static_assert(is_same_v<decltype(p4),
                                tuple<unsigned char &&, int &&, double &&, unique_ptr<X>&&> >);
        REQUIRE(get<3>(p4).get() == pX);
        // The unique pointer did NOT move when making p4
        REQUIRE(get<3>(ex1).get());
        auto discard = move(get<3>(p4));
        // The unique pointer was referenced through p4
        REQUIRE(!get<3>(ex1).get());
    }
}
