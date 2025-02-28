/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CallableTest.cpp

  Paul Licameli

**********************************************************************/
#include <catch2/catch.hpp>
#include "Callable.h"
#include <variant>

// See also VariantTest for more exercise of OverloadSet

using namespace Callable;

namespace {
struct X {
    int member{ 0 };

    X() = default;
    explicit X(int value)
        : member{value} {}
    explicit X(int value, std::shared_ptr<X>)
        : member{value} {}
};

struct TestVisitor {
    static int x;
    int& operator ()(std::monostate) const { return x; }
};

int TestVisitor::x{};

template<auto T> struct TakesNonTypeParameter {};
}

TEST_CASE("Compilation")
{
    {
        // Test contexpr-ness of OverloadSet constructor, and MemberInvoker too
        constexpr auto visitor = OverloadSet{ TestVisitor{}, &X::member },
        // and copy constructor
                       visitor2{ visitor },
        // and move constructor
        visitor3{ OverloadSet{ TestVisitor{}, &X::member } };

        // OverloadSet can be default-constructed, when all the callables can be
        constexpr auto visitor4 = OverloadSet<TestVisitor> {};
    }

    {
        // These function objects are of literal types
        constexpr auto f1 = UniquePtrFactory<X>::Function;
        constexpr auto f2 = UniquePtrFactory<X, int>::Function;
        // How to get multiple signatures
        constexpr auto f3 = OverloadSet{ f1, f2 };
        constexpr auto f4
            =UniquePtrFactory<X, int, std::unique_ptr<X> >::Function;

        {
            auto p1 = f1();
            REQUIRE(p1->member == 0);
            auto p2 = f2(1);
            REQUIRE(p2->member == 1);

            // Demonstrate move of argument
            auto p3 = f4(2, move(p2));
            REQUIRE(p3->member == 2);
            REQUIRE(!p2);
        }

        {
            auto p1 = f3();
            REQUIRE(p1->member == 0);
            auto p2 = f3(1);
            REQUIRE(p2->member == 1);
        }

        TakesNonTypeParameter<f1> t1{};
        TakesNonTypeParameter<f2> t2{};
        // Doesn't work with f3 in C++17
    }

    {
        // These function objects are of literal types
        constexpr auto f1 = SharedPtrFactory<X>::Function;
        constexpr auto f2 = SharedPtrFactory<X, int>::Function;
        // How to get multiple signatures
        constexpr auto f3 = OverloadSet{ f1, f2 };
        constexpr auto f4
            =UniquePtrFactory<X, int, std::shared_ptr<X> >::Function;

        {
            auto p1 = f1();
            REQUIRE(p1->member == 0);
            auto p2 = f2(1);
            REQUIRE(p2->member == 1);

            // Demonstrate move of argument
            auto p3 = f4(2, move(p2));
            REQUIRE(p3->member == 2);
            REQUIRE(!p2);
        }

        {
            auto p1 = f3();
            REQUIRE(p1->member == 0);
            auto p2 = f3(1);
            REQUIRE(p2->member == 1);
        }

        TakesNonTypeParameter<f1> t1{};
        TakesNonTypeParameter<f2> t2{};
        // Doesn't work with f3 in C++17
    }

    {
        // These function objects are of literal types
        constexpr auto f1 = Constantly<0>::Function;
        constexpr auto f2 = Constantly<0, int>::Function;
        // How to get multiple signatures
        constexpr auto f3 = OverloadSet{ f1, f2 };

        REQUIRE(f1() == 0);
        REQUIRE(f2(1) == 0);

        REQUIRE(f3() == 0);
        REQUIRE(f3(1) == 0);

        TakesNonTypeParameter<f1> t1{};
        TakesNonTypeParameter<f2> t2{};
        // Doesn't work with f3 in C++17
    }

    {
        // These function objects are of literal types
        constexpr auto f1 = UniqueMaker<X>();
        constexpr auto f2 = UniqueMaker<X, int>();
        // How to get multiple signatures
        constexpr auto f3 = OverloadSet{ f1, f2 };
        constexpr auto f4
            =UniqueMaker<X, int, std::shared_ptr<X> >();

        {
            auto p1 = f1();
            REQUIRE(p1->member == 0);
            auto p2 = f2(1);
            REQUIRE(p2->member == 1);

            // Demonstrate move of argument
            auto p3 = f4(2, move(p2));
            REQUIRE(p3->member == 2);
            REQUIRE(!p2);

            // Demonstrate how {} can work as an argument
            auto p4 = f2({});
            // auto p4 = f1({}); sorry
            auto p5 = f1(0); // But this works, f1 is still variadic
        }

        {
            auto p1 = f3();
            REQUIRE(p1->member == 0);
            auto p2 = f3(1);
            REQUIRE(p2->member == 1);
        }

        // generat3ed lambdas won't work as template arguments in C++17
    }
}
