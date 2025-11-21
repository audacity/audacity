/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  VariantTest.cpp

  Paul Licameli

**********************************************************************/
#include <catch2/catch.hpp>

#include "Callable.h"
#include "Variant.h"
using namespace Variant;

enum Ref {
    lvalue, rvalue, noref
};

// Not yet varied in these tests
struct ValueType {
    int x;
    operator int() const {
        return x;
    }
};
// Also works with
// using ValueType = int;

//! Parameterize for the type of visitor return
template<bool Const, Ref ref = lvalue>
struct Tester {
    template<typename T>
    using MaybeConst = std::conditional_t<Const, const T, T>;

    using ResultType = std::conditional_t<ref == noref,
                                          MaybeConst<ValueType>,
                                          std::conditional_t<ref == rvalue,
                                                             std::add_rvalue_reference_t<MaybeConst<ValueType> >,
                                                             std::add_lvalue_reference_t<MaybeConst<ValueType> >
                                                             >
                                          >;

    template<int Value>
    static ResultType value()
    {
        static MaybeConst<ValueType> x{ Value };
        if constexpr (ref == rvalue) {
            return std::move(x);
        } else {
            return x;
        }
    }

    struct X {
        ValueType member{ 0 };
    };
//! Structure can specialize with only a non-const member function
    struct Y {
        template<bool C> auto memberfunction()
        -> std::enable_if_t<!C, ResultType>       { return value<1>(); }
        template<bool C> auto memberfunction() const
        -> std::enable_if_t<C, ResultType>        { return value<1>(); }
    };
//! Structure always with a const member function
    struct Z {
        ResultType constmemberfunction() const { return value<2>(); }
    };
    static ResultType nakedFunction(float) { return value<3>(); }
    static auto moveOnly()
    {
        // Note: remove trailing return type and compilation of Visit correctly
        // breaks because a value, not reference results for one alternative
        return [u = std::make_unique<X>()](double) -> ResultType
        { return value<4>(); };
    }

    struct CopyOnly {
        CopyOnly() = default;
        CopyOnly(const CopyOnly&) = default;
        CopyOnly(CopyOnly&&) = delete;

        ResultType operator()(long double) const& { return value<5>(); }
        ResultType operator()(long double) const&& { return value<6>(); }
        ResultType operator()(long double) & { return value<7>(); }
        ResultType operator()(long double) && { return value<8>(); }
    };

// INVOKE-ing pointer to member always gives lvalue references.
// So if &X::member below is one of the captured invocables,
// and the given variant can contain X,
// then the visitor returns an lvalue reference for the X alternative,
// therefore it must also return a reference for all alternatives.
    using VariantType = std::conditional_t<ref == lvalue,
                                           std::variant<
                                               MaybeConst<X>,
                                               MaybeConst<Y>,
                                               MaybeConst<Z>,
                                               MaybeConst<float>,
                                               MaybeConst<double>,
                                               MaybeConst<long double>
                                               >,
                                           std::variant<
                                               // omit X
                                               MaybeConst<Y>,
                                               MaybeConst<Z>,
                                               MaybeConst<float>,
                                               MaybeConst<double>,
                                               MaybeConst<long double>
                                               >
                                           >;

    template<typename Visitor, typename Arg>
    static void testCase(const Visitor& visitor, int result, Arg& arg)
    {
        if constexpr (Const) {
            const std::remove_reference_t<decltype(arg)> carg{ arg };
            const VariantType cv{ carg };
            REQUIRE(result == visitor(carg));
            REQUIRE(result == Visit(visitor, cv));
            if constexpr (ref != lvalue) {
                REQUIRE(result == visitor(std::move(carg)));
                REQUIRE(result == Visit(visitor, move(cv)));
            }
        }

        {
            VariantType v{ arg };
            REQUIRE(result == visitor(arg));
            REQUIRE(result == Visit(visitor, v));
            if constexpr (ref != lvalue) {
                REQUIRE(result == visitor(std::move(arg)));
                REQUIRE(result == Visit(visitor, move(v)));
            }
        }
    }

    void DoTests()
    {
        // Callable::OverloadSet can capture many kinds of things.  Test each.
        // This also tests compilation of the variadic constructor of OverloadSet
        // which can take a mix of l- and rvalues.
        CopyOnly copyOnly;
        const auto visitor = Callable::OverloadSet(
            &X::member,
            &Y::template memberfunction<Const>,
            &Z::constmemberfunction,
            nakedFunction,
            moveOnly(),
            copyOnly
            );

        X x;
        Y y;
        Z z;
        float f{};
        double d{};
        long double ld{};

        // Pointer to data member, captured as INVOKE-able, can only return lvalue
        // references, so it is excluded from other test cases
        if constexpr (ref == lvalue) {
            testCase(visitor, 0, x);
        }

        testCase(visitor, 1, y);
        testCase(visitor, 2, z);
        testCase(visitor, 3, f);
        testCase(visitor, 4, d);
        testCase(visitor, 5, ld);

        // An invocable can distinguish its own value category and constness
        REQUIRE(6 == std::move(visitor)(ld));
        using ConstVisitorType = decltype(visitor);
        using VisitorType = std::remove_const_t<ConstVisitorType>;
        auto& mutVisitor = const_cast<VisitorType&>(visitor);
        REQUIRE(7 == mutVisitor(ld));
        REQUIRE(8 == std::move(mutVisitor)(ld));
    }
}; // stateless struct Tester

TEST_CASE("Variant visitors returning T &")
{
    Tester<false> {}.DoTests();
}

TEST_CASE("Variant visitors returning T const &")
{
    Tester<true> {}.DoTests();
}

TEST_CASE("Variant visitors returning T &&")
{
    Tester<false, rvalue> {}.DoTests();
}

TEST_CASE("Variant visitors returning T const &&")
{
    Tester<true, rvalue> {}.DoTests();
}

TEST_CASE("Variant visitors returning T")
{
    Tester<false, noref> {}.DoTests();
}

TEST_CASE("Variant visitors returning T const")
{
    Tester<true, noref> {}.DoTests();
}

struct TestVisitor {
    static int x;
    int& operator ()(std::monostate) const { return x; }
};

// Visit() can be called for a proper subclasses of a std::variant<> too
struct TestVariant : std::variant<std::monostate> {};

static void compileTest()
{
    // Test compilation of all 16 combinations of constness and value category
    // of the visitor and the variant

    // This is an example that fails to compile when decltype(auto) in Variant.h
    // is replaced with auto
    TestVariant var;
    const TestVariant cvar;
    TestVisitor vis;
    const TestVisitor cvis;

    static_assert(std::is_same_v<int&, decltype(Visit(vis, var))>);
    static_assert(std::is_same_v<int&, decltype(Visit(cvis, var))>);
    static_assert(std::is_same_v<int&, decltype(Visit(std::move(vis), var))>);
    static_assert(std::is_same_v<int&, decltype(Visit(std::move(cvis), var))>);

    static_assert(std::is_same_v<int&, decltype(Visit(vis, cvar))>);
    static_assert(std::is_same_v<int&, decltype(Visit(cvis, cvar))>);
    static_assert(std::is_same_v<int&, decltype(Visit(std::move(vis), cvar))>);
    static_assert(std::is_same_v<int&, decltype(Visit(std::move(cvis), cvar))>);

    static_assert(std::is_same_v<int&, decltype(Visit(vis, std::move(var)))>);
    static_assert(std::is_same_v<int&, decltype(Visit(cvis, std::move(var)))>);
    static_assert(std::is_same_v<int&, decltype(Visit(std::move(vis), std::move(var)))>);
    static_assert(std::is_same_v<int&, decltype(Visit(std::move(cvis), std::move(var)))>);

    static_assert(std::is_same_v<int&, decltype(Visit(vis, std::move(cvar)))>);
    static_assert(std::is_same_v<int&, decltype(Visit(cvis, std::move(cvar)))>);
    static_assert(std::is_same_v<int&, decltype(Visit(std::move(vis), std::move(cvar)))>);
    static_assert(std::is_same_v<int&, decltype(Visit(std::move(cvis), std::move(cvar)))>);
}
