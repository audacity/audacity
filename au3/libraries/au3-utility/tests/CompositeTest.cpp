/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompositeTest.cpp

  Paul Licameli

**********************************************************************/
#include <catch2/catch.hpp>
#include "Composite.h"
#include "Callable.h"
#include <algorithm>
#include <array>
#include <functional>
#include <iterator>
#include <numeric>

using namespace Composite;
using namespace std;

namespace {
struct Ignore {};

struct MyComponent {
    MyComponent(int value)
        : value{value} {}
    MyComponent(int value, Ignore ignored)
        : value{value} {}
    virtual ~MyComponent() = default;
    const int value;
    operator int() const {
        return value;
    }
};

constexpr auto Component = Callable::UniqueMaker<MyComponent, int>();

using MyCompositeBase = Base<MyComponent, unique_ptr<MyComponent>, int>;
using MyCompositeBase2
    =Base<MyComponent, unique_ptr<MyComponent>, int, Ignore>;

inline bool operator==(int n, const unique_ptr<MyComponent>& p)
{
    return n == *p;
}

// Test that two sequences are equal, several ways, which also exercises
// compilation of all the STL style accessors
template<bool members = true, typename Container1, typename Container2>
bool compareSequences(const Container1& c1, const Container2& c2)
{
    bool result = true;
    if constexpr (members) {
        result
            =(equal(c1.begin(), c1.end(), c2.begin(), c2.end()))
              &&
              (equal(c1.cbegin(), c1.cend(), c2.cbegin(), c2.cend()))
              &&
              (equal(c1.rbegin(), c1.rend(), c2.rbegin(), c2.rend()))
              &&
              (equal(c1.crbegin(), c1.crend(), c2.crbegin(), c2.crend()));
    }
    result = result
             && (equal(begin(c1), end(c1), begin(c2), end(c2)))
             &&
             (equal(cbegin(c1), cend(c1), cbegin(c2), cend(c2)))
             &&
             (equal(rbegin(c1), rend(c1), rbegin(c2), rend(c2)))
             &&
             (equal(crbegin(c1), crend(c1), crbegin(c2), crend(c2)))
    ;
    return result;
}
}

template<typename Container, auto Maker = nullptr, typename ... Args>
void DoTest(Args ... args)
{
    auto Make = [](int value){
        if constexpr (!bool(Maker)) {
            return Component(value, Ignore {});
        } else {
            return Maker(value);
        }
    };

    using ComponentType = decltype(Make(0));

    // CompositeBase passes constructor arguments to its Component
    Container container{ args ... };
    REQUIRE(0 == container);
    REQUIRE(container.empty());

    constexpr int N = 4;

    // Values for comparison
    vector<int> values(N);
    iota(values.begin(), values.end(), 1);

    // Make some components
    vector<ComponentType> components;
    // Not yet equal
    REQUIRE(!compareSequences(values, components));

    for (size_t ii = 1; ii <= N; ++ii) {
        components.push_back(Make(ii));
    }

    // Equal values so far
    if (!bool(Maker)) {
        REQUIRE(compareSequences(values, components));
    }

    // Composite works with push_back and back_inserter
    move(components.begin(), components.end(), back_inserter(container));
    REQUIRE(!container.empty());
    REQUIRE(compareSequences(values, container));

    // Break equality of sequences
    values.push_back(N + 1);
    REQUIRE(!compareSequences(values, container));

    // Restore equality (and note, Component can take more arguments)
    container.push_back(Make(N + 1));
    REQUIRE(compareSequences(values, container));
}

TEST_CASE("Composite::Base")
{
    DoTest<MyCompositeBase>(0);
    // Also test the extra arguments of MyComponent
    DoTest<MyCompositeBase2>(0, Ignore {});
}

namespace {
struct MyComponentEx : MyComponent {
    // Scramble the given value!!
    MyComponentEx(int value)
        : MyComponent{-value} {}
};
inline bool operator==(int n, const std::unique_ptr<MyComponentEx>& p)
{
    return n == *p;
}

static auto Maker(int value) { return std::make_unique<MyComponentEx>(value); }
struct MyBuilder;
}

// But define a trait that negates it again to unscramble it!
// This specialization must be in the global namespace and precede the complete
// definition of Builder
template<> struct Composite::Traits<MyCompositeBase, MyBuilder> {
    struct ItemBuilderType {
        auto operator ()(std::unique_ptr<MyComponent> ptr) const
        {
            return std::make_unique<MyComponentEx>(ptr->value);
        }

        auto operator ()(int value) const
        {
            return std::make_unique<MyComponentEx>(-value);
        }
    };
    static constexpr auto ItemBuilder = ItemBuilderType{};
    template<typename T> static constexpr auto enables_item_type_v = true;
};

namespace {
struct MyBuilder : Composite::Builder<MyCompositeBase, MyBuilder, int> {
    using Builder::Builder;
};
}

TEST_CASE("Composite::Builder")
{
    using namespace placeholders;
    std::array values{ 0, 1, 2, 3, 4 };

    DoTest<MyBuilder, Maker>(0);
    // Test compilation of the variadic constructor and the overload of
    // ItemBuilderType::operator ()
    // Remember first 0 is not an element
    MyBuilder builder{ 0, 0, 1, 2, 3, 4 };

    REQUIRE(compareSequences(values, builder));

    // Forward range
    MyBuilder builder2{ 0, begin(values), end(values) };
    REQUIRE(compareSequences(values, builder2));

    // Transform range
    std::array values2{ 1, 2, 3, 4, 5 };
    MyBuilder builder3{ 0, begin(values2), end(values2),
                        bind(minus {}, _1, -1) };
    REQUIRE(compareSequences(values, builder2));
}

TEST_CASE("Composite::Extension")
{
    struct X {};
    using Container = Extension<MyCompositeBase, X, int>;
    DoTest<Container>(0, X {});
    using Container2 = Extension<MyCompositeBase2, X, int, Ignore>;
    DoTest<Container2>(0, Ignore {}, X {});
}

TEST_CASE("Composite::Extension specialized for void")
{
    using Container = Extension<MyCompositeBase, void, int>;
    DoTest<Container>(0);
    using Container2 = Extension<MyCompositeBase2, void, int, Ignore>;
    DoTest<Container2>(0, Ignore {});
}
