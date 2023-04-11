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
#include <iterator>
#include <numeric>

using namespace Composite;
using namespace std;

namespace {
struct Ignore{};

struct MyComponent {
   MyComponent(int value) : value{ value } {}
   MyComponent(int value, Ignore ignored) : value{ value } {}
   virtual ~MyComponent() = default;
   const int value;
   operator int() const { return value; }
};

constexpr auto Component = Callable::UniqueMaker<MyComponent, int>();

using MyCompositeBase = Base<MyComponent, unique_ptr<MyComponent>, int>;
using MyCompositeBase2 =
   Base<MyComponent, unique_ptr<MyComponent>, int, Ignore>;

inline bool operator== (int n, const unique_ptr<MyComponent> &p)
{
   return n == *p;
}

// Test that two sequences are equal, several ways, which also exercises
// compilation of all the STL style accessors
template<bool members = true, typename Container1, typename Container2>
bool compareSequences(const Container1 &c1, const Container2 &c2)
{
   bool result = true;
   if constexpr(members) {
      result =
         (equal(c1.begin(), c1.end(), c2.begin(), c2.end()))
      &&
         (equal(c1.cbegin(), c1.cend(), c2.cbegin(), c2.cend()))
      &&
         (equal(c1.rbegin(), c1.rend(), c2.rbegin(), c2.rend()))
      &&
         (equal(c1.crbegin(), c1.crend(), c2.crbegin(), c2.crend()));
   }
   result = result &&
      (equal(begin(c1), end(c1), begin(c2), end(c2)))
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

template<typename Container, typename... Args>
void DoTest(Args ...args)
{
   // CompositeBase passes constructor arguments to its Component
   Container container{ args... };
   REQUIRE(0 == container);
   REQUIRE(container.empty());

   constexpr int N = 4;

   // Values for comparison
   vector<int> values(N);
   iota(values.begin(), values.end(), 1);

   // Make some components
   vector<unique_ptr<MyComponent>> components;
   // Not yet equal
   REQUIRE(!compareSequences(values, components));

   for (size_t ii = 1; ii <= N; ++ii)
      components.push_back(make_unique<MyComponent>(ii));

   // Equal values so far
   REQUIRE(compareSequences(values, components));

   // Composite works with push_back and back_inserter
   move(components.begin(), components.end(), back_inserter(container));
   REQUIRE(!container.empty());
   REQUIRE(compareSequences(values, container));
   
   // Break equality of sequences
   values.push_back(N + 1);
   REQUIRE(!compareSequences(values, container));

   // Restore equality (and note, Component can take more arguments)
   container.push_back(Component(N + 1, Ignore{}));
   REQUIRE(compareSequences(values, container));
}

TEST_CASE("Composite::Base")
{
   DoTest<MyCompositeBase>(0);
   // Also test the extra arguments of MyComponent
   DoTest<MyCompositeBase2>(0, Ignore{});
}

TEST_CASE("Composite::Extension")
{
   struct X{};
   using Container = Extension<MyCompositeBase, X, int>;
   DoTest<Container>(0, X{});
   using Container2 = Extension<MyCompositeBase2, X, int, Ignore>;
   DoTest<Container2>(0, Ignore{}, X{});
}

TEST_CASE("Composite::Extension specialized for void")
{
   using Container = Extension<MyCompositeBase, void, int>;
   DoTest<Container>(0);
   using Container2 = Extension<MyCompositeBase2, void, int, Ignore>;
   DoTest<Container2>(0, Ignore{});
}

