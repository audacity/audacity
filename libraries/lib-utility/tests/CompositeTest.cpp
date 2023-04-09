/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompositeTest.cpp

  Paul Licameli

**********************************************************************/
#include <catch2/catch.hpp>
#include "Composite.h"
#include <algorithm>
#include <iterator>
#include <numeric>

using namespace Composite;
using namespace std;

namespace {
struct MyComponent {
   MyComponent(int value) : value{ value } {}
   virtual ~MyComponent() = default;
   const int value;
   operator int() const { return value; }
};

using MyCompositeBase = Base<MyComponent, unique_ptr<MyComponent>, int>;

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

TEST_CASE("Composite")
{
   // CompositeBase passes constructor arguments to its Component
   MyCompositeBase compositeBase{ 0 };
   REQUIRE(0 == compositeBase);
   REQUIRE(compositeBase.empty());

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
   move(components.begin(), components.end(),
      back_inserter(compositeBase));
   REQUIRE(!compositeBase.empty());
   REQUIRE(compareSequences(values, compositeBase));
   // Break equality of sequences
   values.push_back(N + 1);
   REQUIRE(!compareSequences(values, compositeBase));
   // Restore it
   compositeBase.push_back(make_unique<MyComponent>(N + 1));
   REQUIRE(compareSequences(values, compositeBase));
}
