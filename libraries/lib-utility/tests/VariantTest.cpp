/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  VariantTest.cpp

  Paul Licameli

**********************************************************************/
#include <catch2/catch.hpp>

#include "Variant.h"

namespace {
// Not yet varied in these tests
using ResultType = int;

struct X {
   ResultType member = 0;
};
struct Y {
   ResultType memberfunction() { return 1; }
};
struct Z {
   ResultType constmemberfunction() const { return 2; }
};
ResultType nakedFunction(float) { return 3; }
auto moveOnly() {
   return [u = std::make_unique<X>()](double){ return 4; };
}
struct CopyOnly{
   CopyOnly() = default;
   CopyOnly(const CopyOnly&) = default;
   CopyOnly(CopyOnly&&) = delete;

   ResultType operator() (long double) const & { return 5; }
   ResultType operator() (long double) const && { return 6; }
   ResultType operator() (long double) & { return 7; }
   ResultType operator() (long double) && { return 8; }
};
}

TEST_CASE("Variant", "")
{
   // Variant::Visitor can capture many kinds of things.  Test each.
   // This also tests compilation of the variadic constructor of visitor
   // which can take a mix of l- and rvalues.
   CopyOnly copyOnly;
   const auto visitor = Variant::Visitor(
      &X::member,
      &Y::memberfunction,
      &Z::constmemberfunction,
      nakedFunction,
      moveOnly(),
      copyOnly
   );

   // Verify the right result,
   // and verify it compiles for two kinds of references
   auto nonConstTestCase = [&](int result, auto &arg){
      REQUIRE(result == visitor(arg));
      REQUIRE(result == visitor(std::move(arg)));
   };
   // Verify the right result,
   // and verify it compiles for four kinds of references
   auto completeTestCast = [&](int result, auto &arg){
      const std::remove_reference_t<decltype(arg)> carg{ arg };
      nonConstTestCase(result, arg);
      REQUIRE(result == visitor(carg));
      REQUIRE(result == visitor(std::move(carg)));
   };

   X x;
   Y y;
   Z z;
   float f{};
   double d{};
   long double ld{};
   
   completeTestCast(0, x);
   nonConstTestCase(1, y); // Correctly fails to compile the complete case
   completeTestCast(2, z);
   completeTestCast(3, f);
   completeTestCast(4, d);
   completeTestCast(5, ld);

   // The invocable distinguishes its own value category and constness
   REQUIRE(6 == std::move(visitor)(ld));
   using ConstVisitorType = decltype(visitor);
   using VisitorType = std::remove_const_t<ConstVisitorType>;
   auto &mutVisitor = const_cast<VisitorType&>(visitor);
   REQUIRE(7 == mutVisitor(ld));
   REQUIRE(8 == std::move(mutVisitor)(ld));
}
