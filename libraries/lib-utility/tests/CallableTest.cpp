/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CallableTest.cpp

  Paul Licameli

**********************************************************************/
#include "Callable.h"
#include <variant>

// See also VariantTest for more exercise of OverloadSet

using namespace Callable;

namespace {
struct X{ int member{ 0 }; };

struct TestVisitor {
   static int x;
   int & operator () (std::monostate) const { return x; }
};

int TestVisitor::x{};
}

static void compileTest()
{
   // Test contexpr-ness of OverloadSet constructor, and MemberInvoker too
   constexpr auto visitor = OverloadSet{ TestVisitor{}, &X::member },
      // and copy constructor
      visitor2{ visitor },
      // and move constructor
      visitor3{ OverloadSet{ TestVisitor{}, &X::member } };
}
