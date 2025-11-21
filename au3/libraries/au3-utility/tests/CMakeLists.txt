#  SPDX-License-Identifier: GPL-2.0-or-later
#[[
Unit tests for lib-utility
]]

add_unit_test(
   NAME
      lib-utility
   SOURCES
      CallableTest.cpp
      CompositeTest.cpp
      MathApproxTest.cpp
      TupleTest.cpp
      TypeEnumeratorTest.cpp
      VariantTest.cpp
   LIBRARIES
      lib-utility
)
