#  SPDX-License-Identifier: GPL-2.0-or-later
#[[
Unit tests for au3-utility
]]

add_unit_test(
   NAME
      au3-utility
   SOURCES
      CallableTest.cpp
      CompositeTest.cpp
      MathApproxTest.cpp
      TupleTest.cpp
      TypeEnumeratorTest.cpp
      VariantTest.cpp
   LIBRARIES
      au3-utility
)
