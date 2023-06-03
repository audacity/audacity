#  SPDX-License-Identifier: GPL-2.0-or-later

add_unit_test(
   NAME
      lib-utility
   SOURCES
      CallableTest.cpp
      CompositeTest.cpp
      TupleTest.cpp
      TypeEnumeratorTest.cpp
      VariantTest.cpp
   LIBRARIES
      lib-utility
)
