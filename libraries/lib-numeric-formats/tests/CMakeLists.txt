#  SPDX-License-Identifier: GPL-2.0-or-later
#[[
Unit tests for lib-numeric-formats
]]

add_unit_test(
   NAME
      lib-numeric-formats
   MOCK_PREFS
   MOCK_AUDIO
   SOURCES
      NumericConverterTests.cpp
   LIBRARIES
      lib-numeric-formats
      wxBase
)
