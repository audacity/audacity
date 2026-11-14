#  SPDX-License-Identifier: GPL-2.0-or-later
#[[
Unit tests for au3-numeric-formats
]]

add_unit_test(
   NAME
      au3-numeric-formats
   MOCK_PREFS
   MOCK_AUDIO
   SOURCES
      NumericConverterTests.cpp
   LIBRARIES
      au3-numeric-formats
      wxBase
)
