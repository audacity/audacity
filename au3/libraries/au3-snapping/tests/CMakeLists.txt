#  SPDX-License-Identifier: GPL-2.0-or-later
#[[
Unit tests for au3-snapping
]]

add_unit_test(
   NAME
      au3-snapping
   MOCK_PREFS
   MOCK_AUDIO
   SOURCES
      SnappingTest.cpp
   LIBRARIES
      au3-snapping
      wxBase
)
