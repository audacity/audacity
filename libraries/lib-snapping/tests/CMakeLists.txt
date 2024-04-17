#  SPDX-License-Identifier: GPL-2.0-or-later
#[[
Unit tests for lib-snapping
]]

add_unit_test(
   NAME
      lib-snapping
   MOCK_PREFS
   MOCK_AUDIO
   SOURCES
      SnappingTest.cpp
   LIBRARIES
      lib-snapping
      wxBase
)
