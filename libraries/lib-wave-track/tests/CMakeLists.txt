#  SPDX-License-Identifier: GPL-2.0-or-later

add_unit_test(
   NAME
      lib-wave-track
   MOCK_PREFS
   MOCK_AUDIO
   SOURCES
      MockSampleBlock.cpp
      MockSampleBlock.h
      WaveClipTests.cpp
   LIBRARIES
      lib-wave-track
      wxBase
)
