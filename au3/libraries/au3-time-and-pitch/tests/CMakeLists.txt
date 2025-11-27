#[[
Unit tests for au3-time-and-pitch
]]

add_compile_definitions(CMAKE_SOURCE_DIR="${CMAKE_SOURCE_DIR}")

add_unit_test(
   NAME
      au3-time-and-pitch
   WAV_FILE_IO
   MOCK_PREFS
   SOURCES
      StaffPadTimeAndPitchTest.cpp
      TimeAndPitchFakeSource.h
      TimeAndPitchRealSource.h
   LIBRARIES
      au3-utility
      au3-time-and-pitch-interface
)
