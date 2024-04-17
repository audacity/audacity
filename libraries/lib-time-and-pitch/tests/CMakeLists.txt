#[[
Unit tests for lib-time-and-pitch
]]

add_compile_definitions(CMAKE_SOURCE_DIR="${CMAKE_SOURCE_DIR}")

add_unit_test(
   NAME
      lib-time-and-pitch
   WAV_FILE_IO
   MOCK_PREFS
   SOURCES
      StaffPadTimeAndPitchTest.cpp
      TimeAndPitchFakeSource.h
      TimeAndPitchRealSource.h
   LIBRARIES
      lib-utility
      lib-time-and-pitch-interface
)
