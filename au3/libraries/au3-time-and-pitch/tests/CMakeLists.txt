#[[
Unit tests for au3-time-and-pitch
]]

# The tests build "tests/samples/..." paths from this; point it at the AU3 root (au3/), not the AU4 one
add_compile_definitions(CMAKE_SOURCE_DIR="${AUDACITY_ROOT}")

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
      au3-time-and-pitch
)
