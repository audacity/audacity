add_compile_definitions(CMAKE_SOURCE_DIR="${CMAKE_SOURCE_DIR}")

add_unit_test(
   NAME
      lib-time-and-pitch
   WAV_FILE_IO
   SOURCES
      StaffPadTimeAndPitchTest.cpp
      TimeAndPitchFakeSource.h
      TimeAndPitchRealSource.h
      TimeAndPitchTest.cpp
   LIBRARIES
      lib-utility
      lib-time-and-pitch-interface
)
