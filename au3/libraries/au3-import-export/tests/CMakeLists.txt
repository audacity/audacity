#[[
Unit tests for lib-import-export
]]
add_subdirectory(../riff-test-util)

add_unit_test(
   NAME
      lib-import-export
   SOURCES
      GetAcidizerTagsTests.cpp
   LIBRARIES
      lib-import-export
)
