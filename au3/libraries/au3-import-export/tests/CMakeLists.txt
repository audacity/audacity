#[[
Unit tests for au3-import-export
]]
add_subdirectory(../riff-test-util)

add_unit_test(
   NAME
      au3-import-export
   SOURCES
      GetAcidizerTagsTests.cpp
   LIBRARIES
      au3-import-export
)
