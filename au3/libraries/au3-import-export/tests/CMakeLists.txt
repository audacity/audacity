#[[
Unit tests for au3-import-export
]]
add_subdirectory(../riff-test-util)

add_unit_test(
   NAME
      au3-import-export
   SOURCES
      GetAcidizerTagsTests.cpp
      LibsndfileTagger.cpp
      LibsndfileTagger.h
   LIBRARIES
      au3-file-formats
      au3-import-export
      SndFile::sndfile # LibsndfileTagger writes the tags it reads back
)
