#[[
A command line testing program for tempo metadata in files
]]

add_executable(riff-test-util
   RiffTestUtil.cpp
   ../tests/LibsndfileTagger.cpp
   ../tests/LibsndfileTagger.h
)

target_link_libraries(riff-test-util
   au3-file-formats
   SndFile::sndfile
)
