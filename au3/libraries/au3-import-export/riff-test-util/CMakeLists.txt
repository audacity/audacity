#[[
A command line testing program for tempo metadata in files
]]

add_executable(riff-test-util
   RiffTestUtil.cpp
)

target_link_libraries(riff-test-util
   au3-import-export
   au3-file-formats
)
