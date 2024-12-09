# **********************************************************************
#
#  Audacity: A Digital Audio Editor
#
# **********************************************************************

message(STATUS "Run unit tests")

# Set environment variables
set(ENV{GTEST_OUTPUT} "xml:test-results")
set(ENV{GTEST_COLOR} "1")
set(ENV{QT_QPA_PLATFORM} "minimal:enable_fonts")
set(ENV{ASAN_OPTIONS} "detect_leaks=0:new_delete_type_mismatch=0")

if(CMAKE_CTEST_COMMAND MATCHES " ")
    set(CMAKE_CTEST_COMMAND ctest)
endif()

execute_process(
    COMMAND ${CMAKE_CTEST_COMMAND} -V
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/build.debug/
    RESULT_VARIABLE TEST_RESULT
)

if(TEST_RESULT EQUAL 0)
    message(STATUS "Tests passed successfully!")
else()
    message(FATAL_ERROR "Tests failed!")
endif()
