set( SOURCES
   main.cpp
)

add_executable(test-lib-string-utils ${SOURCES})

add_test(
   # test NAME can have arbitrary characters in 3.19 and later
   # but we state CMake 3.16 as the minimum
   # name has no special meaning and can just be the same as the target

   NAME lib-string-utils
   # command can be a target, which means the test will build and
   # execute it; otherwise it can mean a shell command
   COMMAND test-lib-string-utils

   # Can restrict testing to only some configurations
   # CONFIGURATIONS Release
)
# Example of a test where success is defined by output
set_tests_properties(lib-string-utils PROPERTIES
   PASS_REGULAR_EXPRESSION "Hello"
)
