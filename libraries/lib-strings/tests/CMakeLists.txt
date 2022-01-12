set( SOURCES
   main.cpp
)

add_executable(test-lib-strings ${SOURCES})

add_test(
   # test NAME can have arbitrary characters in 3.19 and later
   # but we state CMake 3.16 as the minimum
   # name has no special meaning and can just be the same as the target

   NAME lib-strings
   # command can be a target, which means the test will build and
   # execute it; otherwise it can mean a shell command
   COMMAND test-lib-strings

   # Can restrict testing to only some configurations
   # CONFIGURATIONS Release
)
