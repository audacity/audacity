
find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)

    include(GetCompilerInfo)

    if (CC_IS_MINGW)

        set(ENV{CCACHE_CPP2} true)
        set(CMAKE_C_COMPILER_LAUNCHER   "${CCACHE_PROGRAM}")
        set(CMAKE_CXX_COMPILER_LAUNCHER "${CCACHE_PROGRAM}")
        message(STATUS "Using ccache")

    elseif(CC_IS_MSVC)

        # not worked

    elseif(CC_IS_CLANG)

       if (CMAKE_GENERATOR STREQUAL "Xcode")

           # Set up wrapper scripts
           set(C_LAUNCHER   "${CCACHE_PROGRAM}")
           set(CXX_LAUNCHER "${CCACHE_PROGRAM}")
           configure_file(${PROJECT_SOURCE_DIR}/build/launch-c.in   launch-c)
           configure_file(${PROJECT_SOURCE_DIR}/build/launch-cxx.in launch-cxx)
           execute_process(COMMAND chmod a+rx
                            "${CMAKE_BINARY_DIR}/launch-c"
                            "${CMAKE_BINARY_DIR}/launch-cxx"
           )

           # Set Xcode project attributes to route compilation and linking
           # through our scripts
           set(CMAKE_XCODE_ATTRIBUTE_CC         "${CMAKE_BINARY_DIR}/launch-c")
           set(CMAKE_XCODE_ATTRIBUTE_CXX        "${CMAKE_BINARY_DIR}/launch-cxx")
           set(CMAKE_XCODE_ATTRIBUTE_LD         "${CMAKE_BINARY_DIR}/launch-c")
           set(CMAKE_XCODE_ATTRIBUTE_LDPLUSPLUS "${CMAKE_BINARY_DIR}/launch-cxx")
           message(STATUS "Using ccache")
       else()
           set(ENV{CCACHE_CPP2} true)
           set(CMAKE_C_COMPILER_LAUNCHER   "${CCACHE_PROGRAM}")
           set(CMAKE_CXX_COMPILER_LAUNCHER "${CCACHE_PROGRAM}")
           message(STATUS "Using ccache")
       endif()

    elseif(CC_IS_GCC)

        # Support Unix Makefiles and Ninja
        set(ENV{CCACHE_CPP2} true)
        set(CMAKE_C_COMPILER_LAUNCHER   "${CCACHE_PROGRAM}")
        set(CMAKE_CXX_COMPILER_LAUNCHER "${CCACHE_PROGRAM}")
        message(STATUS "Using ccache")

    endif()

endif(CCACHE_PROGRAM)
