# portmidi
# Roger B. Dannenberg
# 20 Sep 2009

cmake_minimum_required(VERSION 2.6)

if(UNIX)
  # allow user to set Release or Debug
  set(CMAKE_BUILD_TYPE Release CACHE STRING 
      "Semicolon-separate list of supported configuration types")
  # set default directories but don't override cached values...
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CACHEFILE_DIR}/${CMAKE_BUILD_TYPE}
      CACHE STRING "libraries go here")
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CACHEFILE_DIR}/${CMAKE_BUILD_TYPE}
      CACHE STRING "libraries go here")
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY 
      ${CMAKE_CACHEFILE_DIR}/${CMAKE_BUILD_TYPE}
      CACHE STRING "executables go here")

else(UNIX)
  # this does not seem to work for xcode:
  set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING 
      "Semicolon-separate list of supported configuration types")
endif(UNIX)

#set(CMAKE_RELATIVE_PATHS ON CACHE STRING "avoid absolute paths" FORCE)

# Clear out the built-in C++ compiler and link flags for each of the 
# unused configurations.
set(CMAKE_CXX_FLAGS_MINSIZEREL "" CACHE INTERNAL "Unused")
set(CMAKE_SHARED_LINKER_FLAGS_MINSIZEREL "" CACHE INTERNAL "Unused")
set(CMAKE_MODULE_LINKER_FLAGS_MINSIZEREL "" CACHE INTERNAL "Unused")
set(CMAKE_EXE_LINKER_FLAGS_MINSIZEREL "" CACHE INTERNAL "Unused")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "" CACHE INTERNAL "Unused")
set(CMAKE_SHARED_LINKER_FLAGS_RELWITHDEBINFO "" CACHE INTERNAL "Unused")
set(CMAKE_MODULE_LINKER_FLAGS_RELWITHDEBINFO "" CACHE INTERNAL "Unused")
set(CMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO "" CACHE INTERNAL "Unused")

set(CMAKE_OSX_ARCHITECTURES i386 ppc x86_64 CACHE STRING "change to needed architecture for a smaller library" FORCE)

PROJECT(portmidi)

if(UNIX)
  # Problem: if there was an old Debug build and you change
  #  CMAKE_BUILD_TYPE to Release, then the OUTPUT_DIRECTORY's will
  #  still be Debug. Try to fix this by checking if the DIRECTORY's
  #  look wrong, and if so, force them to the defaults:
  if(CMAKE_BUILD_TYPE MATCHES "Debug")
    set(BAD_DIR "Release")
  else(CMAKE_BUILD_TYPE MATCHES "Debug")
    set(BAD_DIR "Debug")
  endif(CMAKE_BUILD_TYPE MATCHES "Debug")
  # use library as reference -- if you give it a non-BAD_DIR location
  # then every other path is left alone
  if(CMAKE_LIBRARY_OUTPUT_DIRECTORY MATCHES ${BAD_DIR})
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY 
        ${CMAKE_CACHEFILE_DIR}/${CMAKE_BUILD_TYPE}
        CACHE STRING "executables go here" FORCE)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY 
        ${CMAKE_CACHEFILE_DIR}/${CMAKE_BUILD_TYPE}
        CACHE STRING "libraries go here" FORCE)
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY 
        ${CMAKE_CACHEFILE_DIR}/${CMAKE_BUILD_TYPE}
        CACHE STRING "libraries go here" FORCE)
  endif(CMAKE_LIBRARY_OUTPUT_DIRECTORY MATCHES ${BAD_DIR})
endif(UNIX)

include_directories(pm_common porttime)
add_subdirectory(pm_common)

add_subdirectory(pm_test)

add_subdirectory(pm_dylib)

# Cannot figure out how to make an xcode Java application with CMake
add_subdirectory(pm_java)

