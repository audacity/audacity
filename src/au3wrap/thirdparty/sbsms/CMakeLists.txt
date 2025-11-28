set(TARGET libsbsms)
add_library( ${TARGET} STATIC )

set(TARGET_ROOT ${AUDACITY_ROOT}/lib-src/libsbsms)
list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/src/buffer.cpp
      ${TARGET_ROOT}/src/dBTable.cpp
      ${TARGET_ROOT}/src/fft.cpp
      ${TARGET_ROOT}/src/grain.cpp
      ${TARGET_ROOT}/src/resample.cpp
      ${TARGET_ROOT}/src/sbsms.cpp
      ${TARGET_ROOT}/src/slide.cpp
      ${TARGET_ROOT}/src/sms.cpp
      ${TARGET_ROOT}/src/subband.cpp
      ${TARGET_ROOT}/src/track.cpp
      ${TARGET_ROOT}/src/trackpoint.cpp
)

list( APPEND INCLUDES
   PUBLIC
      ${TARGET_ROOT}/include
   PRIVATE
      ${CMAKE_CURRENT_BINARY_DIR}/private
)

list( APPEND DEFINES
   PUBLIC
      USE_SBSMS=1
)

# Check for headers
include(CheckIncludeFile)
check_include_file(dlfcn.h HAVE_DLFCN_H)
check_include_file(inttypes.h HAVE_INTTYPES_H)
check_include_file(memory.h HAVE_MEMORY_H)
check_include_file(stdint.h HAVE_STDINT_H)
check_include_file(stdlib.h HAVE_STDLIB_H)
check_include_file(strings.h HAVE_STRINGS_H)
check_include_file(string.h HAVE_STRING_H)
check_include_file(sys/stat.h HAVE_SYS_STAT_H)
check_include_file(sys/types.h HAVE_SYS_TYPES_H)
check_include_file(unistd.h HAVE_UNISTD_H)

# Check for lrint and lrintf functions
include(CheckCXXSourceCompiles)
# On Windows, don't require the math library
if(NOT WIN32)
    set(CMAKE_REQUIRED_LIBRARIES m)
endif()
check_cxx_source_compiles("
    #define _USE_MATH_DEFINES
    #include <cmath>
    int main() {
        long int x = lrint(3.14);
        return 0;
    }
" HAVE_LRINT)

check_cxx_source_compiles("
    #define _USE_MATH_DEFINES
    #include <cmath>
    int main() {
        long int x = lrintf(3.14f);
        return 0;
    }
" HAVE_LRINTF)
set(CMAKE_REQUIRED_LIBRARIES)

list( APPEND OPTIONS
   PRIVATE
      $<$<CXX_COMPILER_ID:AppleClang,Clang,GNU>:-w>
      $<$<CXX_COMPILER_ID:MSVC>:/w>
)

list( APPEND PROPERTIES
   POSITION_INDEPENDENT_CODE On
   CXX_STANDARD 14
   CXX_STANDARD_REQUIRED ON
)

configure_file( sbsms_config.h.in private/config.h )

target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
set_target_properties( ${TARGET} PROPERTIES ${PROPERTIES} )

