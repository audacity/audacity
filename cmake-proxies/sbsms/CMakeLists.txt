
add_library( ${TARGET} STATIC )

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
   PRIVATE
      ${CMAKE_CURRENT_BINARY_DIR}/private
   PUBLIC
      ${TARGET_ROOT}/include
)

list( APPEND OPTIONS
   PRIVATE
      $<$<C_COMPILER_ID:CLANG>:-Wno-enum-compare>
      $<$<C_COMPILER_ID:GNU>:-Wno-enum-compare>
)

find_package( Threads )
if( Threads_FOUND AND CMAKE_USE_PTHREADS_INIT )
   set( MULTITHREADED 1 )
endif()

if( UNIX )
   check_cxx_compiler_flag( "-msse" ENABLE_SSE )
   if( ENABLE_SSE )
      list( APPEND OPTIONS
         PRIVATE -msse
      )
   endif()
endif()

configure_file( config.h.in private/config.h )

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_features( ${TARGET} PRIVATE ${FEATURES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

