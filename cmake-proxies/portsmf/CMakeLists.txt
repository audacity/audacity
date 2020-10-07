
add_library( ${TARGET} OBJECT )

def_vars()

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/allegro.cpp
      ${TARGET_ROOT}/allegrord.cpp
      ${TARGET_ROOT}/allegroserial.cpp
      ${TARGET_ROOT}/allegrosmfrd.cpp
      ${TARGET_ROOT}/allegrosmfwr.cpp
      ${TARGET_ROOT}/allegrowr.cpp
      ${TARGET_ROOT}/mfmidi.cpp
      ${TARGET_ROOT}/strparse.cpp
)

list( APPEND INCLUDES
   PUBLIC
      ${TARGET_ROOT}/include
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )

if( CMAKE_CXX_COMPILER_ID STREQUAL "GNU" )
   target_compile_options( ${TARGET} PRIVATE -fPIC )
endif()
