
add_library( ${TARGET} STATIC )

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
   # redirect inclusions to our wrapper header
   INTERFACE
      ${CMAKE_CURRENT_SOURCE_DIR}
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )

