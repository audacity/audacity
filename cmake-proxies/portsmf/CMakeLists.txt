
add_library( ${TARGET} STATIC )

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
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_features( ${TARGET} PRIVATE ${FEATURES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

