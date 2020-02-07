
set( TARGET mod-nyq-bench )
set( TARGET_ROOT "${libsrc}/${TARGET}" )

message( STATUS "========== Configuring ${TARGET} ==========" )

def_vars()

add_library( ${TARGET} MODULE )

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/NyqBench.cpp
      ${TARGET_ROOT}/NyqBench.h
)

list( APPEND INCLUDES
   PUBLIC
      ${TARGET_ROOT}
)

list( APPEND LIBRARIES
   PRIVATE
      Audacity
      libnyquist
      portaudio-v19
      wxwidgets
)

set_target_property_all( ${TARGET} LIBRARY_OUTPUT_DIRECTORY "${_DEST}/modules" )
set_target_properties( ${TARGET}
   PROPERTIES
      PREFIX ""
      FOLDER "lib-src"
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

