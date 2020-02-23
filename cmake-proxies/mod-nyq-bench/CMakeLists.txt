
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

list( APPEND DEFINES
   PRIVATE
      # This is needed until the transition to cmake is complete and
      # the Windows pragmas are removed from NyqBench.cpp.  Without
      # it, the wxWidgets "debug.h" will define __WXDEBUG__ which
      # then causes this module to emit library pragmas for the debug
      # versions of wxWidgets...even if the build if for Release.
      wxDEBUG_LEVEL=0
)

list( APPEND LIBRARIES
   PRIVATE
      Audacity
      libnyquist
      portaudio-v19
      wxWidgets
)

set_target_property_all( ${TARGET} LIBRARY_OUTPUT_DIRECTORY "${_DEST}/modules" )
set_target_properties( ${TARGET}
   PROPERTIES
      PREFIX ""
      FOLDER "lib-src"
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

