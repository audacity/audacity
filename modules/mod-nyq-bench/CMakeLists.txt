
set( TARGET mod-nyq-bench )
set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

message( STATUS "========== Configuring ${TARGET} ==========" )

def_vars()

add_library( ${TARGET} MODULE EXCLUDE_FROM_ALL )

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/NyqBench.cpp
      ${TARGET_ROOT}/NyqBench.h
)

get_target_property( wx_INCLUDES wxWidgets INTERFACE_INCLUDE_DIRECTORIES )
get_target_property( pa_INCLUDES portaudio-v19 INTERFACE_INCLUDE_DIRECTORIES )
get_target_property( ny_INCLUDES libnyquist INTERFACE_INCLUDE_DIRECTORIES )
list( APPEND INCLUDES
   PUBLIC
      ${wx_INCLUDES}
      ${pa_INCLUDES}
      ${ny_INCLUDES}
      ${TARGET_ROOT}
)

get_target_property( DEFINES wxWidgets INTERFACE_COMPILE_DEFINITIONS )
list( APPEND DEFINES
   PRIVATE
      # This is needed until the transition to cmake is complete and
      # the Windows pragmas are removed from NyqBench.cpp.  Without
      # it, the wxWidgets "debug.h" will define __WXDEBUG__ which
      # then causes this module to emit library pragmas for the debug
      # versions of wxWidgets...even if the build is for Release.
      wxDEBUG_LEVEL=0
)

list( APPEND LOPTS
   PRIVATE
      $<$<PLATFORM_ID:Darwin>:-undefined dynamic_lookup>
)

list( APPEND LIBRARIES
   PRIVATE
      Audacity
      $<$<PLATFORM_ID:Windows>:wxWidgets>
)

set_target_property_all( ${TARGET} LIBRARY_OUTPUT_DIRECTORY "${_MODDIR}" )
set_target_properties( ${TARGET}
   PROPERTIES
      PREFIX ""
      FOLDER "modules"
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_options( ${TARGET} PRIVATE ${LOPTS} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

