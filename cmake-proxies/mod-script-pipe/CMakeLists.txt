
set( TARGET mod-script-pipe )
set( TARGET_ROOT "${libsrc}/${TARGET}" )

message( STATUS "========== Configuring ${TARGET} ==========" )

def_vars()

add_library( ${TARGET} MODULE )

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/PipeServer.cpp
      ${TARGET_ROOT}/ScripterCallback.cpp
)

list( APPEND INCLUDES
   PUBLIC
      ${TARGET_ROOT}
)

list( APPEND DEFINES
   PRIVATE
      BUILDING_SCRIPT_PIPE
)

list( APPEND LIBRARIES
   PRIVATE
      wxwidgets
      Audacity
)

set_target_properties( ${TARGET}
   PROPERTIES
      LIBRARY_OUTPUT_DIRECTORY_${CMAKE_BUILD_TYPE} "${_DEST}/modules"
      LIBRARY_OUTPUT_DIRECTORY "${_DEST}/modules"
      PREFIX ""
      FOLDER "lib-src"
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

