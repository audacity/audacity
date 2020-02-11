
add_library( ${TARGET} STATIC )

def_vars()

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/NonGuiThread.cpp
)

list( APPEND INCLUDES
   PUBLIC
      ${TARGET_ROOT}
)

list( APPEND LIBRARIES
   PRIVATE
      wxWidgets
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

