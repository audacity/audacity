
add_library( ${TARGET} STATIC )

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/FileDialog.cpp
      $<$<PLATFORM_ID:Windows>:${TARGET_ROOT}/win/FileDialogPrivate.cpp>
      $<$<PLATFORM_ID:Darwin>:${TARGET_ROOT}/mac/FileDialogPrivate.mm>
      $<$<PLATFORM_ID:Linux>:${TARGET_ROOT}/gtk/FileDialogPrivate.cpp>
)

list( APPEND INCLUDES
   PUBLIC
      ${TARGET_ROOT}
)

list( APPEND DEFINES
   PRIVATE
      $<$<PLATFORM_ID:Windows>:__WIN32__>
)

list( APPEND OPTIONS
   PRIVATE
      $<$<PLATFORM_ID:Windows>:/permissive->
      $<$<PLATFORM_ID:Darwin>:-Wno-deprecated-declarations>
      $<$<PLATFORM_ID:Linux>:-Wno-deprecated-declarations>
)

list( APPEND LIBRARIES
   PRIVATE
      wxwidgets
   PUBLIC
      $<$<PLATFORM_ID:Linux>:PkgConfig::GTK>
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

