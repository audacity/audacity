
add_library( ${TARGET} STATIC )

def_vars()

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/FileDialog.cpp
      $<$<BOOL:${wxIS_WIN}>:${TARGET_ROOT}/win/FileDialogPrivate.cpp>
      $<$<BOOL:${wxIS_MAC}>:${TARGET_ROOT}/mac/FileDialogPrivate.mm>
      $<$<BOOL:${wxIS_GTK}>:${TARGET_ROOT}/gtk/FileDialogPrivate.cpp>
)

list( APPEND INCLUDES
   PUBLIC
      ${TARGET_ROOT}
)

list( APPEND DEFINES
   PRIVATE
      wxDEBUG_LEVEL=0
      $<$<PLATFORM_ID:Windows>:__WIN32__>
)

list( APPEND OPTIONS
   PRIVATE
      $<$<CXX_COMPILER_ID:MSVC>:/permissive->
      $<$<CXX_COMPILER_ID:AppleClang,Clang,GNU>:-Wno-deprecated-declarations>
)

list( APPEND LIBRARIES
   PRIVATE
      wxWidgets
   PUBLIC
      $<$<BOOL:${wxIS_GTK}>:PkgConfig::GTK>
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

