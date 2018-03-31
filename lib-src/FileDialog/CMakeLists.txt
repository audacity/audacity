#directory lib-src/FileDialog


set( SOURCES 
${LIB_SRC_DIRECTORY}FileDialog/FileDialog.cpp
#${LIB_SRC_DIRECTORY}FileDialog/gtk/FileDialogPrivate.cpp #not on windows.
${LIB_SRC_DIRECTORY}FileDialog/win/FileDialogPrivate.cpp
)


project (FileDialog)
add_library( FileDialog STATIC ${SOURCES})
find_package(wxWidgets REQUIRED COMPONENTS net core base)
include( ${wxWidgets_USE_FILE} )
target_include_directories( FileDialog PRIVATE
#${wxWidgets_INCLUDE_DIRS}
${top_dir}/FileDialog
${top_dir}/FileDialog/win
)
add_definitions( -DWXUSINGDLL -DWIN32 -D_LIB 
#${wxWidgets_DEFINITIONS} 
)
target_link_libraries(FileDialog ${wxWidgets_LIBRARIES})