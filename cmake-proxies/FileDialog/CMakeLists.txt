#directory lib-src/FileDialog
set( TARGET FileDialog )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
${LIB_SRC_DIRECTORY}FileDialog/FileDialog.cpp
#${LIB_SRC_DIRECTORY}FileDialog/gtk/FileDialogPrivate.cpp #not on windows.
${LIB_SRC_DIRECTORY}FileDialog/win/FileDialogPrivate.cpp
)

add_library( FileDialog STATIC ${SOURCES})
find_package(wxWidgets REQUIRED COMPONENTS net core base)
include( ${wxWidgets_USE_FILE} )
target_include_directories(  ${TARGET} PRIVATE 
#${wxWidgets_INCLUDE_DIRS}
${TARGET_SOURCE}
${TARGET_SOURCE}/win
)
add_definitions( -DWXUSINGDLL -DWIN32 -D_LIB 
#${wxWidgets_DEFINITIONS} 
)
target_link_libraries(${TARGET} ${wxWidgets_LIBRARIES})

