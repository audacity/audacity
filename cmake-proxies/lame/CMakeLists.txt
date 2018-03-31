#directory cmake-proxies/lame
#UNUSED
set( TARGET lame )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
${TARGET_SOURCE}/lib/xmlparse.c
#${LIB_SRC_DIRECTORY}FileDialog/gtk/FileDialogPrivate.cpp #not on windows.
#${LIB_SRC_DIRECTORY}${TARGET}/win/FileDialogPrivate.cpp
)
# This defines the #define on both Windows and Linux.
add_definitions(  )
add_library( ${TARGET} MODULE ${SOURCES})

target_include_directories( ${TARGET} PRIVATE 

)

target_link_libraries( ${TARGET} )