#directory cmake-proxies/libid3tag
set( TARGET libid3tag )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
${LIB_SRC_DIRECTORY}libid3tag/compat.c
${LIB_SRC_DIRECTORY}libid3tag/crc.c
${LIB_SRC_DIRECTORY}libid3tag/debug.c
${LIB_SRC_DIRECTORY}libid3tag/field.c
${LIB_SRC_DIRECTORY}libid3tag/file.c
${LIB_SRC_DIRECTORY}libid3tag/frame.c
${LIB_SRC_DIRECTORY}libid3tag/frametype.c
${LIB_SRC_DIRECTORY}libid3tag/genre.c
${LIB_SRC_DIRECTORY}libid3tag/latin1.c
${LIB_SRC_DIRECTORY}libid3tag/parse.c
${LIB_SRC_DIRECTORY}libid3tag/render.c
${LIB_SRC_DIRECTORY}libid3tag/tag.c
${LIB_SRC_DIRECTORY}libid3tag/ucs4.c
${LIB_SRC_DIRECTORY}libid3tag/utf16.c
${LIB_SRC_DIRECTORY}libid3tag/utf8.c
${LIB_SRC_DIRECTORY}libid3tag/util.c
${LIB_SRC_DIRECTORY}libid3tag/version.c
)
# This defines the #define on both Windows and Linux.
add_definitions( 
-D_LIB
)
add_library( ${TARGET} STATIC ${SOURCES})

find_package(wxWidgets REQUIRED COMPONENTS net core base)
#include( ${wxWidgets_USE_FILE} )

target_include_directories( ${TARGET} PRIVATE 
${wxWidgets_ROOT_DIR}/src/zlib
)

target_link_libraries( ${TARGET} )