#directory cmake-proxies/libogg
#UNUSED
set( TARGET libogg )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
${LIB_SRC_DIRECTORY}libogg/src/bitwise.c
${LIB_SRC_DIRECTORY}libogg/src/framing.c
)
# This defines the #define on both Windows and Linux.
add_definitions(  )
add_library( ${TARGET} MODULE ${SOURCES})

target_include_directories( ${TARGET} PRIVATE 

)

target_link_libraries( ${TARGET} )