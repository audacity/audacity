#directory cmake-proxies/portburn
set( TARGET portburn )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
${LIB_SRC_DIRECTORY}portburn/portburn_macosx.cpp
${LIB_SRC_DIRECTORY}portburn/portburn_staging.cpp
${LIB_SRC_DIRECTORY}portburn/portburn_winxp.cpp
${LIB_SRC_DIRECTORY}portburn/portburn_winxpv1.cpp
${LIB_SRC_DIRECTORY}portburn/portburn_winxpv2.cpp
${LIB_SRC_DIRECTORY}portburn/test_portburn.cpp

#${LIB_SRC_DIRECTORY}portburn/portburn_macosx.c
${LIB_SRC_DIRECTORY}portburn/portburn_staging.c

)
# This defines the #define on both Windows and Linux.
add_definitions(  )
add_library( ${TARGET} STATIC ${SOURCES})

target_include_directories( ${TARGET} PRIVATE 

)

target_link_libraries( ${TARGET} )