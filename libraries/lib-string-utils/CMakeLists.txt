set(TARGET lib-string-utils)
set(TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

def_vars()

set(SOURCES
    HexHelpers.h
    
    UrlEncode.h
    UrlEncode.cpp

    UrlDecode.h
    UrlDecode.cpp

    CodeConversions.h
    CodeConversions.cpp

    DateTimeConversions.h
    DateTimeConversions.cpp
)

set( LIBRARIES
   PRIVATE
   wxwidgets::base
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
