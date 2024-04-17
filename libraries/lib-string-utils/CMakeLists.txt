#[[
String encoding and formatting utilities
]]

set(TARGET lib-string-utils)
set(TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

def_vars()

set(SOURCES
    HexHelpers.h

    StringUtils.h
    StringUtils.cpp

    UrlEncode.h
    UrlEncode.cpp

    UrlDecode.h
    UrlDecode.cpp

    CodeConversions.h
    CodeConversions.cpp

    DateTimeConversions.h
    DateTimeConversions.cpp

    ToChars.h
    ToChars.cpp

    FromChars.h
    FromChars.cpp

    UriParser.h
    UriParser.cpp
)

set( LIBRARIES
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
