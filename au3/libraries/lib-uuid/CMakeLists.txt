#[[
Generation of UUIDs
]]

set( TARGET lib-uuid )
set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

def_vars()

set( SOURCES
    Uuid.h
    Uuid.cpp
)

if(CMAKE_SYSTEM_NAME MATCHES "Windows")
    set( DEFINES PRIVATE USE_UUID_CREATE )
    set( LIBRARIES PRIVATE Rpcrt4 )
elseif(CMAKE_SYSTEM_NAME MATCHES "Darwin")
    find_library(CORE_FOUNDATION CoreFoundation)

    set( DEFINES PRIVATE USE_CFUUID )
    set( LIBRARIES PRIVATE ${CORE_FOUNDATION})
else()
    set( DEFINES PRIVATE USE_LIBUUID)
    set( LIBRARIES PRIVATE libuuid::libuuid)
endif()

list( APPEND LIBRARIES PRIVATE lib-string-utils lib-utility)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
