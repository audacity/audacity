#[[
Generation of UUIDs
]]

set( TARGET au3-uuid )
set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

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

list( APPEND LIBRARIES PRIVATE au3-string-utils au3-utility)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
