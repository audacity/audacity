#[[
Asynchronously send and receive requests and responses over internet
]]

set(TARGET lib-network-manager)
set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

def_vars()

set( SOURCES
    IResponse.h
    IResponseFactory.h

    HeadersList.h
    HeadersList.cpp

    CookiesList.h
    CookiesList.cpp

    Request.h
    Request.cpp

    RequestPayload.h
    RequestPayload.cpp

    MultipartData.h
    MultipartData.cpp

    NetworkManager.h
    NetworkManager.cpp

    curl/CurlResponse.h
    curl/CurlResponse.cpp
    curl/CurlResponseFactory.h
    curl/CurlResponseFactory.cpp
    curl/CurlStringList.h
    curl/CurlStringList.cpp
    curl/CurlHandleManager.h
    curl/CurlHandleManager.cpp
)


set ( LIBRARIES
    lib-string-utils-interface
    lib-preferences-interface
    lib-concurrency-interface
PRIVATE
    CURL::libcurl
    threadpool::threadpool
    lib-exceptions-interface
)

set ( DEFINES INTERFACE "HAS_NETWORKING" PRIVATE "WIN32_LEAN_AND_MEAN" )


audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
