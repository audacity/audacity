#[[
Asynchronously send and receive requests and responses over internet
]]

set(TARGET au3-network-manager)
set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

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
    au3-string-utils-interface
    au3-preferences-interface
    au3-concurrency-interface
PRIVATE
    CURL::libcurl
    threadpool::threadpool
    au3-exceptions-interface
)

set ( DEFINES INTERFACE "HAS_NETWORKING" PRIVATE "WIN32_LEAN_AND_MEAN" )


audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
