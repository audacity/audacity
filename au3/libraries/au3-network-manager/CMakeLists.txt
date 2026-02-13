#[[
Asynchronously send and receive requests and responses over internet
]]

set(TARGET au3-network-manager)

option(USE_QT_NETWORKMANAGER "Use Qt NetworkManager instead of libcurl for HTTP requests" ON)

set( SOURCES
    IResponse.h
    IResponse.cpp
    IResponseFactory.h

    NetworkManagerApi.h

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

    ThreadPool/ThreadPool.h
)

set ( LIBRARIES
    au3-string-utils-interface
    au3-preferences-interface
    au3-concurrency-interface
PRIVATE
    au3-exceptions-interface
    wxBase
)

set ( DEFINES INTERFACE "HAS_NETWORKING" PRIVATE "WIN32_LEAN_AND_MEAN" )

if(USE_QT_NETWORKMANAGER)
    set(CMAKE_AUTOMOC ON)

    list(APPEND SOURCES
        qt/Response.h
        qt/Response.cpp
        qt/ResponseFactory.h
        qt/ResponseFactory.cpp
    )
    list(APPEND LIBRARIES PRIVATE Qt::Network)
    list(APPEND DEFINES PRIVATE "USE_QT_NETWORKMANAGER")
else()
    list(APPEND SOURCES
        curl/CurlResponse.h
        curl/CurlResponse.cpp
        curl/CurlResponseFactory.h
        curl/CurlResponseFactory.cpp
        curl/CurlStringList.h
        curl/CurlStringList.cpp
        curl/CurlHandleManager.h
        curl/CurlHandleManager.cpp
    )
    list(APPEND LIBRARIES PRIVATE CURL::libcurl)
endif()

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
