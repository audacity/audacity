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


set ( LIBRARIES PRIVATE
    CURL::libcurl
    ThreadPool::ThreadPool
    lib-string-utils
    wxwidgets::base
)

set ( DEFINES INTERFACE "HAS_NETWORKING" )


audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
