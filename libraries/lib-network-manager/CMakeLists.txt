set(TARGET lib-network-manager)
set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

def_vars()

set(SOURCES
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

    NetworkManagerApi.h

    curl/CurlResponse.h
    curl/CurlResponse.cpp
    curl/CurlResponseFactory.h
    curl/CurlResponseFactory.cpp
    curl/CurlStringList.h
    curl/CurlStringList.cpp
    curl/CurlHandleManager.h
    curl/CurlHandleManager.cpp
)

add_library(${TARGET} SHARED ${SOURCES})
add_dependencies( ${TARGET} curl)

target_link_libraries(${TARGET} PRIVATE
    AUDACITY::libcurl
    lib-string-utils
)

target_compile_definitions(${TARGET} PRIVATE
    BUILDING_LIB_NETWORK_MANAGER
    $<$<BOOL:${WIN32}>:NOMINMAX>
)

target_include_directories(${TARGET} PRIVATE "../../lib-src")

set_target_properties( ${TARGET}
   PROPERTIES
      PREFIX ""
      FOLDER "libraries"
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}")