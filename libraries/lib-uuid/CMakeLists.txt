set(TARGET lib-uuid)
set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

def_vars()

set(SOURCES
    UuidApi.h
        
    Uuid.h
    Uuid.cpp
)

add_library(${TARGET} SHARED ${SOURCES})

target_compile_definitions(${TARGET} PRIVATE
    BUILDING_LIB_UUID
    $<$<BOOL:${WIN32}>:NOMINMAX>
)

if(WIN32)
    target_compile_definitions(${TARGET} PRIVATE USE_UUID_CREATE)
    target_link_libraries(${TARGET} PRIVATE Rpcrt4)
elseif(APPLE)
    target_compile_definitions(${TARGET} PRIVATE USE_CFUUID)

    find_library(CORE_FOUNDATION CoreFoundation)
    target_link_libraries(${TARGET} PRIVATE ${CORE_FOUNDATION})
else()
    find_package(PkgConfig REQUIRED)
    pkg_search_module(UUID REQUIRED uuid)

    if (NOT UUID_FOUND)
        message(FATAL_ERROR "You might need to run 'sudo apt-get install uuid-dev' or similar")
    endif()

    target_include_directories(${TARGET} PRIVATE ${UUID_INCLUDE_DIRS})
    target_link_libraries(${TARGET} ${UUID_LIBRARIES})
    target_compile_definitions(${TARGET} PRIVATE USE_LIBUUID)
endif()

set_target_properties( ${TARGET}
   PROPERTIES
      PREFIX ""
      FOLDER "libraries"
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}")
