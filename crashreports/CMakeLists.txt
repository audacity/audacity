# This module provides an interface to configure and start Breakpad handler 
# in a platform independent way.

set(TARGET crashreports)
set(TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR})

message( STATUS "========== Configuring ${TARGET} ==========" )

add_library(${TARGET} STATIC)

set(SOURCES "")
set(INCLUDES INTERFACE ./)
set(LIBRARIES "")
set(DEFINIES "")

# also adding Crash Reporting dialog
add_subdirectory(crashreporter)

list(APPEND SOURCES
    PRIVATE
        BreakpadConfigurer.h
        BreakpadConfigurer.cpp
)
list(APPEND LIBRARIES
    PRIVATE
        breakpad::client
)
list(APPEND DEFINES
    PUBLIC
        -DUSE_BREAKPAD
    PRIVATE 
        -DCRASHREPORTER_PROGRAM_NAME="$<TARGET_FILE_NAME:crashreporter>"
)

if(WIN32)
    list(APPEND SOURCES
        PRIVATE
            internal/win32/CrashReportContext.h
            internal/win32/CrashReportContext.cpp
    )
elseif(UNIX)
    list(APPEND SOURCES
        PRIVATE
            internal/unix/CrashReportContext.h
            internal/unix/CrashReportContext.cpp)
endif()

target_include_directories(${TARGET} ${INCLUDES})
target_sources(${TARGET} ${SOURCES})
target_link_libraries(${TARGET} ${LIBRARIES})
target_compile_definitions(${TARGET} ${DEFINES})

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )