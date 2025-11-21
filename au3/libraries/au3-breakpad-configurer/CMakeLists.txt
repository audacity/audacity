#[[
This library provides an interface to configure and start Breakpad handler 
in a platform independent way.
]]

set(SOURCES 
   BreakpadConfigurer.h
   BreakpadConfigurer.cpp
)
set(LIBRARIES 
   PRIVATE
      breakpad::client
)
set(DEFINES
   INTERFACE
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

audacity_library( lib-breakpad-configurer "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
