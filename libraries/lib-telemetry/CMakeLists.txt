set(TARGET lib-telemetry)
set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

def_vars()

set(SOURCES
    TelemetryApi.h
   
    TelemetryManager.h
    TelemetryManager.cpp

    details/ITelemetryService.h
    details/Event.h
    details/Event.cpp

    details/GoogleAnalyticsUA.h
    details/GoogleAnalyticsUA.cpp
)

add_library(${TARGET} SHARED ${SOURCES})

target_link_libraries(${TARGET} PRIVATE
    lib-uuid
    lib-string-utils
    lib-network-manager
    lib-timer
    wxWidgets
)

target_compile_definitions(${TARGET} PRIVATE
    BUILDING_LIB_TELEMETRY
    $<$<BOOL:${WIN32}>:NOMINMAX>
)

set_target_properties( ${TARGET}
   PROPERTIES
      PREFIX ""
      FOLDER "libraries"
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}")