set(TARGET lib-timer)
set( TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

def_vars()

set(SOURCES
    TimerApi.h
        
    Timer.h
    Timer.cpp
)

add_library(${TARGET} SHARED ${SOURCES})

target_compile_definitions(${TARGET} PRIVATE
    BUILDING_LIB_TIMER
    $<$<BOOL:${WIN32}>:NOMINMAX>
)

target_link_libraries(${TARGET} PRIVATE wxWidgets)

set_target_properties( ${TARGET}
   PROPERTIES
      PREFIX ""
      FOLDER "libraries"
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}")