set(TARGET lib-string-utils)
set(TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

def_vars()

set(SOURCES
    StringUtilsApi.h
        
    HexHelpers.h
    
    UrlEncode.h
    UrlEncode.cpp

    UrlDecode.h
    UrlDecode.cpp

    CodeConversions.h
    CodeConversions.cpp
)

add_library(${TARGET} SHARED ${SOURCES})

target_compile_definitions(${TARGET} PRIVATE
    BUILDING_LIB_STRING_UTILS
    $<$<BOOL:${WIN32}>:NOMINMAX>
)

set_target_properties( ${TARGET}
   PROPERTIES
      PREFIX ""
      FOLDER "libraries"
)

target_link_libraries( ${TARGET} PRIVATE 
    wxWidgets
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}")