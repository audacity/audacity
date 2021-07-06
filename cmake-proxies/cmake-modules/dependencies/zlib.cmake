# ZLib is a very popular library to use.
# Some of the dependecies do not check for the system libraries,
# which can be problematic.
# We need to call find_package once again.
if (${_OPT}use_zlib STREQUAL "system")
    message(STATUS "Fixing up ZLib mess...")
    
    find_package(ZLIB REQUIRED)

    set_target_properties(ZLIB::ZLIB PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${ZLIB_INCLUDE_DIRS}")

    if(ZLIB_LIBRARY_RELEASE)
        set_property(TARGET ZLIB::ZLIB APPEND PROPERTY
            IMPORTED_CONFIGURATIONS RELEASE)
        
        set_property(TARGET ZLIB::ZLIB
            PROPERTY INTERFACE_LINK_LIBRARIES 
            "${ZLIB_LIBRARY_RELEASE}"
        )
    endif()

    if(ZLIB_LIBRARY_DEBUG)
        set_property(TARGET ZLIB::ZLIB APPEND PROPERTY
            IMPORTED_CONFIGURATIONS DEBUG)

        set_property(TARGET ZLIB::ZLIB
            PROPERTY INTERFACE_LINK_LIBRARIES 
            "${ZLIB_LIBRARY_DEBUG}"
        )
    endif()

    if(NOT ZLIB_LIBRARY_RELEASE AND NOT ZLIB_LIBRARY_DEBUG)
        set_property(TARGET ZLIB::ZLIB
            PROPERTY INTERFACE_LINK_LIBRARIES 
            "${ZLIB_LIBRARY}"
        )
    endif()
endif()
