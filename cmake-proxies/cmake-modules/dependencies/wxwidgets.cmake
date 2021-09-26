if( ${_OPT}use_wxwidgets STREQUAL "system" OR NOT ${_OPT}conan_enabled )
    # DV: find_package will be scoped, as FindwxWidgets.cmake is rather outdated.
    # Still - let's perform the sanity check first.
    if( NOT wxWidgets_FOUND )
        find_package( wxWidgets REQUIRED COMPONENTS adv base core html qa xml net )
    endif()

    if( NOT TARGET wxwidgets::wxwidgets )
        add_library( wxwidgets::wxwidgets INTERFACE IMPORTED GLOBAL)
    endif()
    
    if( NOT TARGET wxwidgets::base )
        add_library( wxwidgets::base ALIAS wxwidgets::wxwidgets )
    endif()

    if( NOT TARGET wxwidgets::core )
        add_library( wxwidgets::core ALIAS wxwidgets::wxwidgets )
    endif()

    if( NOT TARGET wxwidgets::html )
        add_library( wxwidgets::html ALIAS wxwidgets::wxwidgets )
    endif()

    if( NOT TARGET wxwidgets::xml )
        add_library( wxwidgets::xml ALIAS wxwidgets::wxwidgets )
    endif()

    if( NOT TARGET wxwidgets::xrc )
        add_library( wxwidgets::xrc ALIAS wxwidgets::wxwidgets )
    endif()

    if( NOT TARGET wxwidgets::qa )
        add_library( wxwidgets::qa ALIAS wxwidgets::wxwidgets )
    endif()

    if( NOT TARGET wxwidgets::aui )
        add_library( wxwidgets::aui ALIAS wxwidgets::wxwidgets )
    endif()

    if( NOT TARGET wxwidgets::adv )
        add_library( wxwidgets::adv ALIAS wxwidgets::wxwidgets )
    endif()

    if( NOT TARGET wxBase )
        add_library( wxBase ALIAS wxwidgets::wxwidgets )
    endif()

    if( NOT TARGET wxwidgets::wxwidgets )
        add_library( wxwidgets::wxwidgets ALIAS wxwidgets::wxwidgets )
    endif()

    if( wxWidgets_INCLUDE_DIRS_NO_SYSTEM )
        target_include_directories( wxwidgets::wxwidgets INTERFACE ${wxWidgets_INCLUDE_DIRS_NO_SYSTEM} )
    else()
        target_include_directories( wxwidgets::wxwidgets INTERFACE ${wxWidgets_INCLUDE_DIRS} )
    endif() 

    target_compile_definitions( wxwidgets::wxwidgets INTERFACE
        ${wxWidgets_DEFINITIONS_GENERAL}
        $<$<CONFIG:Debug>:
            ${wxWidgets_DEFINITIONS_DEBUG}
        >
        $<$<NOT:$<CONFIG:Debug>>:
            ${wxWidgets_DEFINITIONS_OPTIMIZED}
        >
    )

    target_link_directories( wxwidgets::wxwidgets INTERFACE
        $<$<PLATFORM_ID:Windows>:
           ${wxWidgets_LIB_DIR}
        >
    )

    target_link_libraries( wxwidgets::wxwidgets INTERFACE
        ${wxWidgets_LIBRARIES}
        $<$<NOT:$<PLATFORM_ID:Windows>>:
           z
        >
    )

    set( toolkit "${wxWidgets_LIBRARIES}" )

    message(STATUS "Trying to retrieve GTK version from ${toolkit}")

    if( "${toolkit}" MATCHES ".*gtk2.*" )
        set( gtk gtk+-2.0 )
        set( glib glib-2.0 )
    elseif( "${toolkit}" MATCHES ".*gtk3.*" )
        set( gtk gtk+-3.0 )
        set( glib glib-2.0 )
    elseif( "${toolkit}" MATCHES ".*gtk4.*" )
        set( gtk gtk+-4.0 )
        set( glib glib-2.0 )
    endif()
else()
    set_target_properties(wxwidgets::base PROPERTIES IMPORTED_GLOBAL On)
    add_library( wxBase ALIAS wxwidgets::base )
endif()

if( NOT CMAKE_SYSTEM_NAME MATCHES "Windows|Darwin" )

    if( NOT DEFINED gtk )
        set( gtk gtk+-2.0 )
        set( glib glib-2.0 )
    endif()

    pkg_check_modules( GTK REQUIRED IMPORTED_TARGET GLOBAL ${gtk} )
    pkg_check_modules( GLIB REQUIRED IMPORTED_TARGET GLOBAL ${glib} )
endif()

