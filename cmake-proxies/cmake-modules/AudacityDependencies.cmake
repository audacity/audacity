# Load Conan

if( ${_OPT}conan_enabled )
    include( conan )

    conan_add_remote(NAME audacity
        URL https://artifactory.audacityteam.org/artifactory/api/conan/conan-local
        VERIFY_SSL True
    )
endif()

if ( ${_OPT}conan_allow_prebuilt_binaries )
    set ( CONAN_BUILD_MODE BUILD missing )
else()
    set( CONAN_BUILD_MODE BUILD all )
endif()

set( CONAN_BUILD_REQUIRES )
set( CONAN_REQUIRES )
set( CONAN_PACKAGE_OPTIONS )
set( CONAN_ONLY_DEBUG_RELEASE )
set( CONAN_CONFIG_OPTIONS )
set( CONAN_RESOLVE_LIST )

#[[
Add a Conan dependency

Example usage:

add_conan_lib(
  wxWdidget
  wxwidgets/3.1.3-audacity
  OPTION_NAME wxwidgets
  SYMBOL WXWIDGET
  REQUIRED
  ALWAYS_ALLOW_CONAN_FALLBACK
  PKG_CONFIG "wxwidgets >= 3.1.3"
  FIND_PACKAGE_OPTIONS COMPONENTS adv base core html qa xml
  INTERFACE_NAME wxwidgets::wxwidgets
  HAS_ONLY_DEBUG_RELEASE
  CONAN_OPTIONS
       wxwidgets:shared=True
)

PKG_CONFIG accepts a list of possible package configurations.
add_conan_lib will iterate over it one by one until the library is found.
]]

function (add_conan_lib package conan_package_name )
    # Extract the list of packages from the function args
    list( SUBLIST ARGV 2 -1 options )

    set( list_mode on )
    set( current_var "conan_package_options" )

    set( option_name_base ${package} )
    set( allow_find_package off )
    set( find_package_options )
    set( conan_package_options )
    set( required off )
    set( pkg_config_options )
    set( system_only ${${_OPT}obey_system_dependencies})
    set( interface_name "${package}::${package}")

    # Parse function arguments

    foreach( opt IN LISTS options )
        if( opt STREQUAL "FIND_PACKAGE_OPTIONS" )
            set( list_mode on )
            set( allow_find_package on )
            set( current_var "find_package_options" )
        elseif ( opt STREQUAL "ALLOW_FIND_PACKAGE" )
            set ( allow_find_package on )
        elseif ( opt STREQUAL "CONAN_OPTIONS" )
            set( list_mode on )
            set( current_var "conan_package_options" )
        elseif ( opt STREQUAL "PKG_CONFIG" )
            set( list_mode on )
            set( current_var "pkg_config_options" )
        elseif ( opt STREQUAL "OPTION_NAME" )
            set( list_mode off )
            set( current_var "option_name_base" )
        elseif ( opt STREQUAL "SYMBOL" )
            set( list_mode off )
            set( current_var "symbol" )
        elseif ( opt STREQUAL "INTERFACE_NAME" )
            set( list_mode off )
            set( current_var "interface_name" )
        elseif ( opt STREQUAL "REQUIRED" )
            set( required on )
        elseif ( opt STREQUAL "ALWAYS_ALLOW_CONAN_FALLBACK" )
            set( system_only off )
        elseif ( opt STREQUAL "HAS_ONLY_DEBUG_RELEASE" )
            set ( only_debug_release on )
        else()
            if( list_mode )
                list( APPEND ${current_var} ${opt} )
            else()
                set (${current_var} ${opt})
            endif()
        endif()
    endforeach()

    if( NOT DEFINED symbol )
        string( TOUPPER "${option_name_base}" symbol)
    endif()

    # Generate CMake option
    set( option_name ${_OPT}use_${option_name_base} )

    set( option_desc "local" )

    if( pkg_config_options OR allow_find_package OR NOT ${_OPT}conan_enabled )
        set( sysopt "system" )
        string( PREPEND option_desc "system (if available), " )

        if( ${_OPT}conan_enabled )
            set( default "${${_OPT}lib_preference}" )
        else()
            set( default "system" )
        endif()
    else()
        set( default "local" )
    endif()

    if( ${_OPT}conan_enabled )
        set( localopt "local" )
    endif()

    if( NOT required )
        set( reqopt "off" )
        string( APPEND option_desc ", off" )
    endif()

    cmd_option( ${option_name}
                "Use ${option_name_base} library [${option_desc}]"
                "${default}"
                STRINGS ${sysopt} ${localopt} ${reqopt}
    )

    # Early bail out
    if( ${option_name} STREQUAL "off" )

      message( STATUS "========== ${option_name_base} disabled ==========" )

      set( USE_${symbol} OFF CACHE INTERNAL "" FORCE )

      return()
    endif()

    # Let the Audacity target know that this library will be used
    set( USE_${symbol} ON CACHE INTERNAL "" FORCE )

    if ( TARGET "${package}" )
        return()
    endif()

    if( ${option_name} STREQUAL "system" OR NOT ${_OPT}conan_enabled )
        if( pkg_config_options )
            foreach(variant ${pkg_config_options})
                pkg_check_modules( PKG_${package} ${variant} )

                if( PKG_${package}_FOUND )
                    message( STATUS "Using '${package}' system library" )

                    # Create the target interface library
                    add_library( ${interface_name} INTERFACE IMPORTED GLOBAL)

                    # Retrieve the package information
                    get_package_interface( PKG_${package} ${interface_name} )
                    return()
                endif()
            endforeach()
        endif()

        if( allow_find_package )
            find_package( ${package} QUIET ${find_package_options} )

            if ( ${package}_FOUND )
                message( STATUS "Using '${package}' system library" )
                return()
            endif()
        endif()

        if( system_only OR NOT ${_OPT}conan_enabled )
            message( FATAL_ERROR "Failed to find the system package ${package}" )
        else()
            set( ${option_name} "local" )
            set_property( CACHE ${option_name} PROPERTY VALUE "local" )
        endif()
    endif()

    list( APPEND CONAN_REQUIRES ${conan_package_name} )
    list( APPEND CONAN_PACKAGE_OPTIONS ${conan_package_options} )
    list( APPEND CONAN_RESOLVE_LIST ${package} )

    if ( only_debug_release )
        message( STATUS "${package} only has Debug and Release versions" )
        list( APPEND CONAN_ONLY_DEBUG_RELEASE ${package})
    endif()

    set( CONAN_REQUIRES           ${CONAN_REQUIRES}           PARENT_SCOPE )
    set( CONAN_PACKAGE_OPTIONS    ${CONAN_PACKAGE_OPTIONS}    PARENT_SCOPE )
    set( CONAN_RESOLVE_LIST       ${CONAN_RESOLVE_LIST}       PARENT_SCOPE )
    set( CONAN_ONLY_DEBUG_RELEASE ${CONAN_ONLY_DEBUG_RELEASE} PARENT_SCOPE )

    message (STATUS "Adding Conan dependency ${package}")
endfunction()

macro( set_conan_vars_to_parent )
    set( CONAN_REQUIRES        ${CONAN_REQUIRES}        PARENT_SCOPE )
    set( CONAN_PACKAGE_OPTIONS ${CONAN_PACKAGE_OPTIONS} PARENT_SCOPE )
    set( CONAN_RESOLVE_LIST    ${CONAN_RESOLVE_LIST}    PARENT_SCOPE )
    set( CONAN_BUILD_REQUIRES  ${CONAN_BUILD_REQUIRES}  PARENT_SCOPE )
    set( CONAN_ONLY_DEBUG_RELEASE ${CONAN_ONLY_DEBUG_RELEASE} PARENT_SCOPE )
endmacro()

function ( _conan_install build_type )
    conan_cmake_configure (
        REQUIRES ${CONAN_REQUIRES}
        GENERATORS cmake_find_package_multi
        BUILD_REQUIRES ${CONAN_BUILD_REQUIRES}
        ${CONAN_CONFIG_OPTIONS}
        IMPORTS "bin, *.dll -> ./${_SHARED_PROXY_BASE}/${build_type} @ keep_path=False"
        IMPORTS "lib, *.dll -> ./${_SHARED_PROXY_BASE}/${build_type} @ keep_path=False"
        IMPORTS "lib, *.dylib -> ./${_SHARED_PROXY_BASE}/${build_type} @ keep_path=False"
        IMPORTS "lib, *.so* -> ./${_SHARED_PROXY_BASE}/${build_type} @ keep_path=False"
        OPTIONS ${CONAN_PACKAGE_OPTIONS}
    )

    message(STATUS "Configuring packages for ${build_type}")

    conan_cmake_autodetect(settings BUILD_TYPE ${build_type})

    if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
        # TODO: Read the target CPU architecture from the CMake option
        # We have no AppleSilicon support yet
        list( APPEND settings "arch=x86_64" )
        list( APPEND settings "os.version=${CMAKE_OSX_DEPLOYMENT_TARGET}" )
        # This line is required to workaround the conan bug #8025
        # https://github.com/conan-io/conan/issues/8025
        # Without it, libjpeg-turbo will fail to cross-compile on AppleSilicon macs
        list( APPEND settings ENV "CONAN_CMAKE_SYSTEM_PROCESSOR=x86_64")
    endif()

    if (build_type MATCHES "MinSizeRel|RelWithDebInfo")
        message(STATUS "Release only libraries: ${CONAN_ONLY_DEBUG_RELEASE}")

        foreach( package ${CONAN_ONLY_DEBUG_RELEASE} )
            list( APPEND settings "${package}:build_type=Release")
        endforeach()
    endif()


    conan_cmake_install(PATH_OR_REFERENCE .
        ${CONAN_BUILD_MODE}
        SETTINGS ${settings}
    )
endfunction()

macro( resolve_conan_dependencies )
    if( ${_OPT}conan_enabled )
        message(STATUS
        "Executing Conan: \
            REQUIRES ${CONAN_REQUIRES}
            GENERATORS cmake_find_package_multi
            BUILD_REQUIRES ${CONAN_BUILD_REQUIRES}
            ${CONAN_CONFIG_OPTIONS}
            OPTIONS ${CONAN_PACKAGE_OPTIONS}
        ")

        if(MSVC OR XCODE)
            foreach(TYPE ${CMAKE_CONFIGURATION_TYPES})
                _conan_install(${TYPE})
            endforeach()
        else()
            _conan_install(${CMAKE_BUILD_TYPE})
        endif()

        list( REMOVE_DUPLICATES CONAN_REQUIRES )

        foreach( package ${CONAN_RESOLVE_LIST} )
            message(STATUS "Resolving Conan library ${package}")

            find_package(${package} CONFIG)
            mark_as_advanced(${package}_DIR)

            if (NOT ${package}_FOUND)
                message( FATAL_ERROR "Failed to find the conan package ${package}" )
            endif()
        endforeach()
    endif()

    file(GLOB dependency_helpers "${AUDACITY_MODULE_PATH}/dependencies/*.cmake")

    foreach(f ${dependency_helpers})
        include(${f})
    endforeach()
endmacro()

macro ( find_required_package package_name system_package_name )
    find_package ( ${package_name} QUIET ${ARGN} )

    if ( NOT ${package_name}_FOUND )
        if (CMAKE_SYSTEM_NAME MATCHES "Darwin|Windows")
            message( FATAL_ERROR "Error: ${package_name} is required")
        else()
            message( FATAL_ERROR "Error: ${package_name} is required.\nPlease install it with using command like:\n\t\$ sudo apt install ${system_package_name}" )
        endif()
    endif()
endmacro()
