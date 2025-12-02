# **********************************************************************
#
#  Audacity: A Digital Audio Editor
#
#  au3defs.cmake
#
#  AU3 CMake compatibility layer for AU4
#
#  This file provides macros and functions to allow AU3 library
#  CMakeLists.txt files to work in the AU4 build system with minimal
#  modifications.
#
# **********************************************************************

include(GetPlatformInfo)
include(GNUInstallDirs)

# Plugin directory paths (needed by LV2 and other plugin loaders)
# These are shared by all AU3 libraries and au3wrap module
if(NOT DEFINED _PKGLIBDIR)
    # _PKGLIBDIR is not defined in AU4, so we use a reasonable default
    set(_PKGLIBDIR "${CMAKE_INSTALL_PREFIX}/lib/audacity")
endif()
set(PKGLIBDIR "${_PKGLIBDIR}")
set(LIBDIR "${CMAKE_INSTALL_FULL_LIBDIR}")

# Expose only the GUI-less subset of full wxWidgets
# Also prohibit use of some other headers by pre-defining their include guards
# wxUSE_GUI=0 doesn't exclude all of wxCore dependency, and the application
# object and event loops are in wxBase, but we want to exclude their use too
set( WXBASE_RESTRICTIONS
   "wxUSE_GUI=0"

   # Don't use app.h
   _WX_APP_H_BASE_

   # Don't use evtloop.h
   _WX_EVTLOOP_H_

   # Don't use image.h
   _WX_IMAGE_H

   # Don't use colour.h
   _WX_COLOUR_H_BASE_

   # Don't use brush.h
   _WX_BRUSH_H_BASE_

   # Don't use pen.h
   _WX_PEN_H_BASE_
)

function( apply_wxbase_restrictions target )
   target_compile_definitions( ${target} PRIVATE ${WXBASE_RESTRICTIONS} )
endfunction()

# Audacity version information
# Shared by all AU3 libraries and au3wrap module to avoid duplication
set( AUDACITY_VERSION_DEFS
    -DAUDACITY_VERSION=4
    -DAUDACITY_RELEASE=0
    -DAUDACITY_REVISION=0
    -DAUDACITY_MODLEVEL=0

    # Version string for visual display
    -DAUDACITY_VERSION_STRING=L"${AUDACITY_VERSION}.${AUDACITY_RELEASE}.${AUDACITY_REVISION}${AUDACITY_SUFFIX}"

    # This value is used in the resource compiler for Windows
    -DAUDACITY_FILE_VERSION=L"${AUDACITY_VERSION},${AUDACITY_RELEASE},${AUDACITY_REVISION},${AUDACITY_MODLEVEL}"
)

# Platform-specific wxWidgets definitions
# Shared by all AU3 libraries and au3wrap module to avoid duplication
if(OS_IS_LIN)
    set(WXPLATFORM_DEFS -D__WXGTK__)
elseif(OS_IS_MAC)
    set(WXPLATFORM_DEFS -D__WXMAC__)
elseif(OS_IS_WIN)
    set(WXPLATFORM_DEFS -D__WXMSW__ -DWXUSINGDLL)
endif()

# Function to compute API export symbol name from module name
# e.g. "au3-basic-ui" -> "BASIC_UI_API"
function(import_export_symbol var module_name)
   # Remove au3- prefix and convert to uppercase with underscores
   string(REGEX REPLACE "^au3-" "" symbol "${module_name}")
   string(TOUPPER "${symbol}" symbol)
   string(REPLACE "-" "_" symbol "${symbol}")
   string(APPEND symbol "_API")
   set("${var}" "${symbol}" PARENT_SCOPE)
endfunction()

# Generate API definitions for all AU3 libraries
# This is shared by all AU3 libraries and au3wrap module to avoid duplication
# Note: AU3_ALL_LIBRARIES is set by au3/libraries/CMakeLists.txt
set(AU3_API_DEFS "")
if(DEFINED AU3_ALL_LIBRARIES)
    foreach(lib ${AU3_ALL_LIBRARIES})
        import_export_symbol(api_symbol "${lib}")
        list(APPEND AU3_API_DEFS -D${api_symbol}=)
    endforeach()
endif()

# Macro to define an AU3 library
# Usage: audacity_library(NAME SOURCES IMPORT_TARGETS ADDITIONAL_DEFINES ADDITIONAL_LIBRARIES)
# This matches the signature from AU3's AudacityFunctions.cmake
#
# Parameters:
#   NAME - Name of the library target (e.g., "au3-math")
#   SOURCES - List of source files
#   IMPORT_TARGETS - List of libraries to link against
#   ADDITIONAL_DEFINES - Additional compile definitions (usually empty)
#   ADDITIONAL_LIBRARIES - Additional libraries to link (usually empty)
#
macro(audacity_library NAME SOURCES IMPORT_TARGETS ADDITIONAL_DEFINES ADDITIONAL_LIBRARIES)
    # Use the NAME directly as the target name (already in au3-* format)
    set(au3_target_name "${NAME}")

    # Create the library
    add_library(${au3_target_name} STATIC ${SOURCES})

    # Parse the IMPORT_TARGETS list to separate PUBLIC, PRIVATE, and INTERFACE
    set(_public_libs "")
    set(_private_libs "")
    set(_interface_libs "")
    set(_current_scope "public")

    foreach(lib ${IMPORT_TARGETS})
        if(lib STREQUAL "PUBLIC")
            set(_current_scope "public")
        elseif(lib STREQUAL "PRIVATE")
            set(_current_scope "private")
        elseif(lib STREQUAL "INTERFACE")
            set(_current_scope "interface")
        else()
            # Check if this is an interface library (e.g., au3-utility-interface)
            string(REGEX MATCH "-interface$" is_interface "${lib}")

            if(is_interface)
                # Remove -interface suffix and add as INTERFACE dependency
                # This gives us include paths without linking the library
                string(REPLACE "-interface" "" lib_without_interface "${lib}")
                list(APPEND _interface_libs ${lib_without_interface})
            else()
                # Regular library - add to current scope
                list(APPEND _${_current_scope}_libs ${lib})
            endif()
        endif()
    endforeach()

    # Set up include directories
    target_include_directories(${au3_target_name}
        PUBLIC
            # Allow namespaced includes like: #include "au3-math/SampleFormat.h"
            ${CMAKE_CURRENT_SOURCE_DIR}/..
            # For internal includes within the library
            ${CMAKE_CURRENT_SOURCE_DIR}
    )

    # Add PRIVATE include directories for AU3 library dependencies
    # This allows the library to include headers from its dependencies
    set(_private_includes "")
    if(_private_includes)
        target_include_directories(${au3_target_name} PRIVATE ${_private_includes})
    endif()

    # Link libraries with proper scopes
    if(_public_libs)
        target_link_libraries(${au3_target_name} PUBLIC ${_public_libs})
    endif()
    if(_private_libs)
        target_link_libraries(${au3_target_name} PRIVATE ${_private_libs})
    endif()
    # Note: _interface_libs are handled by adding their include directories in the
    # PRIVATE includes section above. We don't actually link them.

    # Add ADDITIONAL_LIBRARIES if provided
    # Note: In macros, we need to check the actual value, not just the parameter name
    if(NOT "${ADDITIONAL_LIBRARIES}" STREQUAL "")
        target_link_libraries(${au3_target_name} ${ADDITIONAL_LIBRARIES})
    endif()

    # Get the API name from target name (e.g., au3-math -> MATH_API)
    import_export_symbol(api_symbol "${NAME}")

    # Set up compile definitions
    set(_public_defs "")
    set(_private_defs "")
    list(APPEND _public_defs
        # Empty API export macro for static libraries
        -D${api_symbol}=
    )

    # Parse ADDITIONAL_DEFINES to separate PUBLIC, PRIVATE, and INTERFACE
    if(NOT "${ADDITIONAL_DEFINES}" STREQUAL "")
        set(_current_def_scope "PUBLIC")
        foreach(item ${ADDITIONAL_DEFINES})
            if(item STREQUAL "PUBLIC" OR item STREQUAL "PRIVATE" OR item STREQUAL "INTERFACE")
                set(_current_def_scope ${item})
            else()
                if(_current_def_scope STREQUAL "PUBLIC")
                    list(APPEND _public_defs ${item})
                elseif(_current_def_scope STREQUAL "PRIVATE")
                    list(APPEND _private_defs ${item})
                endif()
                # INTERFACE defines are ignored for static libraries
            endif()
        endforeach()
    endif()

    if(_public_defs)
        target_compile_definitions(${au3_target_name} PUBLIC ${_public_defs})
    endif()
    if(_private_defs)
        target_compile_definitions(${au3_target_name} PRIVATE ${_private_defs})
    endif()

    # Apply wxBase restrictions if library links to wxBase
    #
    # wxBase is a toolkit-neutral subset of wxWidgets without GUI components.
    # The restrictions are applied as PRIVATE compile definitions so they only
    # affect this library's compilation, not its consumers. This allows libraries
    # to choose between:
    # - wxBase: toolkit-neutral, no GUI (gets these restrictions)
    # - wxwidgets::wxwidgets: full wxWidgets with GUI (no restrictions)
    set(_uses_wxbase FALSE)
    foreach(lib ${_public_libs} ${_private_libs})
        if(lib STREQUAL "wxBase")
            set(_uses_wxbase TRUE)
            break()
        endif()
    endforeach()

    if(_uses_wxbase)
        apply_wxbase_restrictions(${au3_target_name})
    endif()

    # Define API macros for ALL known AU3 libraries (shared variable defined at top of file)
    # This is necessary because AU3 libraries can include headers from transitive dependencies
    set(_private_defs "")
    list(APPEND _private_defs ${AU3_API_DEFS})

    # Audacity version information (shared variable defined at top of file)
    list(APPEND _private_defs ${AUDACITY_VERSION_DEFS})

    # Plugin directory paths (needed by LV2 and other plugin loaders)
    # PKGLIBDIR and LIBDIR are defined at the top of this file
    list(APPEND _private_defs
        -DPKGLIBDIR="${PKGLIBDIR}"
        -DLIBDIR="${LIBDIR}"
    )

    if(_private_defs)
        target_compile_definitions(${au3_target_name} PRIVATE ${_private_defs})
    endif()

    # Platform-specific wxWidgets definitions (shared variable defined at top of file)
    if(WXPLATFORM_DEFS)
        target_compile_definitions(${au3_target_name} PUBLIC ${WXPLATFORM_DEFS})
    endif()
    
    # Disable warnings for AU3 code
    target_no_warning(${au3_target_name} -w)

    # Note: AU3 tests are disabled for now as Catch2 is not set up in AU4
    # TODO: Add Catch2 support and enable tests
    # if(MUSE_ENABLE_UNIT_TESTS AND EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/tests/CMakeLists.txt")
    #     add_subdirectory(tests)
    # endif()
endmacro()

# Macro to add a unit test for an AU3 library
# Usage: add_unit_test(NAME name SOURCES sources... LIBRARIES libraries...)
#
# Parameters:
#   NAME - Name of the test (e.g., "au3-math")
#   SOURCES - List of test source files
#   LIBRARIES - List of libraries to link against
#
macro(add_unit_test)
    # Parse arguments
    set(options "")
    set(oneValueArgs NAME)
    set(multiValueArgs SOURCES LIBRARIES)
    cmake_parse_arguments(TEST "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

    if(NOT MUSE_ENABLE_UNIT_TESTS)
        return()
    endif()

    # Use the NAME directly (already in au3-* format) and append _tests
    set(test_target_name "${TEST_NAME}_tests")

    # Create the test executable
    add_executable(${test_target_name} ${TEST_SOURCES})

    # Link against Catch2 and the specified libraries
    target_link_libraries(${test_target_name}
        PRIVATE
            ${TEST_LIBRARIES}
            Catch2::Catch2
    )

    # Add include directories
    target_include_directories(${test_target_name}
        PRIVATE
            ${CMAKE_CURRENT_SOURCE_DIR}/..
    )

    # Disable warnings for AU3 test code
    target_no_warning(${test_target_name} -w)

    # Add the test to CTest
    add_test(NAME ${test_target_name} COMMAND ${test_target_name})
endmacro()
