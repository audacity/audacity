# SPDX-FileCopyrightText: 2014 Alex Merry <alex.merry@kde.org>
# SPDX-FileCopyrightText: 2012 Alexander Neundorf <neundorf@kde.org>
#
# SPDX-License-Identifier: BSD-3-Clause

#[=======================================================================[.rst:
ECMSetupVersion
---------------

Handle library version information.

::

  ecm_setup_version(<version>
                    VARIABLE_PREFIX <prefix>
                    [SOVERSION <soversion>]
                    [VERSION_HEADER <filename>]
                    [PACKAGE_VERSION_FILE <filename> [COMPATIBILITY <compat>]] )

This parses a version string and sets up a standard set of version variables.
It can optionally also create a C version header file and a CMake package
version file to install along with the library.

If the ``<version>`` argument is of the form ``<major>.<minor>.<patch>``
(or ``<major>.<minor>.<patch>.<tweak>``), The following CMake variables are
set::

  <prefix>_VERSION_MAJOR  - <major>
  <prefix>_VERSION_MINOR  - <minor>
  <prefix>_VERSION_PATCH  - <patch>
  <prefix>_VERSION        - <version>
  <prefix>_SOVERSION      - <soversion>, or <major> if SOVERSION was not given

For backward-compatibility also this variable is set (only if the minimum required
version of ECM is < 5.83)::

  <prefix>_VERSION_STRING - <version> (use <prefix>_VERSION instead)

If CMake policy CMP0048 is not NEW, the following CMake variables will also
be set::

  PROJECT_VERSION_MAJOR   - <major>
  PROJECT_VERSION_MINOR   - <minor>
  PROJECT_VERSION_PATCH   - <patch>
  PROJECT_VERSION         - <version>

For backward-compatibility, if CMake policy CMP0048 is not NEW, also this variable is set
(only if the minimum required version of ECM is < 5.83)::

  PROJECT_VERSION_STRING  - <version> (use PROJECT_VERSION instead)

If the VERSION_HEADER option is used, a simple C header is generated with the
given filename. If filename is a relative path, it is interpreted as relative
to CMAKE_CURRENT_BINARY_DIR.  The generated header contains the following
macros::

   <prefix>_VERSION_MAJOR  - <major> as an integer
   <prefix>_VERSION_MINOR  - <minor> as an integer
   <prefix>_VERSION_PATCH  - <patch> as an integer
   <prefix>_VERSION_STRING - <version> as a C string
   <prefix>_VERSION        - the version as an integer

``<prefix>_VERSION`` has ``<patch>`` in the bottom 8 bits, ``<minor>`` in the
next 8 bits and ``<major>`` in the remaining bits.  Note that ``<patch>`` and
``<minor>`` must be less than 256.

If the PACKAGE_VERSION_FILE option is used, a simple CMake package version
file is created using the write_basic_package_version_file() macro provided by
CMake. It should be installed in the same location as the Config.cmake file of
the library so that it can be found by find_package().  If the filename is a
relative path, it is interpreted as relative to CMAKE_CURRENT_BINARY_DIR. The
optional COMPATIBILITY option is forwarded to
write_basic_package_version_file(), and defaults to AnyNewerVersion.

If CMake policy CMP0048 is NEW, an alternative form of the command is
available::

  ecm_setup_version(PROJECT
                    [VARIABLE_PREFIX <prefix>]
                    [SOVERSION <soversion>]
                    [VERSION_HEADER <filename>]
                    [PACKAGE_VERSION_FILE <filename>] )

This will use the version information set by the project() command.
VARIABLE_PREFIX defaults to the project name.  Note that PROJECT must be the
first argument.  In all other respects, it behaves like the other form of the
command.

Since pre-1.0.0.

COMPATIBILITY option available since 1.6.0.
#]=======================================================================]

include(CMakePackageConfigHelpers)

# save the location of the header template while CMAKE_CURRENT_LIST_DIR
# has the value we want
set(_ECM_SETUP_VERSION_HEADER_TEMPLATE "${CMAKE_CURRENT_LIST_DIR}/ECMVersionHeader.h.in")

function(ecm_setup_version _version)
    set(options )
    set(oneValueArgs VARIABLE_PREFIX SOVERSION VERSION_HEADER PACKAGE_VERSION_FILE COMPATIBILITY)
    set(multiValueArgs )

    cmake_parse_arguments(ESV "${options}" "${oneValueArgs}" "${multiValueArgs}"  ${ARGN})

    if(ESV_UNPARSED_ARGUMENTS)
        message(FATAL_ERROR "Unknown keywords given to ECM_SETUP_VERSION(): \"${ESV_UNPARSED_ARGUMENTS}\"")
    endif()

    set(project_manages_version FALSE)
    set(use_project_version FALSE)
    cmake_policy(GET CMP0048 project_version_policy)
    if(project_version_policy STREQUAL "NEW")
        set(project_manages_version TRUE)
        if(_version STREQUAL "PROJECT")
            set(use_project_version TRUE)
        endif()
    elseif(_version STREQUAL "PROJECT")
        message(FATAL_ERROR "ecm_setup_version given PROJECT argument, but CMP0048 is not NEW")
    endif()

    set(should_set_prefixed_vars TRUE)
    if(NOT ESV_VARIABLE_PREFIX)
        if(use_project_version)
            set(ESV_VARIABLE_PREFIX "${PROJECT_NAME}")
            set(should_set_prefixed_vars FALSE)
        else()
            message(FATAL_ERROR "Required argument PREFIX missing in ECM_SETUP_VERSION() call")
        endif()
    endif()

    if(use_project_version)
        set(_version "${PROJECT_VERSION}")
        set(_major "${PROJECT_VERSION_MAJOR}")
        set(_minor "${PROJECT_VERSION_MINOR}")
        set(_patch "${PROJECT_VERSION_PATCH}")
    else()
        string(REGEX REPLACE "^0*([0-9]+)\\.[0-9]+\\.[0-9]+.*" "\\1" _major "${_version}")
        string(REGEX REPLACE "^[0-9]+\\.0*([0-9]+)\\.[0-9]+.*" "\\1" _minor "${_version}")
        string(REGEX REPLACE "^[0-9]+\\.[0-9]+\\.0*([0-9]+).*" "\\1" _patch "${_version}")
    endif()

    if(NOT ESV_SOVERSION)
        set(ESV_SOVERSION ${_major})
    endif()

    if(ECM_GLOBAL_FIND_VERSION VERSION_LESS 5.83.0)
        set(_set_backward_compat_version_string_vars TRUE)
    else()
        set(_set_backward_compat_version_string_vars FALSE)
    endif()

    if(should_set_prefixed_vars)
        set(${ESV_VARIABLE_PREFIX}_VERSION "${_version}")
        set(${ESV_VARIABLE_PREFIX}_VERSION_MAJOR ${_major})
        set(${ESV_VARIABLE_PREFIX}_VERSION_MINOR ${_minor})
        set(${ESV_VARIABLE_PREFIX}_VERSION_PATCH ${_patch})
    endif()

    set(${ESV_VARIABLE_PREFIX}_SOVERSION ${ESV_SOVERSION})

    if(NOT project_manages_version)
        set(PROJECT_VERSION "${_version}")
        set(PROJECT_VERSION_MAJOR "${_major}")
        set(PROJECT_VERSION_MINOR "${_minor}")
        set(PROJECT_VERSION_PATCH "${_patch}")
    endif()

    if(_set_backward_compat_version_string_vars)
        set(PROJECT_VERSION_STRING "${PROJECT_VERSION}")
        set(${ESV_VARIABLE_PREFIX}_VERSION_STRING "${${ESV_VARIABLE_PREFIX}_VERSION}")
    endif()

    if(ESV_VERSION_HEADER)
        set(HEADER_PREFIX "${ESV_VARIABLE_PREFIX}")
        set(HEADER_VERSION "${_version}")
        set(HEADER_VERSION_MAJOR "${_major}")
        set(HEADER_VERSION_MINOR "${_minor}")
        set(HEADER_VERSION_PATCH "${_patch}")
        configure_file("${_ECM_SETUP_VERSION_HEADER_TEMPLATE}" "${ESV_VERSION_HEADER}")
    endif()

    if(ESV_PACKAGE_VERSION_FILE)
        if(NOT ESV_COMPATIBILITY)
            set(ESV_COMPATIBILITY AnyNewerVersion)
        endif()
        write_basic_package_version_file("${ESV_PACKAGE_VERSION_FILE}" VERSION ${_version} COMPATIBILITY ${ESV_COMPATIBILITY})
    endif()

    if(should_set_prefixed_vars)
        set(${ESV_VARIABLE_PREFIX}_VERSION_MAJOR "${${ESV_VARIABLE_PREFIX}_VERSION_MAJOR}" PARENT_SCOPE)
        set(${ESV_VARIABLE_PREFIX}_VERSION_MINOR "${${ESV_VARIABLE_PREFIX}_VERSION_MINOR}" PARENT_SCOPE)
        set(${ESV_VARIABLE_PREFIX}_VERSION_PATCH "${${ESV_VARIABLE_PREFIX}_VERSION_PATCH}" PARENT_SCOPE)
        set(${ESV_VARIABLE_PREFIX}_VERSION       "${${ESV_VARIABLE_PREFIX}_VERSION}"       PARENT_SCOPE)
    endif()

    # always set the soversion
    set(${ESV_VARIABLE_PREFIX}_SOVERSION "${${ESV_VARIABLE_PREFIX}_SOVERSION}" PARENT_SCOPE)

    if(NOT project_manages_version)
        set(PROJECT_VERSION       "${PROJECT_VERSION}"       PARENT_SCOPE)
        set(PROJECT_VERSION_MAJOR "${PROJECT_VERSION_MAJOR}" PARENT_SCOPE)
        set(PROJECT_VERSION_MINOR "${PROJECT_VERSION_MINOR}" PARENT_SCOPE)
        set(PROJECT_VERSION_PATCH "${PROJECT_VERSION_PATCH}" PARENT_SCOPE)
    endif()

    if(_set_backward_compat_version_string_vars)
        set(PROJECT_VERSION_STRING "${PROJECT_VERSION_STRING}" PARENT_SCOPE)
        set(${ESV_VARIABLE_PREFIX}_VERSION_STRING "${${ESV_VARIABLE_PREFIX}_VERSION}" PARENT_SCOPE)
    endif()
endfunction()
