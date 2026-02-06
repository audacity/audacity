# SPDX-License-Identifier: GPL-3.0-only
# MuseScore-Studio-CLA-applies
#
# MuseScore Studio
# Music Composition & Notation
#
# Copyright (C) 2026 MuseScore Limited
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.

# - Registers the given path as QML import path for QtCreator
macro(add_qml_import_path import_path)
    if (NOT ${import_path} STREQUAL "")
        set(QML_IMPORT_PATH "$CACHE{QML_IMPORT_PATH}")
        list(APPEND QML_IMPORT_PATH ${import_path})
        list(REMOVE_DUPLICATES QML_IMPORT_PATH)
        set(QML_IMPORT_PATH "${QML_IMPORT_PATH}" CACHE STRING
            "QtCreator extra import paths for QML modules" FORCE)
    endif()
endmacro()

# - Creates a target and sets up common properties for Muse modules
function(muse_create_module target_name)
    set(options NO_QT NO_PCH NO_UNITY STUB)
    set(oneValueArgs ALIAS)
    cmake_parse_arguments(PARSE_ARGV 1 arg "${options}" "${oneValueArgs}" "")

    # Status message
    set(message "Configuring ${target_name}")
    if (arg_ALIAS)
        set(message "${message} <${arg_ALIAS}>")
    endif()
    if (arg_STUB)
        set(message "${message} [stub]")
    endif()
    message(STATUS "${message}")

    # Create target
    if (NOT arg_NO_QT AND MUSE_QT_SUPPORT)
        # STATIC/SHARED based on BUILD_SHARED_LIBS, which is set in SetupBuildEnvironment.cmake
        qt_add_library(${target_name})
    else()
        # STATIC/SHARED based on BUILD_SHARED_LIBS, which is set in SetupBuildEnvironment.cmake
        add_library(${target_name})

        set_target_properties(${target_name} PROPERTIES
            AUTOMOC OFF
            AUTOUIC OFF
            AUTORCC OFF
        )
    endif()

    # Alias target
    if (arg_ALIAS)
        add_library(${arg_ALIAS} ALIAS ${target_name})
    endif()

    # Include directories
    if (NOT MUSE_FRAMEWORK_PATH)
        set(MUSE_FRAMEWORK_PATH ${PROJECT_SOURCE_DIR})
    endif()

    target_include_directories(${target_name} PRIVATE
        ${PROJECT_BINARY_DIR}
        ${CMAKE_CURRENT_BINARY_DIR}
        ${CMAKE_CURRENT_SOURCE_DIR}

        ${PROJECT_SOURCE_DIR}/src

        ${MUSE_FRAMEWORK_PATH}
        ${MUSE_FRAMEWORK_PATH}/framework
        ${MUSE_FRAMEWORK_PATH}/framework/global
        ${MUSE_FRAMEWORK_PATH}/framework/testing/thirdparty/googletest/googletest/include

        # compat
        ${MUSE_FRAMEWORK_PATH}/src
        ${MUSE_FRAMEWORK_PATH}/src/framework
        ${MUSE_FRAMEWORK_PATH}/src/framework/global
        ${MUSE_FRAMEWORK_PATH}/src/framework/testing/thirdparty/googletest/googletest/include
        # end compat
    )

    # Precompiled header
    if (NOT arg_NO_PCH AND MUSE_COMPILE_USE_PCH)
        if (${target_name} STREQUAL muse_global)
            target_precompile_headers_clang_ccache(${target_name} PRIVATE ${MUSE_FRAMEWORK_PATH}/buildscripts/pch/pch.h)
        else()
            target_precompile_headers_clang_ccache(${target_name} REUSE_FROM muse_global)
            target_compile_definitions(${target_name} PRIVATE muse_global_EXPORTS=1)
        endif()
    endif()

    # Unity build
    if (arg_NO_UNITY)
        set_target_properties(${target_name} PROPERTIES UNITY_BUILD OFF)
    elseif(MUSE_COMPILE_USE_UNITY)
        set_target_properties(${target_name} PROPERTIES UNITY_BUILD ON)
    endif()

    # Code coverage
    if (MUSE_ENABLE_UNIT_TESTS_CODE_COVERAGE AND MODULE_USE_COVERAGE)
        set(COVERAGE_FLAGS -fprofile-arcs -ftest-coverage --coverage)
        target_compile_options(${target_name} PRIVATE ${COVERAGE_FLAGS})
        target_link_options(${target_name} PRIVATE -lgcov --coverage -fprofile-arcs -ftest-coverage)
    endif()

    # Link with global module
    if (NOT ${target_name} STREQUAL muse_global)
        target_link_libraries(${target_name} PRIVATE muse_global)
    endif()
endfunction()

function(muse_create_qml_module target_name)
    set(oneValueArgs FOR)
    cmake_parse_arguments(PARSE_ARGV 1 arg "" "${oneValueArgs}" "")

    muse_create_module(${target_name} ${arg_UNPARSED_ARGUMENTS})
    target_link_libraries(${target_name} INTERFACE ${target_name}plugin)

    if (arg_FOR)
        get_target_property(_for_dir ${arg_FOR} SOURCE_DIR)
        target_include_directories(${target_name} PRIVATE ${_for_dir})

        target_link_libraries(${target_name} PRIVATE ${arg_FOR})

        # This might not be the cleanest way to obtain this path, but it is a
        # good balance between simplicity and correctness
        get_target_property(_for_binary_dir ${arg_FOR} BINARY_DIR)
        add_qml_import_path(${_for_binary_dir}/qml)
    endif()
endfunction()

# - Creates a target and sets up common properties for Muse thirdparty modules
function(muse_create_thirdparty_module target_name)
    set(oneValueArgs ALIAS)
    cmake_parse_arguments(PARSE_ARGV 1 arg "" "${oneValueArgs}" "")

    # Status message
    set(message "Configuring ${target_name}")
    if (arg_ALIAS)
        set(message "${message} <${arg_ALIAS}>")
    endif()
    message(STATUS "${message}")

    # Create target
    # STATIC/SHARED based on BUILD_SHARED_LIBS, which is set in SetupBuildEnvironment.cmake
    add_library(${target_name})

    set_target_properties(${target_name} PROPERTIES
        AUTOMOC OFF
        AUTOUIC OFF
        AUTORCC OFF
    )

    # Alias target
    if (arg_ALIAS)
        add_library(${arg_ALIAS} ALIAS ${target_name})
    endif()
endfunction()

function(muse_module_add_qrc target_name)
    if (NOT MUSE_QT_SUPPORT)
        message(WARNING "Building without Qt support, cannot add QRC to target ${target_name}")
        return()
    endif()

    set(options BIG_RESOURCES)
    cmake_parse_arguments(PARSE_ARGV 1 arg "${options}" "" "")

    if (arg_BIG_RESOURCES)
        qt_add_big_resources(QRC_SOURCES ${arg_UNPARSED_ARGUMENTS})
    else()
        qt_add_resources(QRC_SOURCES ${arg_UNPARSED_ARGUMENTS})
    endif()

    target_sources(${target_name} PRIVATE ${QRC_SOURCES})
endfunction()

function(fixup_qml_module_dependencies target_name)
    if (CMAKE_GENERATOR MATCHES "Visual Studio")
        # The Visual Studio generator doesn't correctly resolve the dependencies for "qmltyperegistration"
        # generated code files, unless we add this explicit target-level dependency. Other generators
        # don't seem to have this problem.
        add_dependencies(${target_name}_qmltyperegistration ${target_name})
    endif()
endfunction()

### LEGACY MACROS

## Declare
# declare_module(somename) - set module (target) name

## Setup
# set(MODULE somename)                        - set module (target) name
# set(MODULE_ALIAS somename)                  - set module (target) alias name
# set(MODULE_INCLUDE ...)                     - set include (by default see below include_directories)
# set(MODULE_INCLUDE_PRIVATE ...)             - set private include
# set(MODULE_DEF ...)                         - set definitions
# set(MODULE_DEF_PRIVATE ...)                 - set private definitions
# set(MODULE_SRC ...)                         - set sources and headers files
# set(MODULE_LINK ...)                        - set libraries for link
# set(MODULE_LINK_PUBLIC ...)                 - set libraries for link and transitive link
# set(MODULE_QRC somename.qrc)                - set resource (qrc) file
# set(MODULE_BIG_QRC somename.qrc)            - set big resource (qrc) file
# set(MODULE_QML_IMPORT ...)                  - set Qml import for QtCreator (so that there is code highlighting, jump, etc.)
# set(MODULE_QMLAPI_IMPORT ...)               - set Qml api import for QtCreator (so that there is code highlighting, jump, etc.)
# set(MODULE_QMLEXT_IMPORT ...)               - set Qml extensions import for QtCreator (so that there is code highlighting, jump, etc.)
# set(MODULE_USE_PCH ON/OFF)                  - set whether to use precompiled headers for this module (default ON)
# set(MODULE_USE_UNITY ON/OFF)                - set whether to use unity build for this module (default ON)
# set(MODULE_USE_COVERAGE ON)                 - set whether to use coverage for this module (default ON)
# set(MODULE_IS_STUB ON)                      - set a mark that the module is stub

# After all the settings you need to do:
# setup_module()

macro(declare_module name)
    set(MODULE ${name})
    # just reset all settings
    unset(MODULE_ALIAS)
    unset(MODULE_INCLUDE)
    unset(MODULE_DEF)
    unset(MODULE_SRC)
    unset(MODULE_LINK)
    unset(MODULE_LINK_PUBLIC)
    set(MODULE_USE_QT ON)
    unset(MODULE_QRC)
    unset(MODULE_BIG_QRC)
    unset(MODULE_QML_IMPORT)
    unset(MODULE_QMLAPI_IMPORT)
    unset(MODULE_QMLEXT_IMPORT)
    set(MODULE_USE_PCH ON)
    set(MODULE_USE_UNITY ON)
    unset(MODULE_IS_STUB)
    set(MODULE_USE_COVERAGE ON)
endmacro()

macro(add_qml_import_path_if_not_empty input_var)
    if (${input_var})
        add_qml_import_path(${${input_var}})
    endif()
endmacro()

function(target_precompile_headers_clang_ccache target)
    target_precompile_headers(${target} ${ARGN})

    # https://discourse.cmake.org/t/ccache-clang-and-fno-pch-timestamp/7253
    if (CC_IS_CLANG AND COMPILER_CACHE_PROGRAM)
        target_compile_options(${target} PRIVATE
            "$<$<COMPILE_LANGUAGE:CXX>:SHELL:-Xclang -fno-pch-timestamp>"
        )
    endif()
endfunction()

macro(setup_module)
    set(ARGS)

    if (NOT MODULE_USE_QT)
        list(APPEND ARGS NO_QT)
    endif()

    if (MODULE_IS_STUB)
        list(APPEND ARGS STUB)
    endif()

    if (MODULE_ALIAS)
        list(APPEND ARGS ALIAS ${MODULE_ALIAS})
    endif()

    if (NOT MODULE_USE_PCH)
        list(APPEND ARGS NO_PCH)
    endif()

    if (NOT MODULE_USE_UNITY)
        list(APPEND ARGS NO_UNITY)
    endif()

    muse_create_module(${MODULE} ${ARGS})

    target_sources(${MODULE} PRIVATE ${MODULE_SRC})

    if (MODULE_USE_QT AND MUSE_QT_SUPPORT)
        if (MODULE_QRC)
            qt_add_resources(RCC_SOURCES ${MODULE_QRC})
            target_sources(${MODULE} PRIVATE ${RCC_SOURCES})
        endif()

        if (MODULE_BIG_QRC)
            qt_add_big_resources(RCC_BIG_SOURCES ${MODULE_BIG_QRC})
            target_sources(${MODULE} PRIVATE ${RCC_BIG_SOURCES})
        endif()
    endif()

    add_qml_import_path_if_not_empty(MODULE_QML_IMPORT)
    add_qml_import_path_if_not_empty(MODULE_QMLAPI_IMPORT)
    add_qml_import_path_if_not_empty(MODULE_QMLEXT_IMPORT)

    if (BUILD_SHARED_LIBS)
        install(TARGETS ${MODULE} DESTINATION ${SHARED_LIBS_INSTALL_DESTINATION})
    endif()

    if (NOT MUSE_FRAMEWORK_PATH)
        set(MUSE_FRAMEWORK_PATH ${PROJECT_SOURCE_DIR})
    endif()

    target_include_directories(${MODULE}
        PRIVATE ${MODULE_INCLUDE_PRIVATE}
        PUBLIC ${MODULE_INCLUDE}
    )

    target_compile_definitions(${MODULE} PUBLIC
        ${MODULE_DEF}
        ${MODULE}_QML_IMPORT="${MODULE_QML_IMPORT}"
    )

    target_compile_definitions(${MODULE} PRIVATE
        ${MODULE_DEF_PRIVATE}
    )

    target_link_libraries(${MODULE}
        PRIVATE ${MODULE_LINK}
        PUBLIC ${MODULE_LINK_PUBLIC}
    )
endmacro()
