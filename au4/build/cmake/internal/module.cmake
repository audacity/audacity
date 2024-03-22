# SPDX-License-Identifier: GPL-3.0-only
# MuseScore-CLA-applies
#
# MuseScore
# Music Composition & Notation
#
# Copyright (C) 2022 MuseScore BVBA and others
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
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

## Setup
# set(MODULE somename)                        - set module (target) name
# set(MODULE_ROOT ${CMAKE_CURRENT_LIST_DIR})  - set module root
# set(MODULE_INCLUDE ...)                     - set include (by default see below include_directories)
# set(MODULE_DEF ...)                         - set definitions
# set(MODULE_SRC ...)                         - set sources and headers files
# set(MODULE_LINK ...)                        - set libraries for link
# set(MODULE_NOT_LINK_GLOBAL ON)              - set for not link global lib
# set(MODULE_QRC somename.qrc)                - set resource (qrc) file
# set(MODULE_BIG_QRC somename.qrc)            - set big resource (qrc) file
# set(MODULE_UI ...)                          - set ui headers
# set(MODULE_QML_IMPORT ...)                  - set Qml import for QtCreator (so that there is code highlighting, jump, etc.)
# set(MODULE_QMLEXT_IMPORT ...)               - set Qml extensions import for QtCreator (so that there is code highlighting, jump, etc.)
# set(MODULE_USE_PCH_NONE ON)                 - set for disable PCH for module
# set(MODULE_USE_UNITY_NONE ON)               - set for disable UNITY BUILD for module
# set(MODULE_OVERRIDDEN_PCH ...)              - set additional precompiled headers required for module
# set(PROJECT_ROOT_DIR ${PROJECT_SOURCE_DIR}) - set root dir for module
# set(MODULE_IS_STUB ON)                      - set a mark that the module is stub

# After all the settings you need to do:
# include(${PROJECT_SOURCE_DIR}/build/module.cmake)

if (MODULE_IS_STUB)
    message(STATUS "Configuring " ${MODULE} " [stub]")
else()
    message(STATUS "Configuring " ${MODULE})
endif()

if (NOT PROJECT_ROOT_DIR)
    set(PROJECT_ROOT_DIR ${PROJECT_SOURCE_DIR})
endif()

if (MODULE_QRC AND NOT NO_QT_SUPPORT)
    if (MUE_COMPILE_QT5_COMPAT)
        qt5_add_resources(RCC_SOURCES ${MODULE_QRC})
    else()
        qt_add_resources(RCC_SOURCES ${MODULE_QRC})
    endif()
endif()

if (MODULE_BIG_QRC AND NOT NO_QT_SUPPORT)
    if (MUE_COMPILE_QT5_COMPAT)
        qt5_add_big_resources(RCC_BIG_SOURCES ${MODULE_BIG_QRC})
    else()
        qt_add_big_resources(RCC_BIG_SOURCES ${MODULE_BIG_QRC})
    endif()
endif()

if (MODULE_UI)
    if (MUE_COMPILE_QT5_COMPAT)
        find_package(Qt5Widgets)
        QT5_WRAP_UI(ui_headers ${MODULE_UI} )
    else()
        find_package(Qt6Widgets)
        QT6_WRAP_UI(ui_headers ${MODULE_UI} )
    endif()
endif()

if (NOT ${MODULE_QML_IMPORT} STREQUAL "")
    set(QML_IMPORT_PATH "${QML_IMPORT_PATH};${MODULE_QML_IMPORT}" CACHE STRING "QtCreator extra import paths for QML modules" FORCE)
endif()

if (NOT ${MODULE_QMLAPI_IMPORT} STREQUAL "")
    set(QML_IMPORT_PATH "${QML_IMPORT_PATH};${MODULE_QMLAPI_IMPORT}" CACHE STRING "QtCreator extra import paths for QML modules" FORCE)
endif()

if (CC_IS_EMSCRIPTEN)
    add_library(${MODULE} OBJECT)
else()
    add_library(${MODULE}) # STATIC/SHARED set global in the SetupBuildEnvironment.cmake
endif()

if (BUILD_SHARED_LIBS)
    install(TARGETS ${MODULE} DESTINATION ${SHARED_LIBS_INSTALL_DESTINATION})

    if (NOT MSVC)
        set_target_properties(${MODULE} PROPERTIES COMPILE_FLAGS "-fPIC")
    endif (NOT MSVC)
endif()

if (MUE_COMPILE_USE_PCH)
    if (MODULE_USE_PCH_NONE)
        # disabled pch for current module
    else()
        if (NOT ${MODULE} MATCHES global)
            if (NOT DEFINED MODULE_OVERRIDDEN_PCH)
                target_precompile_headers(${MODULE} REUSE_FROM global)
                target_compile_definitions(${MODULE} PRIVATE global_EXPORTS=1)
            else()
                target_precompile_headers(${MODULE} PRIVATE ${MODULE_OVERRIDDEN_PCH})
            endif()
            if (MODULE_NOT_LINK_GLOBAL)
                set(MODULE_NOT_LINK_GLOBAL OFF)
            endif()
        else()
            target_precompile_headers(${MODULE} PRIVATE ${PROJECT_SOURCE_DIR}/build/pch/pch.h)
        endif()
    endif()
endif(MUE_COMPILE_USE_PCH)

if (MUE_COMPILE_USE_UNITY)
    if (MODULE_USE_UNITY_NONE)
        # disabled unity build for current module
        set_target_properties(${MODULE} PROPERTIES UNITY_BUILD OFF)
    else()
        set_target_properties(${MODULE} PROPERTIES UNITY_BUILD ON)
    endif()
endif(MUE_COMPILE_USE_UNITY)

target_sources(${MODULE} PRIVATE
    ${ui_headers}
    ${RCC_SOURCES}
    ${RCC_BIG_SOURCES}
    ${MODULE_SRC}
    )

target_include_directories(${MODULE} PUBLIC
    ${PROJECT_BINARY_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
    ${MODULE_ROOT}
    ${PROJECT_ROOT_DIR}
    ${PROJECT_ROOT_DIR}/src
    ${PROJECT_ROOT_DIR}/src/framework
    ${PROJECT_ROOT_DIR}/src/framework/global
    ${PROJECT_ROOT_DIR}/src/engraving
    ${PROJECT_ROOT_DIR}/thirdparty/googletest/googletest/include
    ${MODULE_INCLUDE}
)

target_compile_definitions(${MODULE} PUBLIC
    ${MODULE_DEF}
    PROJECT_ROOT_DIR="${PROJECT_ROOT_DIR}"
    ${MODULE}_QML_IMPORT="${MODULE_QML_IMPORT}"
)

if (NOT ${MODULE} MATCHES global)
    if (NOT MODULE_NOT_LINK_GLOBAL)
        set(MODULE_LINK global ${MODULE_LINK})
    endif()
endif()

set(MODULE_LINK ${QT_LIBRARIES} ${MODULE_LINK})
set(MODULE_LINK ${CMAKE_DL_LIBS} ${MODULE_LINK})

target_link_libraries(${MODULE} PRIVATE ${MODULE_LINK} )
