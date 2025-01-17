# SPDX-License-Identifier: GPL-3.0-only
# MuseScore-CLA-applies
#
# MuseScore
# Music Composition & Notation
#
# Copyright (C) 2021 MuseScore BVBA and others
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

# This module stores global state
declare_module(project)

set(MODULE_QRC project.qrc)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

include(GetPlatformInfo)

if (OS_IS_MAC)
    set(PLATFORM_SRC
        ${CMAKE_CURRENT_LIST_DIR}/internal/platform/macos/macosrecentfilescontroller.mm
        ${CMAKE_CURRENT_LIST_DIR}/internal/platform/macos/macosrecentfilescontroller.h
        )
    # Don't mix C++ and Objective-C++ in Unity Build
    set_source_files_properties(
        ${CMAKE_CURRENT_LIST_DIR}/internal/platform/macos/macosrecentfilescontroller.mm
        PROPERTIES
        SKIP_UNITY_BUILD_INCLUSION ON
        SKIP_PRECOMPILE_HEADERS ON
        )
elseif(OS_IS_WIN)
    set(PLATFORM_SRC
        ${CMAKE_CURRENT_LIST_DIR}/internal/platform/windows/windowsrecentfilescontroller.cpp
        ${CMAKE_CURRENT_LIST_DIR}/internal/platform/windows/windowsrecentfilescontroller.h
    )
endif()

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/projectmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/projectmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/projecterrors.h
    ${CMAKE_CURRENT_LIST_DIR}/iaudacityproject.h
    ${CMAKE_CURRENT_LIST_DIR}/iprojectconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/iprojectcreator.h
    ${CMAKE_CURRENT_LIST_DIR}/iprojectfilescontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/irecentfilescontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/imscmetareader.h
    ${CMAKE_CURRENT_LIST_DIR}/iprojectautosaver.h
    ${CMAKE_CURRENT_LIST_DIR}/ithumbnailcreator.h

    ${CMAKE_CURRENT_LIST_DIR}/types/projecttypes.h
    ${CMAKE_CURRENT_LIST_DIR}/types/projectmeta.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/audacityproject.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacityproject.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectuiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectuiactions.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectautosaver.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectautosaver.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectactionscontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectactionscontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/recentfilescontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/recentfilescontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/iopensaveprojectscenario.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/opensaveprojectscenario.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/opensaveprojectscenario.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/thumbnailcreator.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/thumbnailcreator.h

    ${PLATFORM_SRC}

    ${CMAKE_CURRENT_LIST_DIR}/view/projectspagemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/projectspagemodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/abstractprojectsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/abstractprojectsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/recentprojectsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/recentprojectsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/cloudprojectsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/cloudprojectsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/cloudprojectstatuswatcher.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/cloudprojectstatuswatcher.h
    ${CMAKE_CURRENT_LIST_DIR}/view/projectthumbnailloader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/projectthumbnailloader.h
    ${CMAKE_CURRENT_LIST_DIR}/view/pixmapprojectthumbnailview.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/pixmapprojectthumbnailview.h
    ${CMAKE_CURRENT_LIST_DIR}/view/newprojectmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/newprojectmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/projectpropertiesmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/projectpropertiesmodel.h
)

set(MODULE_LINK au3wrap)

set(MODULE_USE_UNITY OFF)

setup_module()

if (AU_BUILD_PROJECT_TESTS)
    add_subdirectory(tests)
endif()
