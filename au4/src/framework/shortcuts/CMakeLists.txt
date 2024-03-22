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

set(MODULE shortcuts)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/shortcutsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/shortcutsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/shortcutstypes.h
    ${CMAKE_CURRENT_LIST_DIR}/ishortcutsregister.h
    ${CMAKE_CURRENT_LIST_DIR}/ishortcutscontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/ishortcutsconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/imidiremote.h

    ${CMAKE_CURRENT_LIST_DIR}/api/shortcutsapi.cpp
    ${CMAKE_CURRENT_LIST_DIR}/api/shortcutsapi.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/shortcutsinstancemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/shortcutsinstancemodel.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/shortcutsregister.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/shortcutsregister.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/shortcutscontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/shortcutscontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/shortcutsconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/shortcutsconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/midiremote.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/midiremote.h

    ${CMAKE_CURRENT_LIST_DIR}/view/shortcutsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/shortcutsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/editshortcutmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/editshortcutmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/mididevicemappingmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/mididevicemappingmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/editmidimappingmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/editmidimappingmodel.h
    )

set(MODULE_QRC shortcuts.qrc)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml )

if (OS_IS_MAC)
    find_library(CARBON_LIBRARY Carbon)

    set(MODULE_SRC ${MODULE_SRC}
        ${CMAKE_CURRENT_LIST_DIR}/internal/platform/macos/macosshortcutsinstancemodel.mm
        ${CMAKE_CURRENT_LIST_DIR}/internal/platform/macos/macosshortcutsinstancemodel.h
    )

    set_source_files_properties(
        ${CMAKE_CURRENT_LIST_DIR}/internal/platform/macos/macosshortcutsinstancemodel.mm
        PROPERTIES
        SKIP_UNITY_BUILD_INCLUSION ON
        SKIP_PRECOMPILE_HEADERS ON
    )

    set(MODULE_INCLUDE
        ${CARBON_LIBRARY}
    )

    if (MUE_COMPILE_QT5_COMPAT)
        set(MODULE_INCLUDE ${MODULE_INCLUDE} ${Qt5Gui_PRIVATE_INCLUDE_DIRS} )
    else()
        set(MODULE_INCLUDE ${MODULE_INCLUDE} ${Qt6Gui_PRIVATE_INCLUDE_DIRS} )
    endif()
endif(OS_IS_MAC)

include(${PROJECT_SOURCE_DIR}/build/module.cmake)

