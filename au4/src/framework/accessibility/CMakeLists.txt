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

set(MODULE accessibility)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/accessibilitymodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/accessibilitymodule.h
    ${CMAKE_CURRENT_LIST_DIR}/iaccessible.h
    ${CMAKE_CURRENT_LIST_DIR}/iaccessibilitycontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/iaccessibilityconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/iqaccessibleinterfaceregister.h

    ${CMAKE_CURRENT_LIST_DIR}/api/accessibilityapi.cpp
    ${CMAKE_CURRENT_LIST_DIR}/api/accessibilityapi.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/accessibilitycontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/accessibilitycontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/accessibleobject.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/accessibleobject.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/accessiblestub.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/accessiblestub.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/accessibleiteminterface.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/accessibleiteminterface.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/accessibilityconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/accessibilityconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/qaccessibleinterfaceregister.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/qaccessibleinterfaceregister.h
    )

if (OS_IS_LIN)
    # it needed to send a spontaneous event by activating navigation
    if (MUE_COMPILE_QT5_COMPAT)
        set(MODULE_INCLUDE ${Qt5Core_PRIVATE_INCLUDE_DIRS} )
    else()
        set(MODULE_INCLUDE ${Qt6Core_PRIVATE_INCLUDE_DIRS} )
    endif()
endif()


if (MUE_ENABLE_ACCESSIBILITY_TRACE)
    set(MODULE_DEF ${MODULE_DEF} -DMUE_ENABLE_ACCESSIBILITY_TRACE )
endif()

include(SetupModule)

if (MUE_BUILD_ACCESSIBILITY_TESTS)
    add_subdirectory(tests)
endif()
