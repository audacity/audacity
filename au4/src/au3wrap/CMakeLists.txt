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
set(MODULE au3wrap)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/au3wrapmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3wrapmodule.h

    ${CMAKE_CURRENT_LIST_DIR}/audacity3project.cpp
    ${CMAKE_CURRENT_LIST_DIR}/audacity3project.h
    )

set(MODULE_INCLUDE
    ${AUDACITY_ROOT}
    ${AUDACITY_ROOT}/libraries/lib-registries
    ${AUDACITY_ROOT}/libraries/lib-exceptions
    ${AUDACITY_ROOT}/libraries/lib-utility
)

# ==================================
# === AU3 ===
# ==================================
set(AU3_DEF
    -DUTILITY_API=
    -DPROJECT_API=
    -DSTRINGS_API=
    -DEXCEPTIONS_API=
    -DREGISTRIES_API=
    -DPREFERENCES_API=
    -DXML_API=
    -DBASIC_UI_API=
    -DCOMPONENTS_API=
    -DSTRING_UTILS_API=
    -DFILES_API=
)

set(AU3_INCLUDE
    ${wxwidgets_INCLUDE_DIRS}
    ${AUDACITY_ROOT}
    ${AUDACITY_ROOT}/libraries/lib-registries
    ${AUDACITY_ROOT}/libraries/lib-exceptions
    ${AUDACITY_ROOT}/libraries/lib-utility
    ${AUDACITY_ROOT}/libraries/lib-strings
    ${AUDACITY_ROOT}/libraries/lib-preferences
    ${AUDACITY_ROOT}/libraries/lib-xml
    ${AUDACITY_ROOT}/libraries/lib-components
    ${AUDACITY_ROOT}/libraries/lib-basic-ui
)

set(AUDACITY_GLOBAL_LIBTYPE STATIC)
macro(add_au3_lib name)
    add_subdirectory(${AUDACITY_ROOT}/libraries/lib-${name} au3-lib-${name})
    target_include_directories(lib-${name} PRIVATE ${AU3_INCLUDE})
    target_compile_definitions(lib-${name} PRIVATE ${AU3_DEF})

if (CC_IS_MSVC)
    # set_property(TARGET lib-${name} PROPERTY
    # MSVC_RUNTIME_LIBRARY “MultiThreaded$<$CONFIG:Debug:Debug>”)
    message("CMAKE_CXX_FLAGS_DEBUG: ${CMAKE_CXX_FLAGS_DEBUG}")
    set_target_properties(lib-${name} PROPERTIES COMPILE_FLAGS "${CMAKE_CXX_FLAGS_DEBUG}")
endif()
endmacro()

add_au3_lib(project)
# project dependincies
add_au3_lib(utility)
add_au3_lib(registries)
add_au3_lib(preferences)
add_au3_lib(basic-ui)
add_au3_lib(strings)
add_au3_lib(string-utils)
add_au3_lib(components)
add_au3_lib(exceptions)
add_au3_lib(xml)
add_au3_lib(files)
# end project dependincies

set(MODULE_LINK
    wxwidgets::wxwidgets
    expat::expat
    lib-project-interface
)
# ==================================
set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})

set(MODULE_USE_PCH_NONE ON)
set(MODULE_USE_UNITY_NONE ON)
include(SetupModule)

