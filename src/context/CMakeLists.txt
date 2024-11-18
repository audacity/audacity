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
declare_module(context)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/contextmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/contextmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/iglobalcontext.h
    ${CMAKE_CURRENT_LIST_DIR}/iplaybackstate.h
    ${CMAKE_CURRENT_LIST_DIR}/uicontext.h
    ${CMAKE_CURRENT_LIST_DIR}/shortcutcontext.h
    ${CMAKE_CURRENT_LIST_DIR}/iuicontextresolver.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/globalcontext.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/globalcontext.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/uicontextresolver.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/uicontextresolver.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/playbackstate.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/playbackstate.h
    )

setup_module()

if (AU_BUILD_CONTEXT_TESTS)
    add_subdirectory(tests)
endif()
