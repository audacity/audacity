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
set(MODULE project)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/projectmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/projectmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/projecttypes.h
    ${CMAKE_CURRENT_LIST_DIR}/iaudacityproject.h
    ${CMAKE_CURRENT_LIST_DIR}/iprojectconfiguration.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/audacityproject.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacityproject.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectuiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectuiactions.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectactionscontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectactionscontroller.h
    )

include(SetupModule)

