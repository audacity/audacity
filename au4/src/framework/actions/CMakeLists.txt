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

set(MODULE actions)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/actionsmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/actionsmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/iactionsdispatcher.h
    ${CMAKE_CURRENT_LIST_DIR}/actionable.h
    ${CMAKE_CURRENT_LIST_DIR}/actiontypes.h

    ${CMAKE_CURRENT_LIST_DIR}/api/dispatcherapi.cpp
    ${CMAKE_CURRENT_LIST_DIR}/api/dispatcherapi.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/actionsdispatcher.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/actionsdispatcher.h
    )

include(SetupModule)
