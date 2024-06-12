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

declare_module(processing)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/processingmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/processingmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/processingtypes.h
    ${CMAKE_CURRENT_LIST_DIR}/iprocessinginteraction.h
    ${CMAKE_CURRENT_LIST_DIR}/iprocessingactionscontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/iselectioncontroller.h

    ${CMAKE_CURRENT_LIST_DIR}/dom/processingproject.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dom/processingproject.h
    ${CMAKE_CURRENT_LIST_DIR}/dom/track.h
    ${CMAKE_CURRENT_LIST_DIR}/dom/clip.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dom/clip.h
    ${CMAKE_CURRENT_LIST_DIR}/dom/wave.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dom/wave.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/processinguiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/processinguiactions.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/processingactionscontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/processingactionscontroller.h
    )

setup_module()



