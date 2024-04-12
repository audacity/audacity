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
declare_module(projectscene)

set(MODULE_QRC projectscene.qrc)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/projectscenemodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/projectscenemodule.h
    ${CMAKE_CURRENT_LIST_DIR}/iplaybackcontroller.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsceneactioncontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsceneactioncontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/playbackcontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/playbackcontroller.h

    ${CMAKE_CURRENT_LIST_DIR}/view/projectsceneuiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/projectsceneuiactions.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarcustomisemodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarcustomisemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarcustomiseitem.h
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/playbacktoolbarcustomiseitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/projecttoolbarmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/projecttoolbarmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/trackslistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/trackslistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/trackitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/trackitem.h

    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/waveview.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/waveview.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/wavesource.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/wavesource.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/clipsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/clipsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/trackclipsitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/trackclipsitem.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/clipitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/clipitem.h
    )

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml )

set(MODULE_LINK processing)

setup_module()


