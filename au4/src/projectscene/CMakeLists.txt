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
    ${CMAKE_CURRENT_LIST_DIR}/iprojectsceneconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/iprojectviewstate.h
    ${CMAKE_CURRENT_LIST_DIR}/iprojectviewstatecreator.h
    ${CMAKE_CURRENT_LIST_DIR}/iprojectsceneactionscontroller.h

    ${CMAKE_CURRENT_LIST_DIR}/types/projectscenetypes.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsceneconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsceneconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectviewstate.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectviewstate.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectviewstatecreator.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectviewstatecreator.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsceneuiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsceneuiactions.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsceneactionscontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsceneactionscontroller.h

    ${CMAKE_CURRENT_LIST_DIR}/view/common/tracksviewstatemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/common/tracksviewstatemodel.h

    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/trackslistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/trackslistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/trackitem.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/trackspanel/trackitem.h

    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/projecttoolbarmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/toolbars/projecttoolbarmodel.h

    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/clipslistmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/clipslistmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/clipcontextmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/clipcontextmenumodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/trackslistclipsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/trackslistclipsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/waveview.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/waveview.h
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/selectioncontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/clipsview/selectioncontroller.h

    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/timelinecontext.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/timelinecontext.h
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/timelineruler.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/timelineruler.h
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/timeformat.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/timeformat.h
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/timelinecontextmenumodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/timeline/timelinecontextmenumodel.h

    ${CMAKE_CURRENT_LIST_DIR}/view/playcursor/playcursorcontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/playcursor/playcursorcontroller.h
    )

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml )

set(MODULE_LINK processing)

set(MODULE_INCLUDE
    ${CLIPSVIEW_V1_INC}
)

set(MODULE_DEF
    ${CLIPSVIEW_V1_DEF}
)

setup_module()
