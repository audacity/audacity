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

set (DOCK_LIBS
    kddockwidgets
)

set (DOCKWINDOW_SRC
    ${CMAKE_CURRENT_LIST_DIR}/docksetup.cpp
    ${CMAKE_CURRENT_LIST_DIR}/docksetup.h
    ${CMAKE_CURRENT_LIST_DIR}/docktypes.h
    ${CMAKE_CURRENT_LIST_DIR}/dockwindow.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dockwindow.h
    ${CMAKE_CURRENT_LIST_DIR}/idockwindow.h
    ${CMAKE_CURRENT_LIST_DIR}/idockwindowprovider.h
    ${CMAKE_CURRENT_LIST_DIR}/dockpageview.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dockpageview.h
    ${CMAKE_CURRENT_LIST_DIR}/dockpanelview.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dockpanelview.h
    ${CMAKE_CURRENT_LIST_DIR}/dockstatusbarview.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dockstatusbarview.h
    ${CMAKE_CURRENT_LIST_DIR}/docktoolbarview.cpp
    ${CMAKE_CURRENT_LIST_DIR}/docktoolbarview.h
    ${CMAKE_CURRENT_LIST_DIR}/docktitlebar.cpp
    ${CMAKE_CURRENT_LIST_DIR}/docktitlebar.h
    ${CMAKE_CURRENT_LIST_DIR}/dockingholderview.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dockingholderview.h
    ${CMAKE_CURRENT_LIST_DIR}/dockcentralview.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dockcentralview.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/dockbase.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/dockbase.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/dropcontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/dropcontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/dockseparator.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/dockseparator.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/dockframemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/dockframemodel.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/docktabbar.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/docktabbar.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/dockwindowactionscontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/dockwindowactionscontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/dockwindowprovider.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/dockwindowprovider.h
    ${DOCKWINDOW_PLATFORM_SRC}
)

