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
declare_module(project)

set(MODULE_QRC project.qrc)

set(MODULE_QML_IMPORT ${CMAKE_CURRENT_LIST_DIR}/qml)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/projectmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/projectmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/projecttypes.h
    ${CMAKE_CURRENT_LIST_DIR}/iaudacityproject.h
    ${CMAKE_CURRENT_LIST_DIR}/iprojectconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/iprojectcreator.h
    ${CMAKE_CURRENT_LIST_DIR}/iprojectfilescontroller.h
    ${CMAKE_CURRENT_LIST_DIR}/irecentfilescontroller.h

    ${CMAKE_CURRENT_LIST_DIR}/types/projecttypes.h
    ${CMAKE_CURRENT_LIST_DIR}/types/projectmeta.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/audacityproject.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/audacityproject.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectconfiguration.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectuiactions.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectuiactions.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectactionscontroller.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectactionscontroller.h

    ${CMAKE_CURRENT_LIST_DIR}/view/projectspagemodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/projectspagemodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/abstractprojectsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/abstractprojectsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/recentprojectsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/recentprojectsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/cloudprojectsmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/cloudprojectsmodel.h
    ${CMAKE_CURRENT_LIST_DIR}/view/cloudprojectstatuswatcher.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/cloudprojectstatuswatcher.h
    ${CMAKE_CURRENT_LIST_DIR}/view/projectthumbnailloader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/projectthumbnailloader.h
    ${CMAKE_CURRENT_LIST_DIR}/view/pixmapprojectthumbnailview.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/pixmapprojectthumbnailview.h
    ${CMAKE_CURRENT_LIST_DIR}/view/newprojectmodel.cpp
    ${CMAKE_CURRENT_LIST_DIR}/view/newprojectmodel.h
)

set(MODULE_LINK au3wrap)

setup_module()

