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

set(MODULE_TEST ui_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/environment.cpp
    ${CMAKE_CURRENT_LIST_DIR}/mocks/navigationmocks.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/mainwindowmock.h

    ${CMAKE_CURRENT_LIST_DIR}/navigationcontroller_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/qmltooltip_tests.cpp
    )

set(MODULE_TEST_LINK
    actions
    ui
    )

include(${PROJECT_SOURCE_DIR}/src/framework/testing/gtest.cmake)
