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

set(MODULE_TEST global_tests)

set(MODULE_TEST_SRC
    ${CMAKE_CURRENT_LIST_DIR}/mocks/applicationmock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/filesystemmock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/processmock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/globalconfigurationmock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/interactivemock.h
    ${CMAKE_CURRENT_LIST_DIR}/mocks/systeminfomock.h

    ${CMAKE_CURRENT_LIST_DIR}/uri_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/val_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/logremover_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/bytearray_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/buffer_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/file_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/iodevice_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/fileinfo_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/string_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/json_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/datetime_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/flags_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/allocator_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/mnemonicstring_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/containers_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/version_tests.cpp
    ${CMAKE_CURRENT_LIST_DIR}/number_tests.cpp
)

include(${PROJECT_SOURCE_DIR}/src/framework/testing/gtest.cmake)
