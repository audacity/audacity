# SPDX-License-Identifier: GPL-3.0-only
# MuseScore-Studio-CLA-applies
#
# MuseScore Studio
# Music Composition & Notation
#
# Copyright (C) 2024 MuseScore Limited
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

if (CMAKE_C_COMPILER_LAUNCHER OR CMAKE_CXX_COMPILER_LAUNCHER)
    message(WARNING "CMAKE_C_COMPILER_LAUNCHER or CMAKE_CXX_COMPILER_LAUNCHER have already been set; not setting up compiler cache in order not to override them.")
    return()
endif()

find_program(COMPILER_CACHE_PROGRAM NAMES ccache sccache buildcache)
if (NOT COMPILER_CACHE_PROGRAM)
    message(STATUS "No compiler cache program found")
    return()
endif()

if (CMAKE_GENERATOR MATCHES "Make" OR CMAKE_GENERATOR MATCHES "Ninja")
    set(CMAKE_C_COMPILER_LAUNCHER   "${COMPILER_CACHE_PROGRAM}")
    set(CMAKE_CXX_COMPILER_LAUNCHER "${COMPILER_CACHE_PROGRAM}")

    set(ENV{CCACHE_CPP2} true)
    set(ENV{CCACHE_SLOPPINESS} "pch_defines,time_macros")

    message(STATUS "Using compiler cache program ${COMPILER_CACHE_PROGRAM} via CMAKE_C_COMPILER_LAUNCHER and CMAKE_CXX_COMPILER_LAUNCHER")
    return()
endif()

if (CMAKE_GENERATOR STREQUAL "Xcode")
    set(C_LAUNCHER   "${COMPILER_CACHE_PROGRAM}")
    set(CXX_LAUNCHER "${COMPILER_CACHE_PROGRAM}")
    configure_file(${PROJECT_SOURCE_DIR}/buildscripts/tools/launch-c.in   launch-c)
    configure_file(${PROJECT_SOURCE_DIR}/buildscripts/tools/launch-cxx.in launch-cxx)
    execute_process(COMMAND chmod a+rx
                    "${CMAKE_BINARY_DIR}/launch-c"
                    "${CMAKE_BINARY_DIR}/launch-cxx"
    )

    set(CMAKE_XCODE_ATTRIBUTE_CC         "${CMAKE_BINARY_DIR}/launch-c")
    set(CMAKE_XCODE_ATTRIBUTE_CXX        "${CMAKE_BINARY_DIR}/launch-cxx")
    set(CMAKE_XCODE_ATTRIBUTE_LD         "${CMAKE_BINARY_DIR}/launch-c")
    set(CMAKE_XCODE_ATTRIBUTE_LDPLUSPLUS "${CMAKE_BINARY_DIR}/launch-cxx")

    set(CMAKE_XCODE_ATTRIBUTE_CLANG_ENABLE_MODULES "NO")
    set(CMAKE_XCODE_ATTRIBUTE_COMPILER_INDEX_STORE_ENABLE "NO")

    message(STATUS "Using compiler cache program ${COMPILER_CACHE_PROGRAM} with Xcode via wrapper scripts")
    return()
endif()
