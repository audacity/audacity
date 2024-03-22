# SPDX-License-Identifier: GPL-3.0-only
# MuseScore-CLA-applies
#
# MuseScore
# Music Composition & Notation
#
# Copyright (C) 2023 MuseScore BVBA and others
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

# Compiler definition
# MuseScore-specific, defines typical configurations

if (COMPILER_INFO_DETECTED)
    return()
endif()

include(GetPlatformInfo)

if (OS_IS_WASM)

    set(CC_IS_EMSCRIPTEN 1)

elseif (CMAKE_CXX_COMPILER_ID STREQUAL "Clang" OR CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang")

    set(CC_IS_CLANG 1)

elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")

    if (OS_IS_WIN)

        set(CC_IS_MINGW 1)

    else(OS_IS_WIN)

        set(CC_IS_GCC 1)

    endif(OS_IS_WIN)

elseif (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")

  set(CC_IS_MSVC 1)

else()
    message(FATAL_ERROR "Unsupported compiller: ${CMAKE_CXX_COMPILER_ID}")
endif()

# Define MINGW for VS, as it appears not to be defined
if (MSVC)
   set (MINGW false)
endif (MSVC)

set(COMPILER_INFO_DETECTED 1)
