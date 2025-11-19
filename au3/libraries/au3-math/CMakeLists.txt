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

# AU3 Math Library
# A library of mathematical utilities and manipulation of samples

set(SOURCES
    Biquad.cpp
    EBUR128.cpp
    SampleFormat.cpp
    SampleCount.cpp
    Resample.cpp
    Dither.cpp
    InterpolateAudio.cpp
    Matrix.cpp
    PitchName.cpp
)

set(HEADERS
    Biquad.h
    EBUR128.h
    SampleFormat.h
    SampleCount.h
    Resample.h
    Dither.h
    InterpolateAudio.h
    Matrix.h
    LinearFit.h
    PitchName.h
    Reverb_libSoX.h
    RoundUpUnsafe.h
    float_cast.h
    Gain.h
)

add_library(au3-math STATIC ${SOURCES} ${HEADERS})

target_include_directories(au3-math
    PUBLIC
        # Allow includes like: #include "au3-math/SampleFormat.h"
        ${CMAKE_CURRENT_SOURCE_DIR}/..
        # For internal includes within au3-math (e.g., "SampleFormat.h")
        ${CMAKE_CURRENT_SOURCE_DIR}
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/../lib-preferences
        ${CMAKE_CURRENT_SOURCE_DIR}/../lib-strings
        ${CMAKE_CURRENT_SOURCE_DIR}/../lib-utility
        ${CMAKE_CURRENT_SOURCE_DIR}/../lib-components
        # For soxr.h
        ${CMAKE_CURRENT_SOURCE_DIR}/../../lib-src/libsoxr/src
)

target_compile_definitions(au3-math
    PUBLIC
        -DMATH_API=
        # wxWidgets base restrictions (no GUI)
        -DwxUSE_GUI=0
        -D_WX_APP_H_BASE_
        -D_WX_EVTLOOP_H_
        -D_WX_IMAGE_H
        -D_WX_COLOUR_H_BASE_
        -D_WX_BRUSH_H_BASE_
        -D_WX_PEN_H_BASE_
    PRIVATE
        # Required for AU3 library dependencies
        -DUTILITY_API=
        -DSTRINGS_API=
        -DCOMPONENTS_API=
        -DPREFERENCES_API=
)

# Platform-specific wxWidgets definitions
include(GetPlatformInfo)
if (OS_IS_LIN)
    target_compile_definitions(au3-math PUBLIC -D__WXGTK__)
elseif(OS_IS_MAC)
    target_compile_definitions(au3-math PUBLIC -D__WXMAC__)
elseif(OS_IS_WIN)
    target_compile_definitions(au3-math PUBLIC -D__WXMSW__ -DWXUSINGDLL)
endif()

target_link_libraries(au3-math
    PUBLIC
        wxwidgets::wxwidgets
        soxr
    PRIVATE
        expat::expat
)

# Disable warnings for AU3 code
target_no_warning(au3-math -w)
