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
declare_module(au3wrap)

set(MODULE_SRC
    ${CMAKE_CURRENT_LIST_DIR}/au3wrapmodule.cpp
    ${CMAKE_CURRENT_LIST_DIR}/au3wrapmodule.h
    ${CMAKE_CURRENT_LIST_DIR}/au3types.h
    ${CMAKE_CURRENT_LIST_DIR}/iau3project.h
    ${CMAKE_CURRENT_LIST_DIR}/au3projectcontext.h

    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audiometer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audiometer.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audiometerfactory.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3audiometerfactory.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/wxtypes_convert.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/wxlogwrap.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/wxlogwrap.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/domconverter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/domconverter.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/progressdialog.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/progressdialog.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/domaccessor.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/domaccessor.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3project.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3project.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3projectcontext.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3commonsettings.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3commonsettings.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3basicui.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/au3basicui.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsnap.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectsnap.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectthumbnail.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/projectthumbnail.h
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackcolor.cpp
    ${CMAKE_CURRENT_LIST_DIR}/internal/trackcolor.h
    )

# ==================================
# === AU3 ===
# ==================================

# Include AU3 wrap definitions
include(${CMAKE_CURRENT_LIST_DIR}/au3wrapDefs.cmake)

# Third-party libraries (not part of au3/libraries)
add_subdirectory(${CMAKE_SOURCE_DIR}/thirdparty/sqlite au3-sqlite)
add_subdirectory(${CMAKE_SOURCE_DIR}/thirdparty/soxr au3-soxr)
add_subdirectory(${CMAKE_SOURCE_DIR}/thirdparty/pffft au3-pffft)
add_subdirectory(${CMAKE_SOURCE_DIR}/thirdparty/portmixer au3-portmixer)
add_subdirectory(${CMAKE_SOURCE_DIR}/thirdparty/twolame au3-twolame)
tft_add_to_build()

# Conditional time-stretching and pitch-shifting libraries
if (AU_USE_SOUNDTOUCH)
    add_subdirectory(${CMAKE_SOURCE_DIR}/thirdparty/soundtouch au3-soundtouch)
endif()

if (AU_USE_SBSMS)
    add_subdirectory(${CMAKE_SOURCE_DIR}/thirdparty/sbsms au3-sbsms)
endif()

# Conditional third-party SDKs (must be set up before AU3 libraries)
if (AU_MODULE_EFFECTS_LV2)
    add_subdirectory(${CMAKE_SOURCE_DIR}/thirdparty/lv2sdk lv2sdk)
endif()

if(AU_MODULE_EFFECTS_VST)
    add_subdirectory(${CMAKE_SOURCE_DIR}/thirdparty/vst3 vst3)
endif()

# RapidJSON (header-only, used by au3-musehub, au3-cloud-audiocom, au3-import-export, au3-sentry-reporting)
rapidjson_add_to_build()

# Include AU3 libraries - this will add all library subdirectories
# and set AU3_LIBRARIES_LIST in this scope
add_subdirectory(${AU3_LIBRARIES} au3-libraries)

# AU3 import/export format plugins (per-format static libraries)
add_subdirectory(${AU3_MODULES}/import-export au3-import-export-modules)

# ==================================
# AU3_LIBRARIES_LIST is now set by au3/libraries/CMakeLists.txt
# ==================================
set(MODULE_INCLUDE ${AU3_INCLUDE})
set(MODULE_DEF ${AU3_DEF})
set(MODULE_LINK
        ${AU3_LINK}
        project # required for projectconfiguration and projecterrors
        audio # required for audiometer
        sqlite # static Factory init HACK for au3-project-file-io/SqliteSampleBlock.cpp included in au3project.cpp
        # TODO: fix Hack and remove sqlite dependency from au3wrap - tracked in
        # https://github.com/audacity/audacity/issues/11806
        au3-mod-ffmpeg # au3wrapmodule.cpp calls FFmpegStartup()
        # Bulk link of all au3 libraries: no longer propagates includes/defines
        # (consumers declare their own deps), but still guarantees every enabled
        # au3 archive reaches the final link closure. Retiring it is tracked in
        # https://github.com/audacity/audacity/issues/11806
        ${AU3_LIBRARIES_LIST}
)

set(MODULE_LINK_PUBLIC
        # au3 libraries exposed by au3wrap's own public headers - consumers get
        # these transitively and only declare the au3 libraries they include directly
        au3-audio-devices
        au3-basic-ui
        au3-label-track
        au3-project
        au3-registries
        au3-track
        au3-utility
        au3-wave-track

        # TODO: this isn't nice, we should avoid Wx dependencies in AU4 modules.
        # wxBase must stay PUBLIC for now: the au3 libraries link wxBase PRIVATE,
        # so this is the only path providing wx include dirs to the many AU4 files
        # that include au3 headers (which pull in wx/string.h etc.). Flipping it
        # requires removing wx from au3 public headers first - tracked in
        # https://github.com/audacity/audacity/issues/11804
        wxBase
)

set(MODULE_USE_PCH OFF)
set(MODULE_USE_UNITY OFF)

setup_module()

target_no_warning(${MODULE} -w)

if (CC_IS_CLANG)
    set_property(TARGET ${MODULE} APPEND_STRING PROPERTY COMPILE_FLAGS "-fobjc-arc")
endif()

if (AU_BUILD_PROJECT_TESTS)
    add_subdirectory(tests)
endif()
